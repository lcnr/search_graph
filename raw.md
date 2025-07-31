# search graph fun

On cycle, rerun from the bottom-up instead of the top down. This should fix rayon and https://github.com/rust-lang/trait-system-refactor-initiative/issues/210.

It should be fairly easy to test its correctness as we can compare the result of the current behavior with the new one via fuzzing.

For this to work, each cycle head needs to know the following:
- all leaf nodes which depend on its provisional result
    - the stack from the head to the leaf at the point where we've evaluated the leaf
        - if we didn't need to fully rerun a cycle head between these two goals, we should use its final provisional result
            - what if one of these heads overflowed?
        - the final result of each of the stack entries

We then do the following instead of rerunning on canonical cycles:
- for a `leaf` which was evaluated with a given `stack`
    - rerun `leaf` given the original `stack` but with the new provisional result
    - if its result is still the same, go to next leaf
    - otherwise, pop `leaf` from `stack`, and treat its parent as leaf

We then either terminate after having checked all `leaf`s and none of them have changed, or after checking some leaf ended up popping all goals between it and `head`, in which case we're also done.

## Issue: provisional cache and fuzzing

For this to not be observable, we'd need to remember the current provisional cache at the start of evaluating every single nested goal. This seems prohibitively expensive, so we cannot handle provisional cache entries in a non-observable way.

We instead start out with a filtered provisional cache when evaluating the leaf.

How can we still test this:
- make sure that the "provisional cache test trees" have the same result
- make sure that with the provisional cache disabled, this doesn't impact behavior in the "global cache graphs"

OKI

## Concrete impl design

1. don't track anything until there's a cycle
2. once we hit a cycle: store the entire stack from the head to the leaf in the head
    - if we encounter another cycle with the same head, we ideally don't store the entire stack again, but instead store it as a tree... seems hard. let's not do that at the start
    - we can store it at `offset` from the previous stack. also means that if we ended up having to evaluate to depth `n` before it no longer changed, we won't try the next cycle head separately at all :3
    - issue: evaluate goal has different result, results in changed provisional cache entry. parent does not change, but the parents parent may yet again depend on this provisional cache entry
        - either: also treat goals which depend on provisional cache entries with a given head as depending on the head
        - or: track which goals a provisional cache entry depends on
            - requires going from a normal tree to a acyclic graph
            - in `CycleHeads`, store `BottomUpEntryIndex` of the current goal for each head
            - then walk the current path and instead of adding it as a leaf, add a provisional cache entry edge
3. instead of doing a loop if we encounter a changed provisional result, we call a separate `fn recompute_with_changed_provisional_result`

### `fn recompute_with_changed_privisional_result`

1. loopy loop
2. put the stored stack on the stack :3
3. `evaluate_canonical_goal` for the leaf 
    - if its result equals the one from the previous iteration, pop stack until we're at the `offset` of the next leaf
        - popping the stack uses the existing behavior of cleaning up the provisional cache
        - if we're < the `offset` of the next leaf, skip it
    - if the result differs, pop the parent, but without clearing depent provisional result (after all, we'll readd it with the same one right away), and then use it as the new leaf

## impl notes

storing stack entries is kinda scuffed. we don't want to store them eagerly but instead only store them once we hit a cycle. They have changed by that point from their initial state when evaluating them.

```rust
struct StackEntry<X: Cx> {
    // FROZEN
    input: X::Input,
    step_kind_from_parent: PathKind,
    available_depth: AvailableDepth,
    provisional_result: Option<X::Result>,

    /// MUTABLE: have an initial value, adding things is commutative and associative
    reached_depth: StackDepth,
    heads: CycleHeads,
    encountered_overflow: bool,
    has_been_used: Option<UsageKind>,
    nested_goals: NestedGoals<X>,
}
```

## search graph, reimagined

we can construct a complete dependency graph once we're in a cycle
- acyclic, heads are a different kind of node
- needs O(1) access input -> nodes
- needs forwards and back endges

when accessing a provisional cache entry we
- walk through the stack, if a goal is in the dependency graph, recompute its dependents
    - mutate provisional result to ignore subgraph
- look at cycle heads of the provisional cache entry
    - if they are on the stack, eq provisional results
    - recompute dependents

Does not seem like this can work

## handling `BottomUpStorageIndex` for provisional entries

The issue: evaluate goal has different result, results in changed provisional cache entry. parent does not change, but the parents parent may yet again depend on this provisional cache entry.

- change `ButtomUpInfo` to an acyclic graph
- accessing a provisional cache entry adds another dependency of its node in the graph
    - provisional cache entries need to store their index for each of their cycle heads
- checking the graph needs to recompute not only parent goals, but also goals which later use the provisional cache entry
    - ASS

## another perspective

The issue with rerunning cycles is that all provisional cache entries get cleared when doing so. If they tracked the graph of goals they depended on, we'd have to only discard cache entries whose value is actually impacted by the change in provisional result

## vibes

general incremental compilation only makes sense if checking the dependency graph is significantly faster than recomputing all affected nodes.

we know which leaf nodes changed their value, so we only need to track the graph and then checking red nodes can go bottom up instead of top down

we could only do this for provisional cache entries as reevaluating the head would immediately encounter a provisional cache entry for its direct children

what would we need to track for that
- for each provisional cache entry, remember the stack at each place we use a cycle head, mapping from cycle head to leafs which use this cycle head 
- when accessing a provisional cache entry
    - easy-ish: recursively require that one to remain valid
    - harder: check its dependence on the changed cycle head?
        - clone the used provisional cache entries and remember its graph? seems ass, rebasing makes things weird, never matters when rebasing :thinking_face:
    - hmm: reevaluate that goal with the stack when accessing it?
        - needs to access provisional cache entries
        - provisional cache entries get added in order, we could recursively first try to handle the nested one and then this one
            - how?
    - remove all provisional cache entries which depend on other provisional cache entries, check the ones which don't depend on any, then the ones which dependet on that one!

## OKAY!
- once we encounter a cycle
    - each goal stores all cycle heads and provisional cache entries it depends on
        - and the stack at the point of doing so
- provisional cache entries:
    - also stores all cycle heads and the provisional cache entries it depends on
- if we fail to reach a fixpoint
    - completely remove and store provisional results for which this goal was the highest cycle head
    - look at the "bottom up info" for this goal, and take all cyclic uses of this cycle head and all provisional cache entries the head depends on in order of occurance
        - if its a cyclic use, reprove it or its parents until the result no longer changes
        - if its a provisional cache entry, recursively expand that cache entry, this is acyclic, so we're gonna reach a fixpoint
            - if reevaluated, automatically readd to provisional cache
            - if hasn't changed, manually readd to cache with updated graph with the new execution
            - after doing so, reprove it or its parents until the result no longer changes
                
### how to impl?

- observation: we need a single shared tree for the whole evaluation
    - each goal only stores its node index
    - rerunning a goal adds a new node to the tree with an updated `provisional_result`
    - we can always build the entire graph, it shouldn't be too expensive, it's a linear cost per node. to avoid infecting the global cache: don't support `X::Input` -> Id lookup. Store Id in stack entry + provisional cache entry
- impact provisional caching, more general structure of partially rerunning goals
    - instead of removing goals from the provisional cache, we invalidate them but remember their node_id, then recompute them lazily on access
    - instead of rerunning goals, lazily recompute their tree
- what to do when having to rerun a goal which already tried to rerun
    - could have a node kind `Rerun` which
        - of another `node_id`
        - has a diff map `NodeId -> NodeId`
        - has a separate `provisional_result` access range
        - reevaluating it lazy starts with the access list of the node its rerunning and ignore provisional results if the path from that node to this provisional result access goal through a replaced node.
- rebasing a provisional cache entry doesn't mutate its node
- ISSUE: rerunning a provisional cache entry may need a lower stack, fix by support skipping part of the stack :thinking_face: the stack is now a linked list


Requirements for the data structure:
- need back edges to walk from "accessed provisional results/cycle heads" back to the root
- need to quickly get the list of accessed provisional results/cycle heads for a given goal. separate list of `provisional_result_accesses` and each node has a `(ProvResultAccessIndex, Option<ProvResultAccessIndex>)` pair.
    - only used to lazily rerun goals

### when do we access nodes

- when finishing a child node we need to point to its parent, need to eagerly create nodes in `evaluate_goal`. get the parent node by looking at the last `StackEntry`. this is correct even when rerunning. the stack entries on rerun keep their old `NodeId`
-  when the provisional result changed, we iterate through `head.cycles()`
    - if its a `CycleOnStack { id }` with `head.node_id == id`, rerun its parent goal. Building the stack by iterating over `Node.parent` until we get to an `InProgress` node which is on the stack
    - if its a `ProvisionalCacheHit { id }` we look try to fetch it from the provional cache. In case it is invalided (can debug_assert that `id`s match), recursively iterate through `head.cycles()` at the node with `id` which should be either `Regular | Rerun`.

### invariants

- `Rerun` is never a parent: the only parents care `InProgress` and `Regular`
    - nodes always point to the parent of the previous run (walking over parents is only correct if we know the current `diff`: only support walking over parents if there's a `diff`)
    - cycles and provisional cache hit can point to `Rerun`
- why do we care about:
    - `Regular::result`: to make sure reevaluating didn't change the result
    - `Regular::provisional_result`: need it as the provisional result when building the stack. Using their `result` is wrong as that result is only valid given the previous stack (which we have now changed) :thinking_face: 
    - `Regular::encountered_overflow`: doesn't impact evaluation, we could ignore it when checking whether things have changed, doesn't impact behavior, as long as we `|` the `encountered_overflow` of the provisional cache entries :thinking_face:
    - `Regular::heads`: doesn't impact evaluation, but used when checking the output as it may force us to rerun additional goals. It doesn't matter if we end up with fewer cycle heads.
- `StackDepth` needs to be stable and actually correspond to the index on the stack

## important concepts

- main stack vs temporary stacks
    - temporary stacks share part of the main stack but are well... temporary
    - created when lazily reevaluating provisional cache entries

### Impl plan

- reevaluating provisional cache entries
    - needs a different stack: easy
    - would need to
        - take all affected entries from provisional cache
        - reevaluate them and readd if necessary

## Thinking yet again

- evaluating a goal is a function `Stack -> ProvisionalCache -> AvailableDepth -> GoalInfo -> EvaluationResult`
    - storing and rebuilding changed `Stack` and `GoalInfo` is easy
    - the provisional cache does not matter for soundness
        - only necessary to avoid exponential blowup
        - we do weird shit with it (rebase)
- does throwing away provisional cache entries when invalidating mean we've still got hangs? potentially
- reevaluations take the tree of the initial evaluation, while
    - replacing/rerunning leaf nodes
    - changing the used `provisional_result`s
- we only ever access `cycles` when rerunning a goal itself. this happens exactly once
- we only ever access the rest of the nodes when rerunning
    -  if we've got `AB(A|B)` then rerunning `B` updates the `provisional_result`. rerunning `A` needs to check both the cycle with `B`s initial provisional result and with during the rerun
- we don't need `NodeKind::Rerun`. Can mutate `cycles` and need to diff `provisional_results` differently 

### how to change the provisional_result of parent_goals

- we only need to access the provisional_result for leafs, when encountering a cycle or using a provisional cache entry

## Fun provisional cache bug

```
B -> !C
C -> !B
```
- C YES
    - B NO
        - C cycle YES
- B YES
    - C NO using the entry provisional cache here is wrong 
        - B cycle YES

getting two would be
- `T::Foo eq u32` final result error
  - normalize `T::Foo` 
    - via impl -> u32
    - param_env shadowing
      - `T::Bar eq u32` final result ok
        - normalize `T::Bar`
          - via impl -> u32
          - param_env shadowing
            - `T::Foo eq u32` non-productive cycle -> error
- `T::Bar eq u32`
  - normalize `T::Bar` 
    - via impl -> u32
    - param_env shadowing
      - `T::Foo eq u32`
        - normalize `T::Foo`
          - via impl -> u32
          - param_env shadowing
            - `T::Bar eq u32`


## vibeck exponential blowup rerun

size 4

- A
    - A
    - B
        - A
        - B
        - C
            - A
            - B
            - C
            - D
                - A
                - B
                - C
                - D
        - D changed stack entries: [C], nested goals on stack: []
            - rerun C rm stack entries: [], nested goals on stack: [D]
                - rerun D cycle on stack -> did not change
    - C changed stack entries: [B], nested goals on stack: [C]
        - rerun B rm stack entries: [], nested goals on stack: [C]
            - rerun C cycle on stack -> did not change
    - D changed stack entries: [B], nested goals on stack: [] HOW?
        - rerun B rm stack entries: [], nested goals on stack: [D]
            - rerun D cycle on stack -> did not change

Being able to easily reuse the first lazy recompute of D when recomputing it the second time is very hard: issue, need to update nested goals
- track "number of occurances in nested goals"
- walk over the original of all recomputed subgraphs and remove it

Goals can change their complete evaluation when getting rerun. Only storing the provisional cache entry which was the lowest on the stack may end up redoing a lot of work if that entry had a very small subtree but a different cache entry has a big one. However, reevaluating the nested goals now on the stack and their parents should also be able to use a provisional cache entry.

- issue O(n²) for every unique goal in the current evaluation , we track
    - all its nested goals O(n)
    - and its evaluation tree (comparatively small, O(~1))
- issue: if you've got the same goal with an inductive and a coinductive step to the same nested goal, then only storing the last used subtree causes us to not have any useful caching and causes us to potentially entirely reevaluate the whole subtree
    - we could store all previous evaluations in the previous cache. how to decide which one to reeval?
- do not expect a perfect solution, the trait solver is a mess

## gamerino

Idea: a single provisional cache entry for every `X::Input`:
- after the first evaluation it start out as the full search tree of that evaluation
- we then evaluate it with a different stack/provisional result
    - for every affected node we split the tree there and reeval, resulting in a tree with branches
    - we then check take the branch of the current goal
- how does this interact with the global cache: also, we're likely not tracking `AvailableDepth`, so we have to be careful

What do we need
- for every goal we need to store its evaluation subtree and its branches
    - lookup from `X::Input` to nodes in that subtree
    - cyclic accesses between two nodes
    - which stack entries did it depend on
    - for its nested goals, a table from their results to the following path

How to impl this
```rust=
fn lazily_reevaluate_goal(input) {
    let prev_tree = self.get_tree(input)?;
    let mut pos = prev_tree.start();
    for node in start.walk_branches().flat_map(|b| path_to_branch_find(branch, stack)) {
        let curr_depth = stack.len();
        let mut curr = goal_of(node);
        let stack = stack_at_point(node);
        while stack.len() > curr_depth {
            let res = evaluate(curr);
            if let Some(known_path) = branch.match_on(res) {
                // update path to `known_path` and continue
                continue;
            } else {
                // Add an additional branch to `branch` here
                curr = stack.pop();
                stack.update_parent_goal(curr);
            }
        }
    }
    
}
```
- `lazily_reevaluate_goal` visits too many nodes. we add a branch each time a nested goal changed, even if this change does not impact the final result
- we don't want a decision tree based on the control flow of goals, but based on their dependencies
    - go through `used_stack_entries` of provisional result, if one is no longer on the stack
        - quick check whet
    - `nested_goals` can store the final result of evaluating that goal, doesn't matter if its on the stack rn
    - then have a decision tree based on whether othedr goal

- how would I check whether a goal is still applicable:
    - scan over its execution until I reach a stack access which is no longer on the stack/has a changed provisional result
        - reevaluate that node and compare the result, if different, reevaluate the parent
        - remember these two choices, if its not on the stack at a later date, scan the reevaluated execution
        - if reevaluation has same result as stack access, ignore the stack access
    - if on the way to that stack access there is a nested goal which is now on the stack
        - reevaluate that node and compare the result, if different, reevaluate the parent



Cannot do this in the global cache:
    - dep node tracking sucks https://rust-lang.zulipchat.com/#narrow/channel/131828-t-compiler/topic/query.20system.3A.20unnecessary.20constraints/with/525311762
    - i don't want to track `required_depth` for now, should certainly be possible
    
How to avoid messing up the global cache

## gamerino2: the cleanest of slates

To quickly lazily reeval we need to quickly iterate over all
- accesses of goals which were on the stack to compare the provisional result
- evaluations of nested goals which are now on the stack

We need a node for every "call nested goal":
- for every previous output of this goal, we point to the next node in the trace
- every goal has a list of "cycle on stack" which goes into nested goals

This does not seem to avoid the exponential slowdown

## gamerino3: actually be performant

walking literally all branches inside of a goal is too slow. The number of branches grows
exponentially. However it grows exponentially by 2^n instead of n^n. This feels unavoidable.
Evaluating goals where the order of cycles can impact their result fundamentally has such
complexity.

## gamerino4: different invariants

Can we change the invariants of the search graph to allow this instead. The core issue is that ABA and BAB differ. If we only allow ambiguity as the initial provisional result then this shouldn't be an issue :thinking_face:

Changing the fuzzer for this seems to agree.
- [ ] rebase_entry, actually think about cycle path requirements
- [ ] finish lazily reeval

### Rebase entry, path kinds

- `heads[E]`
- `p(H1, H2)` path between two heads

For provisional cache entry `E`
#### nested goal now on stack

If we've got an entry AB[C[B|A]|A] and then ACB, using the cache entry for B needs to make sure the following paths are still the same
- ABA = ACBA
- ABCA = ACBCA

Generally, we do this by making sure AB and ACB are match.

Future improvement: we could extend this to use cache entries with original path `P` support any current path `C` where `for N in Heads { PN == CN }` 

#### goal on stack has been popped

If a cycle head `H` with a path `pre(H)` to its parent cycle head has been popped from the stack we need to make sure
- `for h in heads[H] { pre(H)p(H,h) = Cp(E,H)p(H,h) }`


## lazy reeval cont

- [ ] correctness, need to actually track whether a goal has been reevaluated
- [ ] clear vs rebase depending on whether the provisional result is the final one

## rayon proof tree

- `A: IntoParallelIterator`
  - via impl
    - `A:Sized`
      - `AliasRelate(A, <D as IntoParallelIterator>::Iter)`
        - `NormalizesTo(<D as IntoParallelIterator>::Iter)`
          - via impl
            - `D: Sized`
              - `AliasRelate(D, <D as IntoParallelIterator>::Iter)`
                - `NormalizesTo(<D as IntoParallelIterator>::Iter)` inductive cycle -> `NoSolution`
              - `AliasRelate(D, <C as IntoParallelIterator>::Iter)`
                - `NormalizesTo(<C as IntoParallelIterator>::Iter)`
                  - via impl
                    - `C: Sized`
                      - `AliasRelate(C, <D as IntoParallelIterator>::Iter)`
                        - `NormalizesTo(<D as IntoParallelIterator>::Iter)` inductive cycle -> `NoSolution`
                      - `AliasRelate(C, <C as IntoParallelIterator>::Iter)`
                        - `NormalizesTo(<C as IntoParallelIterator>::Iter)` inductive cycle -> `NoSolution`
                      - `AliasRelate(C, <B as IntoParallelIterator>::Iter)`
                        - `NormalizesTo(<B as IntoParallelIterator>::Iter)`
                          - via impl
                            - `B: Sized`
                              - `AliasRelate(B, <D as IntoParallelIterator>::Iter)`
                                - `NormalizesTo(<D as IntoParallelIterator>::Iter)` inductive cycle -> `NoSolution`
                              - `AliasRelate(B, <C as IntoParallelIterator>::Iter)`
                                - `NormalizesTo(<C as IntoParallelIterator>::Iter)` inductive cycle -> `NoSolution`
                              - `AliasRelate(B, <B as IntoParallelIterator>::Iter)`
                                - `NormalizesTo(<B as IntoParallelIterator>::Iter)` inductive cycle -> `NoSolution`
                              - `AliasRelate(B, <A as IntoParallelIterator>::Iter)`
                                - `NormalizesTo(<A as IntoParallelIterator>::Iter)`
                                  - via impl
                                    - `A: Sized` inductive cycle -> `NoSolution`
                                  - via where-bound -> rigid
                            - `B: ParallelIterator`
                              - `AliasRelate(B, <D as IntoParallelIterator>::Iter)` provisional cache hit
                              - `AliasRelate(B, <C as IntoParallelIterator>::Iter)` provisional cache hit
                              - `AliasRelate(B, <B as IntoParallelIterator>::Iter)` provisional cache hit
                              - `AliasRelate(B, <A as IntoParallelIterator>::Iter)` provisional cache hit
                          - via where-bound -> rigid
                          - changed provisional result, reevaluate
                            - reevaluate cycle: `[B: Sized, AliasRelate(B, <B as IntoParallelIterator>::Iter), NormalizesTo(<B as IntoParallelIterator>::Iter)]`
                              - reevaluate `NormalizesTo(<B as IntoParallelIterator>::Iter)` cycle -> rigid
                              - reevaluate `AliasRelate(B, <B as IntoParallelIterator>::Iter)`
                                - `NormalizesTo(<B as IntoParallelIterator>::Iter)` cycle rigid
                            - reevaluate cycle: `[B: ParallelIterator, AliasRelate(B, <B as IntoParallelIterator>::Iter)]`
                              - reevaluate `AliasRelate(B, <B as IntoParallelIterator>::Iter)` provisional cache hit
                      - `AliasRelate(C, <A as IntoParallelIterator>::Iter)`
                        - `NormalizesTo(<A as IntoParallelIterator>::Iter)` provisional cache hit -> `NoSolution`
                    - `C: ParallelIterator`
                      - `AliasRelate(C, <D as IntoParallelIterator>::Iter)` provisional cache hit
                      - `AliasRelate(C, <C as IntoParallelIterator>::Iter)` provisional cache hit
                      - `AliasRelate(C, <B as IntoParallelIterator>::Iter)` provisional cache hit
                      - `AliasRelate(C, <A as IntoParallelIterator>::Iter)` provisional cache hit
                  - changed provisional result, reevaluate
                    - reevaluate cycle: `[C: Sized, AliasRelate(C, <C as IntoParallelIterator>::Iter), NormalizesTo(<C as IntoParallelIterator>::Iter)]`
                      - reevaluate `NormalizesTo(<C as IntoParallelIterator>::Iter)` cycle rigid
                      - reevaluate `AliasRelate(C, <C as IntoParallelIterator>::Iter)`
                        - `NormalizesTo(<C as IntoParallelIterator>::Iter)` cycle rigid
                    - reevaluate cycle: `[C: Sized, AliasRelate(C, <B as IntoParallelIterator>::Iter), B: Sized, AliasRelate(B, <C as IntoParallelIterator>::Iter), NormalizesTo(<C as IntoParallelIterator>::Iter)]`
                      - reevaluate `NormalizesTo(<C as IntoParallelIterator>::Iter)` cycle rigid
                      - reevaluate `AliasRelate(B, <C as IntoParallelIterator>::Iter)`
                        - `NormalizesTo(<C as IntoParallelIterator>::Iter)` cycle rigid
              - `AliasRelate(D, <B as IntoParallelIterator>::Iter)`
                - `NormalizesTo(<B as IntoParallelIterator>::Iter)` provisional cache hit
              - `AliasRelate(D, <A as IntoParallelIterator>::Iter)`
                - `NormalizesTo(<A as IntoParallelIterator>::Iter)` provisional cache hit
            - `D: ParallelIterator`
              - `AliasRelate(D, <D as IntoParallelIterator>::Iter)` provisional cache hit
              - `AliasRelate(D, <C as IntoParallelIterator>::Iter)` provisional cache hit
              - `AliasRelate(D, <B as IntoParallelIterator>::Iter)` provisional cache hit
              - `AliasRelate(D, <A as IntoParallelIterator>::Iter)` provisional cache hit
          - changed provisional result, reevaluate
            - reevaluate cycle: `[D: Sized, AliasRelate(D, <D as IntoParallelIterator>::Iter), NormalizesTo(<D as IntoParallelIterator>::Iter)]`
              - reevaluate `NormalizesTo(<D as IntoParallelIterator>::Iter)` cycle rigid
              - reevaluate `AliasRelate(D, <D as IntoParallelIterator>::Iter)`
                - `NormalizesTo(<D as IntoParallelIterator>::Iter)` cycle rigid
            - reevaluate cycle: ...

That's cool, seems fast enough!

- `TraitPredicate(D: IntoParallelIteratorIndir)`
  - via impl
    - `TraitPredicate(Box<D>: IntoParallelIterator)`
      - via impl
        - `TraitPredicate(Box<D>: ParallelIterator)`
          - `AliasRelate(Box<D>, <A as IntoParallelIteratorIndir>::Iter)`
            - `NormalizesTo(<A as IntoParallelIteratorIndir>::Iter)`
              - via impl
                - `TraitPredicate(Box<A>: IntoParallelIterator)`
                  - via impl
                    - `TraitPredicate(Box<A>: ParallelIterator)`
                      - `AliasRelate(Box<A>, <A as IntoParallelIteratorIndir>::Iter)`
                        - `NormalizesTo(<A as IntoParallelIteratorIndir>::Iter)` cycle overflow
                      - `AliasRelate(<A as IntoParallelIteratorIndir>::Iter, ?fresh)`
                        - `NormalizesTo(<A as IntoParallelIteratorIndir>::Iter)` cycle overflow
                      - `AliasRelate(Box<A>, <B as IntoParallelIteratorIndir>::Iter)`
                        - `NormalizesTo(<B as IntoParallelIteratorIndir>::Iter)`
                          - via impl
                            - `TraitPredicate(Box<B>: IntoParallelIterator)`
                              - via impl
                                - `TraitPredicate(Box<B>: ParallelIterator)`
                                  - `AliasRelate(Box<B>, <A as IntoParallelIteratorIndir>::Iter)`
                                    - `NormalizesTo(<A as IntoParallelIteratorIndir>::Iter)` cycle overflow
                                  - `AliasRelate(<A as IntoParallelIteratorIndir>::Iter, ?fresh)` provisional cache hit
                                  - `AliasRelate(Box<B>, <B as IntoParallelIteratorIndir>::Iter)`
                                    - `NormalizesTo(<B as IntoParallelIteratorIndir>::Iter)` cycle overflow
                                  - `AliasRelate(<B as IntoParallelIteratorIndir>::Iter, ?fresh)`
                                    - `NormalizesTo(<B as IntoParallelIteratorIndir>::Iter)` cycle overflow
                                  - `AliasRelate(Box<B>, <C as IntoParallelIteratorIndir>::Iter)`
                                    - `NormalizesTo(<C as IntoParallelIteratorIndir>::Iter)`
                                      - via impl
                                        - `TraitPredicate(Box<C>: IntoParallelIterator)`
                                          - via impl
                                            - `TraitPredicate(Box<C>: ParallelIterator)`
                                              - `AliasRelate(Box<C>, <A as IntoParallelIteratorIndir>::Iter)`
                                                - `NormalizesTo(<A as IntoParallelIteratorIndir>::Iter)` cycle overflow
                                              - `AliasRelate(<A as IntoParallelIteratorIndir>::Iter, ?fresh)` provisional cache hit
                                              - `AliasRelate(Box<C>, <B as IntoParallelIteratorIndir>::Iter)`
                                                - `NormalizesTo(<B as IntoParallelIteratorIndir>::Iter)` cycle overflow
                                              - `AliasRelate(<B as IntoParallelIteratorIndir>::Iter, ?fresh)` provisional cache hit
                                              - `AliasRelate(Box<C>, <C as IntoParallelIteratorIndir>::Iter)`
                                                - `NormalizesTo(<C as IntoParallelIteratorIndir>::Iter)` cycle overflow
                                              - `AliasRelate(<C as IntoParallelIteratorIndir>::Iter, ?fresh)`
                                                - `NormalizesTo(<C as IntoParallelIteratorIndir>::Iter)` cycle overflow
                                              - `AliasRelate(Box<C>, <D as IntoParallelIteratorIndir>::Iter)`
                                                - `NormalizesTo(<D as IntoParallelIteratorIndir>::Iter)`
                                                  - via impl
                                                    - `TraitPredicate(Box<D>: IntoParallelIterator)` cycle overflow
                                      - changed provisional result, reevaluate
                                        - reevaluate cycle: ?
                                          - reevaluate `NormalizesTo(<C as IntoParallelIteratorIndir>::Iter)` cycle rigid
                                          - reevaluate `AliasRelate(Box<C>, <C as IntoParallelIteratorIndir>::Iter)`
                                            - `NormalizesTo(<C as IntoParallelIteratorIndir>::Iter)` cycle rigid
                                          - reevaluate `TraitPredicate(Box<C>: ParallelIterator)`
                                            - `AliasRelate(Box<C>, <A as IntoParallelIteratorIndir>::Iter)` provisional cache hit
                                            - `AliasRelate(<A as IntoParallelIteratorIndir>::Iter, ?fresh)` provisional cache hit
                                            - `AliasRelate(Box<C>, <B as IntoParallelIteratorIndir>::Iter)` provisional cache hit
                                            - `AliasRelate(<B as IntoParallelIteratorIndir>::Iter, ?fresh)` provisional cache hit
                                            - `AliasRelate(Box<C>, <C as IntoParallelIteratorIndir>::Iter)` provisional cache hit
                                            - `AliasRelate(<D as IntoParallelIteratorIndir>::Iter, ?fresh)` provisional cache hit
                                  - `AliasRelate(Box<B>, <D as IntoParallelIteratorIndir>::Iter)`
                                    - `NormalizesTo(<D as IntoParallelIteratorIndir>::Iter)` provisional cache hit
                          - changed provisional result, reevaluate
                            - reevaluate cycle: ?
                              - reevaluate `NormalizesTo(<B as IntoParallelIteratorIndir>::Iter)` cycle rigid
                              - reevaluate `AliasRelate(Box<B>, <B as IntoParallelIteratorIndir>::Iter)`
                                - `NormalizesTo(<B as IntoParallelIteratorIndir>::Iter)` cycle rigid
                              - `TraitPredicate(Box<B>: ParallelIterator)` :skull:
                                - `AliasRelate(Box<B>, <A as IntoParallelIteratorIndir>::Iter)` reevaluate, no changes
                                - `AliasRelate(Box<B>, <B as IntoParallelIteratorIndir>::Iter)` provisional cache hit
                                - `AliasRelate(<B as IntoParallelIteratorIndir>::Iter, ?fresh)` reevaluate
                                  - `NormalizesTo(<B as IntoParallelIteratorIndir>::Iter)` cycle rigid
                                - `AliasRelate(Box<B>, <C as IntoParallelIteratorIndir>::Iter)` reevaluate
                                  - reevaluate cycle: ?
                                    - reevaluate `NormalizesTo(<B as IntoParallelIteratorIndir>::Iter)` cycle rigid
                                    - reevaluate `AliasRelate(Box<C>, <B as IntoParallelIteratorIndir>::Iter)`
                                    - reevaluate `TraitPredicate(Box<C>: ParallelIterator)`
                                      - `AliasRelate(Box<C>, <A as IntoParallelIteratorIndir>::Iter)` provisional cache hit
                                      - `AliasRelate(<A as IntoParallelIteratorIndir>::Iter, ?fresh)` provisional cache hit
                                      - `AliasRelate(Box<C>, <B as IntoParallelIteratorIndir>::Iter)` provisional cache hit
                                      - `AliasRelate(<B as IntoParallelIteratorIndir>::Iter, ?fresh)` provisional cache hit
                                      - `AliasRelate(Box<C>, <C as IntoParallelIteratorIndir>::Iter)` provisional cache hit
                                      - `AliasRelate(<C as IntoParallelIteratorIndir>::Iter, ?fresh)` provisional cache hit
                                      - `AliasRelate(Box<C>, <D as IntoParallelIteratorIndir>::Iter)` provisional cache hit
                                  

## reevaluate provisional cache hit

`AliasRelate(C, <B as IntoParallelIterator>::Iter)` did not change. While the computation of this provisional cache
entry relies on `AliasRelate(C, <B as IntoParallelIterator>::Iter)`, it only does so indirectly and we've already checked
that all nested goals don't change their result. We should get an immediate cache hit here.

We could explicitly check whether evaluating the cache entry depends on anything which changed during the reevaluation and
then skip trying to reevaluate it.

This is brittle if we've got the following proof tree.
- A
  - C
    - B does not change on rerun
      - A
  - D
    - C provisional cache hit
    - A
This would avoid C first, but then has to rerun its parent goal D at which point we now actually have to evaluate it. Ideally
this would still be cached.

As an alternative, instead of completely dropping provisional cache entries, we could move them to a `BTreeMap` and then whenever
we reevaluate a cycle, we move all provisional cache entries with a smaller `NodeId` than the current cycle back into the provisional
cache in case they have not changed.

## reevaluate main cycle trunk

Going through a trait goal means that the initial provisional result is ambiguity. This causes the parent alias-relate to also be ambiguous which makes the trait goal also be ambiguous. The normalizes-to goal then returns the normalized-to type together with this nested obligation at which point its parent alias-relate fails.

However, this means we need to reevaluate all trait goals which try to apply an alias `param_env` candidate. In the `IntoParallelIteratorIndir`
this causes us to reevaluate `TraitPredicate(Box<B>: ParallelIterator)` requires reevaluating pretty much the whole tree.

This is unnecessary. I've got two ideas how to avoid this.
- we can either avoid having to reevaluate large subtrees by adding the ability to only reevaluate a single candidate, or
  - this is insufficient. the result of the `param_env` candidate actually changes from ambiguity to `NoSolution`
- we can avoid recursing into most nested goals when reevaluating a large subtree

## lazily evaluating subtrees/skipping their evaluation

When rerunning a goal, we store its previous search tree in the `SearchGraph`. When evaluating a nested goal, we check
whether this nested goal has also been evaluated in a previous iteration with the same stack. If so, we already lazily
reevaluate it in the first iteration. Datastructure requirements:

- we need to be able to walk up the search tree, go from parent to its children
  - evaluating a child goal needs to quickly fetch its node given the search tree of its parent
  - this needs to know the provisional results at the point of evaluating a goal
    - we currently store this per cycle usage. No way to access it for arbitrary goals

Alternative approach: store a peekable iterator over `CycleId` in the search graph, when trying to compute a goal it should be
cached unless
- it has not been previously evaluated, need to just evaluate it anyways
- its result depends on the changed cycle head, want to lazily reevaluate it

We never evaluate cycles out of order, so we can have a single shared cycles iterator. 
In `evaluate_goal_in_task`, we check whether the next cycle uses the current stack, if so,
can now iterate over cycles as long as they share the current stack. However, as we only get here
if the result of a previous cycle changed, we may skip some cycles.

So, when we actually call `evaluate_goal` for the rerun, we eagerly compute all cycles which happened
while computing this goal given the current provisional results.

When evaluating a goal, if we get to `evaluate_goal_in_task`, we check whether any of the `parent_cycles`
go through the new goal. If so, we lazily reevaluate all of these cycles.

We want a `HashMap<GoalInfo<X>, Vec<Cycle<X>>>` which is activated while in the initial `compute_goal` of the subtree.
- we then lookup the nested goal
- check whether the provisional result of all stack entries match
- if so, lazily reevaluate this goal

issue: if we lazily reevaluate its first iteration, we won't have the search tree for the second one

that's ass

## out of order evaluation

The basic idea: when failing to evaluate a goal and having to evaluate a parent, we first try to lazily evaluate
all other nested goals of that parent and only evaluate the parent then.

- `TraitPredicate(D: IntoParallelIteratorIndir)`
  - via impl
    - `TraitPredicate(Box<D>: IntoParallelIterator)`
      - via impl
        - `TraitPredicate(Box<D>: ParallelIterator)`
          - `AliasRelate(Box<D>, <A as IntoParallelIteratorIndir>::Iter)`
            - `NormalizesTo(<A as IntoParallelIteratorIndir>::Iter)`
              - via impl
                - `TraitPredicate(Box<A>: IntoParallelIterator)`
                  - via impl
                    - `TraitPredicate(Box<A>: ParallelIterator)`
                      - `AliasRelate(Box<A>, <A as IntoParallelIteratorIndir>::Iter)`
                        - `NormalizesTo(<A as IntoParallelIteratorIndir>::Iter)` cycle overflow
                      - `AliasRelate(<A as IntoParallelIteratorIndir>::Iter, ?fresh)`
                        - `NormalizesTo(<A as IntoParallelIteratorIndir>::Iter)` cycle overflow
                      - `AliasRelate(Box<A>, <B as IntoParallelIteratorIndir>::Iter)`
                        - `NormalizesTo(<B as IntoParallelIteratorIndir>::Iter)`
                          - via impl
                            - `TraitPredicate(Box<B>: IntoParallelIterator)`
                              - via impl
                                - `TraitPredicate(Box<B>: ParallelIterator)`
                                  - `AliasRelate(Box<B>, <A as IntoParallelIteratorIndir>::Iter)`
                                    - `NormalizesTo(<A as IntoParallelIteratorIndir>::Iter)` cycle overflow
                                  - `AliasRelate(<A as IntoParallelIteratorIndir>::Iter, ?fresh)` provisional cache hit
                                  - `AliasRelate(Box<B>, <B as IntoParallelIteratorIndir>::Iter)`
                                    - `NormalizesTo(<B as IntoParallelIteratorIndir>::Iter)` cycle overflow
                                  - `AliasRelate(<B as IntoParallelIteratorIndir>::Iter, ?fresh)`
                                    - `NormalizesTo(<B as IntoParallelIteratorIndir>::Iter)` cycle overflow
                                  - `AliasRelate(Box<B>, <C as IntoParallelIteratorIndir>::Iter)`
                                    - `NormalizesTo(<C as IntoParallelIteratorIndir>::Iter)`
                                      - via impl
                                        - `TraitPredicate(Box<C>: IntoParallelIterator)`
                                          - via impl
                                            - `TraitPredicate(Box<C>: ParallelIterator)`
                                              - `AliasRelate(Box<C>, <A as IntoParallelIteratorIndir>::Iter)`
                                                - `NormalizesTo(<A as IntoParallelIteratorIndir>::Iter)` cycle overflow
                                              - `AliasRelate(<A as IntoParallelIteratorIndir>::Iter, ?fresh)` provisional cache hit
                                              - `AliasRelate(Box<C>, <B as IntoParallelIteratorIndir>::Iter)`
                                                - `NormalizesTo(<B as IntoParallelIteratorIndir>::Iter)` cycle overflow
                                              - `AliasRelate(<B as IntoParallelIteratorIndir>::Iter, ?fresh)` provisional cache hit
                                              - `AliasRelate(Box<C>, <C as IntoParallelIteratorIndir>::Iter)`
                                                - `NormalizesTo(<C as IntoParallelIteratorIndir>::Iter)` cycle overflow
                                              - `AliasRelate(<C as IntoParallelIteratorIndir>::Iter, ?fresh)`
                                                - `NormalizesTo(<C as IntoParallelIteratorIndir>::Iter)` cycle overflow
                                              - `AliasRelate(Box<C>, <D as IntoParallelIteratorIndir>::Iter)`
                                                - `NormalizesTo(<D as IntoParallelIteratorIndir>::Iter)`
                                                  - via impl
                                                    - `TraitPredicate(Box<D>: IntoParallelIterator)` cycle overflow
                                      - changed provisional result, reevaluate
                                        - reevaluate cycle: ?
                                          - reevaluate `NormalizesTo(<C as IntoParallelIteratorIndir>::Iter)` cycle rigid
                                          - reevaluate `AliasRelate(Box<C>, <C as IntoParallelIteratorIndir>::Iter)`
                                            - `NormalizesTo(<C as IntoParallelIteratorIndir>::Iter)` cycle rigid
                                          - reevaluate `TraitPredicate(Box<C>: ParallelIterator)`
                                            - `AliasRelate(Box<C>, <A as IntoParallelIteratorIndir>::Iter)` provisional cache hit
                                            - `AliasRelate(<A as IntoParallelIteratorIndir>::Iter, ?fresh)` provisional cache hit
                                            - `AliasRelate(Box<C>, <B as IntoParallelIteratorIndir>::Iter)` provisional cache hit
                                            - `AliasRelate(<B as IntoParallelIteratorIndir>::Iter, ?fresh)` provisional cache hit
                                            - `AliasRelate(Box<C>, <C as IntoParallelIteratorIndir>::Iter)` provisional cache hit
                                            - `AliasRelate(<D as IntoParallelIteratorIndir>::Iter, ?fresh)` provisional cache hit
                                  - `AliasRelate(Box<B>, <D as IntoParallelIteratorIndir>::Iter)`
                                    - `NormalizesTo(<D as IntoParallelIteratorIndir>::Iter)` provisional cache hit
                          - changed provisional result, reevaluate
                            - reevaluate cycle: ?
                              - reevaluate `NormalizesTo(<B as IntoParallelIteratorIndir>::Iter)` cycle rigid
                              - reevaluate `AliasRelate(Box<B>, <B as IntoParallelIteratorIndir>::Iter)`
                                - `NormalizesTo(<B as IntoParallelIteratorIndir>::Iter)` cycle rigid
                              - reevaluate `TraitPredicate(Box<B>: ParallelIterator)` :skull:
                                - reevaluate cycle `AliasRelate(<B as IntoParallelIteratorIndir>::Iter, ?fresh)`
                                  - `NormalizesTo(<B as IntoParallelIteratorIndir>::Iter)` cycle overflow
                                - reevaluate cycle `AliasRelate(Box<C>, <B as IntoParallelIteratorIndir>::Iter)`
                                  - `NormalizesTo(<B as IntoParallelIteratorIndir>::Iter)` cycle overflow
                                    - `AliasRelate(<B as IntoParallelIteratorIndir>::Iter, ?fresh)` provisional cache hit
                                  - reevaluate `TraitPredicate(Box<C>: ParallelIterator)`
                                    - reevaluate cycle `AliasRelate(<B as IntoParallelIteratorIndir>::Iter, ?fresh)` provisional cache hit
                                    - `AliasRelate(Box<C>, <A as IntoParallelIteratorIndir>::Iter)` provisional cache hit
                                    - `AliasRelate(<A as IntoParallelIteratorIndir>::Iter, ?fresh)` provisional cache hit
                                    - `AliasRelate(Box<C>, <B as IntoParallelIteratorIndir>::Iter)` provisional cache hit
                                    - `AliasRelate(<B as IntoParallelIteratorIndir>::Iter, ?fresh)` provisional cache hit
                                    - `AliasRelate(Box<C>, <C as IntoParallelIteratorIndir>::Iter)` provisional cache hit
                                    - `AliasRelate(<C as IntoParallelIteratorIndir>::Iter, ?fresh)` provisional cache hit
                                    - `AliasRelate(Box<C>, <D as IntoParallelIteratorIndir>::Iter)` provisional cache hit
                                    - has not changed, move this, parents, and siblings back to provisional cache
                                - `AliasRelate(Box<B>, <A as IntoParallelIteratorIndir>::Iter)` provisional cache hit
                                - `AliasRelate(<A as IntoParallelIteratorIndir>::Iter, ?fresh)` provisional cache hit
                                - `AliasRelate(Box<B>, <B as IntoParallelIteratorIndir>::Iter)` provisional cache hit
                                - `AliasRelate(<B as IntoParallelIteratorIndir>::Iter, ?fresh)` provisional cache hit
                                - `AliasRelate(Box<B>, <C as IntoParallelIteratorIndir>::Iter)` provisional cache hit

## fun bug

- A
  - B
    - C
      - B
    - A
  - D
    - C
  - rerun A
  - B has changed
  - D
    - C depends on B, but not A, must recompute D

## bug 2

issue, when reevaluating a goal which has changed, its parent has the wrong final result. The parent is the finished
evaluation from the previous iteration.


## taking a step back

The implementation has grown too complex and I cannot quite wrap my head around it. What should the algorithm be

At its core we recursively evaluate. When encountering a goal on the stack we return a provisional result. This result
depends on the path kind from the stack entry its usage during the first iteration and the result of the previous
iteration when rerunning this cyclic goal until we're reaching a fixpoint.

We can think of search graph changes as mutations of the final search tree. When figuring out whether we've reached a fixpoint,
we take the search tree of the previous iteration and replace all cyclic usages of the head with that tree. If this does not
change the result of the tree, doing it again won't change the result. So we can treat the cyclic paths as infinite.

We assume that the final result of all cycle participants stays the same regardless of where in the cycle they occur. The
result of B in BAB and in ABA should be the same. We need to cache the result of evaluating subtrees, even if they depend on
entries currently on the stack. We need to keep these entries around even after we've popped some of their cycle heads.

The search tree of some goal G with head H changes as follows if H is no longer on the stack when reevaluating G
- we replace all evaluations of G in the evaluation of H with a usage of the G stack entry
- we replace all usages of H in the evaluation of G with the previous evaluation of H

This impacts cycles as follows: the HGH cycles are now GHG cycles. Cycles of G which did not involve H are unchanged.
Cycles involving some other head P going through G and H now go only through G: PHGP -> PGP. Cycles of H involving some other head
P which not go through G now do: PHP -> PGHP. Rebasing G over P removes G from the cycle again: PGHP -> GHPH.

The search tree of some goal G with nested goal H changes as follows if H ends up on the stack when reevaluating G
- we replace all evaluations of H in G with usages of the H stack entry

GHG cycles are now HGH cycles. If H has a cycle which does not involve G, e.g. HPH, we now also have a cycle which goes
through G: GHPH -> HGH. This also means G may no longer depends on P at all. If G has a cycle that does not involve H,
this cycle does not change.

We also need to reuse results from previous fixpoint steps to avoid exponential blowup. The core question is whether
provisional cache entries are still valid given the change in provisional result. Importantly, to avoid exponential blowup,
reevaluating some goal must not result is separate uses of some other cycle head.

The only cache entries which *can* depend on the previous provisional result are cache entries created while evaluating the
current goal if these depend on that goal.

The core idea here is that we do the initial evaluation of a goal. If we fail to reach a fixpoint, we have to clear the dependent
provisional results. We want to then reevaluate as little as possible of its nested goals.

For this, we track all cyclic usages of this goal and all usages of provisional cache entries which depend on this goal.
We then evaluate all these goals from the bottom up:
- build the stack at the point of encountering this cycle/provisional cache entry
- evaluate the goal as normal:
  - if its result stayed the same, ok
    - if all nested goals of a goal stayed the same, pop that goal from the stack and move it to the provisional cache
    - if some other nested goal of the parent has changed, we want to reuse the already evaluated result as a provisional cache entry
      - this is weird as we've evaluated it with the current goal on the stack and we've since popped it
      - we can only keep entries which relied on the goal with the current provisional result
        - keeping a result from evaluating a nested goal during the final iteration to then reevaluate the parent is wrong, as the
          parent incorrectly uses a cache entry which is only correct if we're in the final iteration of the parent :thinking:

```rust
fn reevaluate() {
  let cycle_head_stack_len;
  let mut needs_reeval = vec![];
  let did_fully_reeval = HashSet;
  'outer: for cycle in cycles {
    let mut path_to_cycle = tree.path();

    for depth in cycle_head_stack_len.. {
      let entry = match (path_to_cycle.next(), stack.get(depth)) {
        (Some(cycle_entry), Some(stack_entry)) => {
          if stack_entry == cycle_entry {
            continue;
          } else {
            pop_stack_until(needs_reeval, depth, &mut did_fully_reeval);
            cycle_entry
          }
        }
        (Some(cycle_entry), None) => cycle_entry,
        (None, Some(stack_entry)) => {
          pop_stack_until(needs_reeval, depth);
          break;
        }
        (None, None) => break,
      };
      if did_fully_reeveal(cycle_entry.node_id) {
        continue 'outer;
      }
      stack.push(cycle_entry);
    } 
    
    if self.evaluate_goal(leaf).result_matches(..) {
      uwu
    } else {
      needs_reeval.push(self.stack.last_index())
    }
  }

  let final_entry = pop_stack_until(cycle_head_stack_len);
  final_entry
}

fn pop_stack_until(needs_reeval, depth, did_fully_) {
  while stack.len() > depth {
    let (entry, is_final_fixpoint_iteration) = if needs_reeval.last() == stack.last_index() {
      if stack.last().provisional_result.is_some() {
        let entry = stack.pop();
        tree.clear_cycles(entry.cycles_start);
        self.clear_provisional_results();
        stack.push(StackEntry {
          ...
        });
      }
      let reeval_result = evaluate_goal_on_stack();
      if reeval_entry.provisional_result.is_some()  {

      }
      let reeval_result = self.evaluate_goal();
      let reached_fixpoint = reached_fixpoint();
      if reeval_result == expected_result {
          (EvaluationResult { .. }, reached_fixpoint)
      } else if reached_fixpoint {
        needs_reeval.push(stack.last_index());
      }       }
    } else {
      stack.pop()
    };
    if is_final_fixpoint_iteration {
      match rebase_kind {
        rebase_provisional_cache_entries(entry)
      }
      insert_provisional_result()
    } else {
      clear_provisional_cache_entries()
    }
  }
}
```
Using `compute_goal` for nested goals kind of sucks. We need to handle fixpoint iterations
in some way :thinking:

Reevaluating a nested goal needs to invalidate the provisional results we've precomputed if they depend on it.

If the result has fewer cycle heads, we would like to reuse it :thinking: 

How to handle fixpoint iterations when reevaluating:
- when popping a non-final unchanged iteration from the stack, we need to clear provisional cache entries which depend on it
- when popping *not the first* changed iteration from the stack, we need to clear the provisional cache and update `cycles_start`
- rerun via `evaluate_goal_on_stack`. This sucks. Having rerunning use a distinct function from normal evaluation is rly complex and iffy
  - rerun via `evaluate_goal` needs to support 2 things:
    - take an existing cycle start

### what do we need

- every time the current goal has been accessed as a cycle head directly or a provisional cache entry which uses it as a cycle head
  - can be stored on the stack for the current goal, only ever used directly after we've finished one evaluation step
  - need to reuse the list when lazily recomputing some parent goal, that's chill
- every use of a cycle head needs to be able to
  - rebuild the stack at the point it was accessed
  - get the result of the current evaluation step of each parent stack entry
    - each time we rebuild the stack we give each stack entry a new node_id, node_id is per `evaluate_goal` but shared between evaluation steps
    - for each node, track a list of (provisional_result, step_result)
- `evaluate_goal_on_stack` // stack entry contains provisional result + cycles, we only ever reevaluate when we know the next cycle and pop until the stack is usable for that one
- compute tree with edges to parent goal
  - each cycle points to the last goal on the stack
  - provisional cache hits get put on the stack as we need to check them even if nothing changed to make sure we also consider them when rerunning
    - each cycle stores the stack from the head to the usage
    - need to diff the result? easier if we have the search tree, search tree needs to only contain nodes actually put on the stack

## what are the actual goals and questions I need to answer

- avoid all hangs during param-env normalization to support FCW rigid alias changes
  - https://github.com/rust-lang/trait-system-refactor-initiative/issues/89
  - https://github.com/rust-lang/trait-system-refactor-initiative/issues/216
- fix rayon in a way which we are fine stabilizing
  - the only fixes for rayon I can think of are either non-continous wrt to future language extensions: less impl shadowing, or
  - non-continous wrt to source code changes: adding `Projection` bounds to the where-clauses
- how to avoid feeling overwhelmed by being solely responsible for the 2 remaining large blockers of the new solver
  - search graph exponential blowup + opaque types handling
  - figure out how to keep opaque type changes next-solver only/behind a feature gate and then stabize for old solver after its been merged
    - this should be chill

assumption: we actually need two things, reevaluate with repop provisional cache to handle https://github.com/rust-lang/trait-system-refactor-initiative/issues/210 and some sort of "discard unnecessary work" to deal with rayon.

- is reeval with repop enough to handle #210?
  - i don't know, how long does it take to impl? :3
  - 2-3 days probably, i am unsure, it will likely take longer to actually merge it however
  - how long does it take to figure out whether something else is needed?
  - I don't know how to go about that, manually working on proof trees is hard, minimizing the test to be more structured may loose some of its complexity.
- does lowering the recursion limit actually give us anything useful over just not considering impl candidates?
  - more eagerly bail in failing alias-relate goals
  - also helpful for typenum potentially
- does weakening incompleteness cause any issues, and if not, land this separately
  - it only affects param_env/alias-bound candidates, resulting in slightly weaker inference, should try to land this
- what do i need to do to work on something else again and to no longer feel burdened by this
  - rayon compiles quickly, less than 5x of perf with the old solver
  - #210 compiles or I at least have a good understanding of why it's failing
  - reduced incompleteness is landed
- what do i want to do afterwards
  - opaque types change next-solver only, can split it into mutliple PRs
  - cleanup bootstrap PR and redo full crater run
  - types team blogpost
  - talk: help me, I am stressed about it :3

## impl cont

Interesting fun fact: we may evaluate nested goals of some goal and then evaluating the actual goal is a provisional or global cache hit.
- this happens in case reevaluating an earlier goal already depends a goal we'd be evaluating later or alternatively, we've dropped a goal from the provisional cache
- what can we do
  - avoid using the global cache or the provisional cache entry here
    - what if the goal actually doesn't depend on anything, we should still cache it and moving it into the provisional cache is wrong/would be hacky
  - erase all the info that we've ever computed nested stuff
    - when accessing a provisional cache result, we can completely clear all nested entries and cycles from the cache
    - when using a global cache entry we must support cycles whose highest head is the current goal

Issue: new impl does not quickly compile rayon:
- we always compute the branches of provisional cache hits, even if they did not change
- i believe this to be necessary as one could imagine the following setup
  - A
    - B does change depending on the result of D
      - C doesn't change even if D changes
        - D does not change when rerunning B does change once B and A get rerun
          - B
          - A
      - D
- what about the following pattern
  - A rerunning B while rerunning A does change the value of C
    - B rerunning B initially doesn't change the value of C
      - C
        - D
          - C
        - B
    - C does rerunning A need to also check whether C has changed



Separate question: is rerunning cycles necessary in general, could we avoid it?

## search graph: do it right this time?

- we create a new `NodeId` iff we (re)evaluate a goal.
- accessing a provisional cache entry adds it as a parent to the `entry_node_id`
- when reevaluating a goal, we copy the list of all parents, and do the following for all of them:
    - if its result changed, reevaluate all parents, lazily adding the `NodeId`s of each parents to the list of the new node
    - if its result did not change, copy the list of parents to the new node
- we've got a N-to-1 relationship between the parents and the set of nested goals used. if the store the node_id's of nested goals used by each `parent`, we can add all nodes whose list of cycle heads does not contain the current goal to the provisional cache, at least the ones which are already valid in the first iteration 
- this means whether a nested goal has changed changes its parent from a reuse to a new node. Lazily create nodes when finishing evaluation instead of eagerly?
    - instead of eagerly setting the parent, we only set it once we've finished evaluating the parent goal, before then all children are tracked in the parents `StackEntry`
    - actually: `evaluate_goal` adds the `node_id` of the nested goal to the stack entry of the parent. Finishing the parent updates the `parent` of all nested goals/provisional cache hits.

### the search tree of a reevaluation

When reevaluating we've got new nodes for goals we've reevaluated.

This node is the parent of both
- other nodes we're reevaluated just now
- unchanged nodes from the previous iteration we've accessed

This node has the same parents as the previous evaluation, but mapped
- if during reeval we pop the parent without reeval, we use the old node id
- we also rebuild the stack at all provisional cache hits of this goal
    - if all their nested goals don't change, reuse old node id
- if they changed, use the new `NodeId`

### reevaluating: figuring out the order

We want to walk through the reevaluated graph in order. For cycle head accesses,
the order is just the order of `NodeId`/`CycleId`.

For uses of a single provisional cache entry its just its list of parents. As some of them may have
been reevaluated while others have not, their `NodeId` is not well-ordered. Better idea:

Reevaluate by actually recursing into nested goals. Each node has `parents: Vec<(parent_id, Vec<child_id>)>`, put the goal on the stack and
- for each child, check whether the goal we're reevaluating is a cycle head of it
  - if no, try to add it to the provisional cache (if its not a cycle head access)
    - if it depends on the current goal we're adding, only do so for nested goals of its first fixpoint iteration
  - if yes, and the child is a use of the current cycle head, mark the current goal as `needs_reeval`
  - if yes, and the child is an ordinary nested goal, put the child on the stack and recurse
    - if the child has changed, mark the current goal as `needs_reeval`
  - if yes, and the child is a provisional cache hit, we will have already visited that `node_id` before.
    - if that resulted in a different result, set `needs_reeval`
- if the goal is marked as `needs_reeval`, reevaluate this goal "as normal". We strip all `provisional_cache_hits` from its nested goals before doing so
  - using a nested goal as provisional cache entry does not add anything to `nested_goals` of this `StackEntry`
- if not, add its `(node_id, Vec<child_id>)` to the `nested_goals` of its parent
- once the parent is done, it adds `parent_id, Vec<child_id>` to the `parents` of this goal

### sidenote: evaluating nested goals of the current is ass

It makes things harder and more annoying. What if the evaluation changes and ends up being global/a provisional cache hit.

Easier: can we readd provisional cache entries which did not change and mutate `has_been_used` on access instead?
- the answer should be yes
- need to make sure we correctly handle rebasing

### sidenote: having `(parent, Vec<child_id>)` is insufficient

At least if reruns have the same node id:
- evaluations share the same `NodeId` if none of their children changed their result

### why do we care about keeping `NodeId`s the same

To deduplicate work. We want to only visit each `NodeId` once. How would we do that?
- each `NodeId` always has the exact same stack, with different provisional results
- given that stack, it's got a set of nested goals, where each goal may have been computed multiple times, so for each nested goal we have a set of `NodeId`s

## another idea: change provisional cache entry to store path X provisional result

don't discard entries, ever :thinking: ...

## nyaaaa?

to fix perf here, we need to avoid reevaluating nested goals when reevaluating the parent.

we can do this if the reevaluations of the nested goal:
- only access parent as cycle head in a shared `node_id`

each `node_id` needs to know the path when it was first evaluated. This is
used when actually reevaluating it to make sure it doesn't access weird shit
if its behavior changes.

---

The whole reason we are using lazy reevaluation is to hide the fact that the search graph has an exponential size. This means we must not separately walk into the original evaluation and its reevaluations.

We can do this if all the evaluations have equal nested goal results. In this case there's no need to reeval. However, if we reevaluate this goal later and its result changes, we now need to reevaluate all goals which used it as a nested goal.

This needs to *lazily* branch between different reevaluations, as we'd otherwise get immediate exponential blowup again.

A goal only has multiple parents if the parent evaluation depends on a separate nested goals whose result also differs due to a rerun.

---

We can have the tree
- 0
  - 1 initial
    - 2
      - 0 cycle
      - 1 cycle
    - 3
  - 1 reeval (nested goals remain unchanged)
    - 4 unchanged
      - 0 cycle
      - 1 cycle
    - 3
- 0
  - 1 initial
    - 5 unchanged 
      - 0 cycle
      - 1 cycle
    - 3
  - 7 unchanged
    - 6 now changed in second iterations of 0 and 1
      - 0 cycle
      - 1 cycle
    - 3

Reevaluating 1 causes us to reevaluate 2, while 3 is shared.
If reevaluating a parent goal of 0 later means the result of 2 stays the same
but the result of 4 changes, we only need to reevaluate 0 from the second iteration
of 0.

As a more complete example:
- A0
  - B0
    - C0
      - D0
        - A cycle
        - B cycle
      - E
  - rerun B0
    - C0
      - D1
        - A cycle
        - B cycle
- rerun A0
  - B1
    - C0
      - D2
        - A cycle
        - B cycle
      - E
  - rerun B
    - C1
      - D3
        - A cycle
        - B cycle

Concretely, we need to store the following info for accesses of A

- stack: [B0 initial, C0 initial, D0 initial]
- stack: [B0 rerun, C0 initial, D1 initial]

---

We need to go from all its uses as cycle head to their parent goals.
Each goal then has a `IndexMap<(Vec<nested_goal_id>, Vec<parent_id>)>`.
If reevaluating a goal with its original stack changes its output, we need to reevaluate all of its parents. To do this, we need to know for each of the parent,
the first time that parent has been evaluated with the changed child as a nested goal!

---

We create a new node when actually having to reeval a goal. We can remember the stack of that node at this point. Let's look at a case where there are reuse nested and parent nodes

- A
  - B0
    - C0
      - D0
        - E0
          - B cycle
          - A cycle
        - F0
          - A cycle
      - B cycle
    - D cache hit D0
  - rerun
    - C1
      - D0
        - E1
          - B cycle
          - A cycle
        - F0
      - B cycle
    - D cache hit D0

When rerunning A we've got the following info:

Uses of A as cycle head:
- stack: [E0 initial, D0 initial, C0 initial, B0 initial]
- stack: [F0 initial, D0 initial, C0 initial, B0 initial]
- stack: [E1 initial, D0 initial, C1 initial, B0 rerun]

Nodes with multiple parents:
- D0:
  - with children [E0, F0]: [C0, cache hit B0],
  - children [E1, F0]: [C1, cache hit B0]

How do we get the relevant info when nodes change.

We rerun E0 using the stack from its use as cycle head.
E0 has a single parent D0, we mark it for rerun if E0 changed.
D0 has multiple parents. If its result changed, we mark its initial parent C0 for rerun and queue all parents which relied on E0.
C0 is boring. Let's say this one didn't change.
We then need to figure out whether E1 or C1 happened first... TODO

We rerun E1 using the stack from its use as cycle head.
Rerunning D0 if it changed should only reevealuates C1 and B0, not C0

---

- A
  - B0
    - C0
      - D0
        - E0
          - F0
            - B cycle
            - A cycle
      - B cycle
  - rerun
    - C1
      - D0
        - E0
          - F1
            - B cycle
            - A cycle
      - B cycle

Here if F1 changes, we only need to reevaluate C1, not C0. How do we track this?

C0 and C1 only differ due to the provisional result of one of its heads. F1 directly accesses the provisional result of A and B. We only ever split parents when reevaluating a head and can store that into the "parents list". When creating a new node, we store the cycle head and its itereation at which we've split.

So walking up the evaluation of F1, we know that it depends on the first rerun of B.
D0 has two parents, where the first is used in `iter(B) < 1` and the second one in `iter(B) >= 1`, we only need to rerun C1 because of this.

--- 

What do we do if we rerun a shared node, and that rerun now depends on the value of that cycle head?

- A
  - B0
    - C0
      - D0
        - B 
      - A
  - rerun
    - C0
      - D1 unchanged
        - B
      - A
- rerun
  - lazy reeval C0 (didn't depend on B directly, now does)
    - D[0|1]
      - B
    - A changed
    - B!

We initially reevaluate C0 with the stack when it was first used. If this now depends on the value of B we need to reevaluate it again for `iter(B) = 1`

For this to make sense, we need to track on which iteration of a cycle head we depend on. We'd need to use a provisional cache entry for `D` which states `0 < iter(B) <= 1`
while using the cycle head requires `iter(B) = 0`.

This means we've got the following reevals: `A0B01`, `A1B0`, `A1B1`.
- given that the initial iteration of `A`, the result of `B` does not matter, in later ones it does

What happens if the of the opposite happens, `B` matters for `A1` but not `A0`: `A0B0`, `A0B1`.
We then rerun `A1B0` and `A1B1`. If these two have the same result but both differ from `A0B0` and `A0B1`, then we end up with 4 evaluations instead of 3.
This is acceptable. Merging evaluations seems very hard when compared to not splitting.

## TODO

- change `has_been_used` to also get lazily updated in `update_parent_goal`, remove `IndexMut`, only last stack entry is mutable
- need to detect difference between using provisional cache entries for nested goals which were the same between iterations and accessing other entries/the cycle head
  - if the reeval depends on a nested cycle head directly, we now need to also reeval this goal in the other fixpoint iterations

- for cycle heads, also store range of valid iterations
- on rerun, if nested goals unchanged, pop entry from stack and change its 
