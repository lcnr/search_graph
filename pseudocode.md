# how to impl the last approach

```rust
fn evaluate_goal() {
    let available_depth = check_overflow()?;

    lookup_provisional_cache()?;

    lookup_global_cache()?;

    check_cycle()?;

    let result = evaluate_goal_in_task();

    if heads.is_empty() {
        insert_global_cache();
    } else {
        create_node();
        insert_provisional_cache();
    }
    result
}

fn evaluate_goal_in_task() {
    let mut result = D::compute_goal();
    let mut entry = stack.pop();
    if let Some(usage_kind) = entry.has_been_used {
        if reached_fixpoint(result, usage_kind) {
            rebase_provisional_cache_entries();
            return result;
        }
    } else {
        return result;
    }

    for i in 1..D::FIXPOINT_STEP_LIMIT {
        force_ambiguity(result)  {
            rebase_provisional_cache_entries();
            return result;
        }

        let (new_entry, new_result) = reevaluate_goal_step();
        if new_result == result {
            rebase_provisional_cache_entries(entry);
            return result;
        } else {
            result = new_result;
            entry = new_entry;
        }
    }

    rebase_provisional_cache_entries();
    fixpoint_overflow()
}

fn reevaluate_goal_step(prev_entry) {
    let children = prev_entry.children;
    let current_depth = self.stack.next_index();
    stack.push(StackEntry {
        ..prev_entry
    });

    let mut needs_reeval = false;
    let mut visited_nodes = HashMap::default();
    for child in children {
        match child.kind {
            CycleOnStack { head } => if current_depth == head {
                needs_reeval = true;
            } else {
                stack.last().children.push(child)
            }
            ProvisionalCacheHit { entry_node_id } => {
                if !tree.heads(entry_node_id).contains(current_depth) {
                    stack.last().children.push(child);
                } else if visited_nodes[entry_node_id].has_changed {
                    needs_reeval = true;
                } else {
                    stack.last().children.push(ProvisionalCacheHit {
                        entry_node_id: visted_nodes[entry_node_id]
                    });
                }
            }
            NestedEvaluation { node_id } => {
                if !tree.heads(entry_node_id).contains(current_depth) {
                    stack.last().children.push(child);
                } else {
                    match recursively_reevaluate_nested_goal(
                        &mut visited_nodes,
                        node_id
                    ) {
                        DidChange => needs_reeval = true,
                        HasNotChanged { new_node_id } => {
                            debug_assert_eq!(
                                stack.last().children.last().unwrap(),
                                node_id
                            );
                        }
                    }
                }
            }
        }
    }
}

fn recursively_reevaluate_nested_goal(visited_nodes, node_id) {

}
```