# Important graphs

## provisional cache entries for unchanged goals

- A rerun
  - C
    - B does not change on rerun
      - A
  - D
    - C provisional cache hit
    - A

When reevaluating D as A changed, we should use the cache entry for C.

## tracking provisional cache uses for reruns 1

- A rerun
  - B does change depending on the result of D
    - C doesn't change even if D changes
      - D does not change when rerunning B does change once B and A get rerun
        - B
        - A
    - D

Rerunning B in the first iteration of A does not need to reeval B as D does not change.
D does change when we're both reevaluating A and B. At this point we need to reevaluate B
itself as it accesses the changed provisional cache entry of D.

## tracking provisional cache uses for reruns 2

- A rerun
  - B does change on rerun
    - C
      - B
    - A
  - D
    - C must rerun as C may have changed

We must check whether ADC changed its result even though we've never directly reevaluated C.

## tracking provisional cache uses for reruns 3

- A rerun
  - B needs to be reevaluated when reevaluating A as E changed
    - C does not change when reevaluating A
      - D changes when reevaluating A
        - E changes when reevaluating A
          - D
        - A
    - E cache hit

Similar the proof tree above.