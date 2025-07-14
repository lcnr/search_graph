# Important graphs

- A rerun
  - C
    - B does not change on rerun
      - A
  - D
    - C provisional cache hit
    - A

When reevaluating D as A changed, we should use the cache entry for C.