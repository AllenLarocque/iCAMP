# iCAMP 1.9.1.9001

* New argument `cl = NULL` on the parallel functions reached by `icamp.big()`,
  `pdist.big()`, `NRI.p()` and `NTI.p()`: `pdist.big`, `NRI.p`, `NTI.p`,
  `bNRIn.p`, `bNTIn.p`, `bNRI.bin.big`, `bNTI.bin.big`, `RC.pc`, `RC.bin.bigc`,
  `maxbigm`, `tree.droot`, `tree.path`, `icamp.big`, `midpoint.root.big` and
  `taxa.binphy.big`. A cluster made by the caller (`parallel::makeCluster()`)
  replaces every cluster the function would create. `nworker` is then taken
  from `length(cl)` (and so is `nworker.pd` in `pdist.big`), and the cluster is
  never stopped. With `cl = NULL` the behaviour is unchanged.
* `icamp.big()` records `nworker = length(cl)` in `detail$setting` when `cl` is
  given.
* Added a testthat suite for `cl`.
