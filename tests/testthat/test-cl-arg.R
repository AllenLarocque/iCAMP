set.seed(1)
tree <- ape::rtree(40)
tree$tip.label <- paste0("sp", 1:40)
comm <- matrix(rpois(12 * 40, lambda = 3), nrow = 12,
               dimnames = list(paste0("s", 1:12), tree$tip.label))
dis <- ape::cophenetic.phylo(tree)

workerSeeds <- function(cl) {
  parallel::clusterEvalQ(cl, get(".Random.seed", envir = globalenv()))
}

test_that("pdist.big uses the supplied cluster", {
  cl <- newTestCluster(2)
  withr::defer(parallel::stopCluster(cl))
  ref <- withLibWorkers(
    iCAMP::pdist.big(tree, wd = withr::local_tempdir(), output = TRUE, nworker = 2))
  localNoNewClusters()
  got <- iCAMP::pdist.big(tree, wd = withr::local_tempdir(), output = TRUE,
                          nworker = 2, cl = cl)
  expect_equal(got, ref)
  # the descriptor path (output = FALSE) as well
  wd <- withr::local_tempdir()
  res <- iCAMP::pdist.big(tree, wd = wd, nworker = 1, cl = cl)
  pd <- bigmemory::attach.big.matrix(dget(file.path(wd, res$pd.file)))
  expect_equal(unname(pd[]), unname(ref))
})

test_that("NRI.p / NTI.p use the supplied cluster", {
  cl <- newTestCluster(2)
  withr::defer(parallel::stopCluster(cl))
  refNRI <- withLibWorkers(withr::with_seed(7,
    iCAMP::NRI.p(comm, dis, nworker = 2, rand = 20)))
  refNTI <- withLibWorkers(withr::with_seed(7,
    iCAMP::NTI.p(comm, dis, nworker = 2, rand = 20)))
  localNoNewClusters()
  expect_equal(withr::with_seed(7,
    iCAMP::NRI.p(comm, dis, nworker = 2, rand = 20, cl = cl)), refNRI)
  expect_equal(withr::with_seed(7,
    iCAMP::NTI.p(comm, dis, nworker = 2, rand = 20, cl = cl)), refNTI)
})

test_that("RC.pc reseeding reproduces", {
  cl <- newTestCluster(2)
  withr::defer(parallel::stopCluster(cl))
  localNoNewClusters()
  run <- function(stream) {
    parallel::clusterSetRNGStream(cl, stream)
    withr::with_seed(7, iCAMP::RC.pc(comm, rand = 20, nworker = 2, cl = cl))
  }
  a <- run(7)
  expect_identical(run(7), a)
  expect_false(identical(run(8), a))
})

test_that("icamp.big reseeding reproduces", {
  cl <- newTestCluster(2)
  withr::defer(parallel::stopCluster(cl))
  localNoNewClusters()
  run <- function(stream) {
    parallel::clusterSetRNGStream(cl, stream)
    withr::with_seed(7, iCAMP::icamp.big(
      comm, tree, pd.wd = withr::local_tempdir(), output.wd = withr::local_tempdir(),
      rand = 20, bin.size.limit = 5, nworker = 2, temp.save = FALSE, cl = cl)
    )$CbMPDiCBraya
  }
  a <- run(7)
  expect_true(is.matrix(a) || is.data.frame(a))
  expect_identical(run(7), a)
  expect_false(identical(run(8), a))
})

test_that("nworker follows the cluster", {
  cl2 <- newTestCluster(2)
  withr::defer(parallel::stopCluster(cl2))
  parallel::clusterSetRNGStream(cl2, 7)
  before <- workerSeeds(cl2)
  localNoNewClusters()
  iCAMP::RC.pc(comm, rand = 20, nworker = 1, cl = cl2)
  after <- workerSeeds(cl2)
  expect_length(after, 2)
  for (i in 1:2) expect_false(identical(after[[i]], before[[i]]))
})

test_that("bare cluster: missing library errors on the worker; cluster survives", {
  bare <- parallel::makeCluster(1)
  withr::defer(parallel::stopCluster(bare))
  sees <- parallel::clusterEvalQ(bare, requireNamespace("bigmemory", quietly = TRUE))[[1]]
  testthat::skip_if(isTRUE(sees), "bare worker can see bigmemory; precondition not met")
  localNoNewClusters()
  expect_error(
    iCAMP::pdist.big(tree, wd = withr::local_tempdir(), nworker = 1, cl = bare),
    "there is no package called")
  expect_equal(parallel::clusterEvalQ(bare, 1 + 1)[[1]], 2)
})
