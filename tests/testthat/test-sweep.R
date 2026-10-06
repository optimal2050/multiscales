# =========================================================================== #
# Choosing k: the numbers, and what they are allowed to claim.
# =========================================================================== #

test_that("the sweep reports one row per k with the documented columns", {
  s <- three_group_scale()
  out <- cluster_sweep(three_group_data(), s, ks = 2:4, method = "hclust")
  expect_named(out, c("k", "within", "silhouette", "smallest"))
  expect_identical(out$k, 2:4)
  expect_false(anyNA(out$within))
})

test_that("within-cluster dispersion falls as k rises", {
  s <- three_group_scale()
  out <- cluster_sweep(three_group_data(), s, ks = 2:6, method = "hclust")
  expect_true(all(diff(out$within) <= 1e-8))
})

test_that("the silhouette peaks at the number of groups the data has", {
  s <- three_group_scale()
  out <- cluster_sweep(three_group_data(), s, ks = 2:6, method = "hclust")
  expect_identical(out$k[which.max(out$silhouette)], 3L)
})

test_that("smallest reports the smallest cluster, which is often the veto", {
  s <- three_group_scale()
  out <- cluster_sweep(three_group_data(), s, ks = c(2, 8),
                       method = "hclust")
  expect_identical(out$smallest[out$k == 8], 1L)
  expect_gt(out$smallest[out$k == 2], 1L)
})

test_that("k = 1 has no silhouette rather than a made-up one", {
  s <- three_group_scale()
  out <- cluster_sweep(three_group_data(), s, ks = 1:2, method = "hclust")
  expect_true(is.na(out$silhouette[out$k == 1]))
})

test_that("the sweep can drive the constrained clustering too", {
  s <- ordered_scale()
  out <- cluster_sweep(ordered_data(), s, ks = 2:4,
                       FUN = cluster_contiguous)
  expect_identical(out$k, 2:4)
  expect_identical(out$k[which.max(out$silhouette)], 3L)
})

test_that("nothing in the package picks k for you", {
  # There is deliberately no `best_k()`: the sweep reports and the modeller
  # decides. This test exists so adding one is a conscious act.
  expect_false(any(grepl("^best_k$", getNamespaceExports("multiscales"))))
})
