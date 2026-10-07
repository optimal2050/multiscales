# =========================================================================== #
# Structure queries: family, nests, coverage.
# =========================================================================== #

test_that("scale_family() returns the adjacent pairs by default", {
  s <- tidy_scale()
  fam <- scale_family(s)
  expect_named(fam, c("parent_frame", "parent", "child_frame", "child"))
  expect_setequal(unique(fam$parent_frame), c("top", "mid"))
  # one row per (parent, child) pair actually present
  expect_identical(nrow(scale_family(s, "top", "mid")), 2L)
  expect_identical(nrow(scale_family(s, "mid", "leaf")), 4L)
})

test_that("scale_family() omits atoms unassigned at either frame", {
  s <- scale_example()
  fam <- scale_family(s, "sector", "class")
  expect_false(anyNA(fam$parent))
  expect_false(anyNA(fam$child))
  # "OTH" is NA at both, so it contributes nothing
  expect_false("OTH" %in% fam$child)
})

test_that("scale_family() checks its frame arguments", {
  s <- tidy_scale()
  expect_error(scale_family(s, "nope", "mid"), "is not a frame")
  expect_error(scale_family(s, "top", "nope"), "is not a frame")
})

test_that("a single-frame scale has an empty family", {
  fam <- scale_family(flat_scale())
  expect_identical(nrow(fam), 0L)
  expect_named(fam, c("parent_frame", "parent", "child_frame", "child"))
})

test_that("scale_nests() detects a clean hierarchy", {
  s <- tidy_scale()
  expect_true(scale_nests(s, "top", "mid"))
  expect_true(scale_nests(s, "mid", "leaf"))
})

test_that("scale_nests() reports cross-cutting frames and their offenders", {
  s <- scale_example()
  ok <- scale_nests(s, "class", "group")
  expect_false(ok)
  expect_true("GB" %in% attr(ok, "offenders"))
})

test_that("scale_coverage() is 1 for a scale that was never subset", {
  s <- scale_example()
  expect_equal(scale_coverage(s), c(size = 1, count = 1))
  expect_equal(scale_coverage(s, "size"), 1)
  expect_error(scale_coverage(s, "nope"), "unknown weight")
})

test_that("scale_coverage() reads the bookkeeping when present", {
  s <- tidy_scale()
  sampled <- Scale(
    leaftable = scale_leaftable(s), frames = scale_frames(s),
    members = S7::prop(s, "members"), key = "leaf",
    meta = list(
      name = "part", weights = "w", default_weight = "w",
      coverage = c(w = 0.5), parent_totals = list(w = 20),
      parent_name = "tidy"
    )
  )
  expect_equal(scale_coverage(sampled), c(w = 0.5))
  expect_true(summary(sampled)$sampled)
  expect_output(print(summary(sampled)), "SAMPLED")
  expect_output(print(summary(sampled)), "of 'tidy'")
})
