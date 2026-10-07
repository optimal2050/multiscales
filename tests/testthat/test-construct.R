# =========================================================================== #
# scale_from_leaftable(): defaults, normalisation, and the errors it owns.
# =========================================================================== #

test_that("frames must be columns of the leaftable", {
  df <- data.frame(a = "A", unit = "u1", stringsAsFactors = FALSE)
  expect_error(
    scale_from_leaftable(df, frames = c("a", "nope")),
    "missing frame column"
  )
  expect_error(
    scale_from_leaftable(df, frames = character()),
    "non-empty character vector"
  )
  expect_error(
    scale_from_leaftable("not a frame", frames = "a"),
    "must be a data.frame"
  )
})

test_that("the key defaults to `unit`, else to the finest frame", {
  df <- data.frame(
    grp = c("A", "A"), leaf = c("l1", "l2"),
    stringsAsFactors = FALSE
  )
  expect_identical(
    scale_key(scale_from_leaftable(df, c("grp", "leaf"))),
    "leaf"
  )

  df$unit <- c("u1", "u2")
  expect_identical(
    scale_key(scale_from_leaftable(df, c("grp", "leaf"))),
    "unit"
  )
})

test_that("an explicit key is honoured and must exist", {
  df <- data.frame(
    grp = c("A", "A"), leaf = c("l1", "l2"),
    id = c("x", "y"), stringsAsFactors = FALSE
  )
  expect_identical(scale_key(scale_from_leaftable(df, c("grp", "leaf"),
    key = "id"
  )), "id")
  expect_error(
    scale_from_leaftable(df, c("grp", "leaf"), key = "nope"),
    "not found in `leaftable`"
  )
})

test_that("the key column may not be empty or missing", {
  df <- data.frame(
    grp = c("A", "A"), leaf = c("l1", NA),
    stringsAsFactors = FALSE
  )
  expect_error(
    scale_from_leaftable(df, c("grp", "leaf")),
    "missing or empty values"
  )
  df$leaf <- c("l1", "")
  expect_error(
    scale_from_leaftable(df, c("grp", "leaf")),
    "missing or empty values"
  )
})

test_that("blank frame codes are normalised to NA", {
  df <- data.frame(
    grp = c("A", "", "  "), unit = c("u1", "u2", "u3"),
    stringsAsFactors = FALSE
  )
  s <- scale_from_leaftable(df, frames = c("grp", "unit"))
  expect_identical(sum(is.na(scale_leaftable(s)$grp)), 2L)
  expect_identical(scale_units(s, "grp"), "A")
})

test_that("weights default to the numeric non-frame columns", {
  df <- data.frame(
    grp = c("A", "A"), unit = c("u1", "u2"),
    w1 = c(1, 2), w2 = c(3, 4),
    label = c("x", "y"), stringsAsFactors = FALSE
  )
  s <- scale_from_leaftable(df, frames = c("grp", "unit"))
  expect_identical(scale_weights(s), c("w1", "w2"))
  expect_identical(S7::prop(s, "meta")$default_weight, "w1")
})

test_that("a scale may declare no weights at all", {
  df <- data.frame(
    grp = c("A", "A"), unit = c("u1", "u2"),
    stringsAsFactors = FALSE
  )
  s <- scale_from_leaftable(df, frames = c("grp", "unit"))
  expect_identical(scale_weights(s), character())
  expect_identical(scale_coverage(s), stats::setNames(numeric(), character()))
})

test_that("a named weight column must exist", {
  df <- data.frame(
    grp = c("A", "A"), unit = c("u1", "u2"),
    stringsAsFactors = FALSE
  )
  expect_error(
    scale_from_leaftable(df,
      frames = c("grp", "unit"),
      weights = "nope"
    ),
    "not found in `leaftable`"
  )
})

test_that("members are derived in first-appearance order", {
  df <- data.frame(
    grp = c("B", "A", "B"), unit = c("u1", "u2", "u3"),
    stringsAsFactors = FALSE
  )
  s <- scale_from_leaftable(df, frames = c("grp", "unit"))
  expect_identical(scale_units(s, "grp"), c("B", "A"))
})

test_that("a frame with no codes at all is an error", {
  df <- data.frame(
    grp = c(NA, NA), unit = c("u1", "u2"),
    stringsAsFactors = FALSE
  )
  expect_error(
    scale_from_leaftable(df, frames = c("grp", "unit")),
    "no codes at all"
  )
})

test_that("frames out of coarse-to-fine order warn", {
  df <- data.frame(
    fine = c("f1", "f2", "f3"),
    coarse = c("A", "A", "B"),
    unit = c("u1", "u2", "u3"),
    stringsAsFactors = FALSE
  )
  expect_warning(
    scale_from_leaftable(df, frames = c("fine", "coarse", "unit")),
    "coarsest first"
  )
})

test_that("extra ... entries land in meta", {
  df <- data.frame(
    grp = c("A", "A"), unit = c("u1", "u2"),
    stringsAsFactors = FALSE
  )
  s <- scale_from_leaftable(df,
    frames = c("grp", "unit"),
    name = "n", desc = "d", source = "somewhere"
  )
  expect_identical(S7::prop(s, "meta")$source, "somewhere")
  expect_identical(S7::prop(s, "meta")$name, "n")
  expect_identical(S7::prop(s, "meta")$desc, "d")
})
