# =========================================================================== #
# The backend quintet: detection, laziness, and class restoration.
#
# No conversion verbs exist yet, so these test the primitives directly -- the
# contract the engine will be written against.
# =========================================================================== #

.df <- function() {
  data.frame(
    unit = c("u1", "u2", "u3"), v = c(1, 2, 3),
    stringsAsFactors = FALSE
  )
}

test_that(".ms_backend() names every supported input", {
  expect_identical(.ms_backend(.df()), "data.frame")
  expect_identical(.ms_backend(list(a = 1)), NA_character_)

  skip_if_not_installed("tibble")
  expect_identical(.ms_backend(tibble::as_tibble(.df())), "tibble")
})

test_that(".ms_backend() recognises the lazy engines", {
  skip_if_tier_below("full")
  skip_if_not_installed("data.table")
  skip_if_not_installed("dtplyr")
  skip_if_not_installed("arrow")

  expect_identical(
    .ms_backend(data.table::as.data.table(.df())),
    "data.table"
  )
  expect_identical(
    .ms_backend(dtplyr::lazy_dt(data.table::as.data.table(.df()))), "dtplyr"
  )
  expect_identical(.ms_backend(arrow::arrow_table(.df())), "arrow")
})

test_that(".ms_is_lazy() marks exactly the query-producing backends", {
  expect_true(.ms_is_lazy("arrow"))
  expect_true(.ms_is_lazy("dtplyr"))
  expect_false(.ms_is_lazy("data.frame"))
  expect_false(.ms_is_lazy("tibble"))
  expect_false(.ms_is_lazy("data.table"))
})

test_that(".ms_require_backend() rejects unsupported input once", {
  expect_error(
    .ms_require_backend(list(a = 1)),
    "data\\.frame, tibble, data\\.table"
  )
  expect_identical(.ms_require_backend(.df()), "data.frame")
})

test_that(".ms_schema() describes columns without materialising rows", {
  s <- .ms_schema(.df())
  expect_identical(nrow(s), 0L)
  expect_named(s, c("unit", "v"))
})

test_that(".ms_restore() round-trips every eager backend to its own class", {
  for (bk in setdiff(test_backends(), c("dtplyr", "arrow"))) {
    x <- as_backend(.df(), bk)
    out <- .ms_restore(.ms_lazy(x, bk), bk)
    expect_true(.bk_expect_class(out, bk),
      label = sprintf("[%s] class restored", bk)
    )
    expect_equal(.bk_sort(out, "unit")$v, c(1, 2, 3),
      label = sprintf("[%s] values", bk)
    )
  }
})

test_that("lazy inputs stay lazy unless collect = TRUE", {
  skip_if_tier_below("full")
  for (bk in intersect(test_backends(), c("dtplyr", "arrow"))) {
    x <- as_backend(.df(), bk)
    out <- .ms_restore(x, bk)
    expect_true(.bk_expect_class(out, bk),
      label = sprintf("[%s] stays lazy", bk)
    )
    out2 <- .ms_restore(x, bk, collect = TRUE)
    expect_true(.bk_expect_class(out2, bk, collected = TRUE),
      label = sprintf("[%s] collect = TRUE materialises", bk)
    )
    expect_equal(.bk_sort(out2, "unit")$v, c(1, 2, 3),
      label = sprintf("[%s] collected values", bk)
    )
  }
})

# The verbs ------------------------------------------------------------------

test_that("recast_scale() honours the contract on every backend", {
  s <- scale_example()
  input <- data.frame(
    unit = c("U1", "U2", "U3", "U4", "U5", "U6"),
    value = c(1, 2, 3, 4, 5, 6),
    stringsAsFactors = FALSE
  )
  expect_backend_contract(
    input,
    function(x, collect = NULL) {
      suppressWarnings(
        recast_scale(x, s,
          from = "unit", to = "sector", rule = "sum",
          collect = collect
        )
      )
    },
    key_cols = "sector",
    # a lazy result carries the observed groups only; the eager one is
    # completed to the full vocabulary and so has an extra NA row
    value_cols = "value"
  )
})

test_that("recast_scale() with identifiers holds on every backend", {
  s <- scale_example()
  input <- data.frame(
    unit = rep(c("U1", "U2", "U3", "U4", "U5", "U6"), 2),
    year = rep(c(2020, 2021), each = 6),
    value = c(1, 2, 3, 4, 5, 6, 2, 4, 6, 8, 10, 12),
    stringsAsFactors = FALSE
  )
  expect_backend_contract(
    input,
    function(x, collect = NULL) {
      suppressWarnings(
        recast_scale(x, s,
          from = "unit", to = "sector", rule = "sum",
          values = "value", collect = collect
        )
      )
    },
    key_cols = c("sector", "year"),
    value_cols = "value"
  )
})

test_that("recast_to_atoms() honours the contract on every backend", {
  s <- scale_example()
  input <- data.frame(
    sector = c("P", "S"), cap = c(10, 20),
    stringsAsFactors = FALSE
  )
  expect_backend_contract(
    input,
    function(x, collect = NULL) {
      suppressWarnings(
        recast_to_atoms(x, s,
          from = "sector", rule = "sum",
          weight = "size", collect = collect
        )
      )
    },
    key_cols = "unit"
  )
})

test_that("every entry point rejects unsupported input the same way", {
  s <- scale_example()
  expect_backend_rejects(function(x) {
    recast_scale(x, s, from = "unit", to = "sector", rule = "sum")
  })
  expect_backend_rejects(function(x) {
    recast_to_atoms(x, s, from = "sector", rule = "sum")
  })
  expect_backend_rejects(function(x) {
    recast_from_atoms(x, s, to = "sector", rule = "sum")
  })
})

test_that(".ms_lazy() wraps a data.table for dtplyr translation", {
  skip_if_not_installed("data.table")
  skip_if_not_installed("dtplyr")
  x <- data.table::as.data.table(.df())
  expect_s3_class(.ms_lazy(x, "data.table"), "dtplyr_step")
  # every other backend passes through untouched
  expect_identical(.ms_lazy(.df(), "data.frame"), .df())
})
