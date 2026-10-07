# =========================================================================== #
# scale_map(): the crosswalk, its schema, and the registry.
# =========================================================================== #

test_that("the map has the documented schema", {
  s <- scale_example()
  m <- scale_map("class", "group", x = s)
  expect_named(m, c("class", "group", "n_from", "n_overlap", "w", "w_from"))
  expect_true(is.numeric(m$w))
  expect_true(is.integer(m$n_overlap))
})

test_that("n_from is the source unit's full atom count", {
  s <- scale_example()
  m <- scale_map("class", "unit", x = s)
  # class G1 holds U1 and U2
  expect_equal(unique(m$n_from[m$class == "G1"]), 2L)
  expect_true(all(m$n_overlap == 1L))
})

test_that("w_from is the source unit's full weight", {
  s <- scale_example()
  m <- scale_map("class", "unit", x = s, weight = "size")
  expect_equal(unique(m$w_from[m$class == "G1"]), 300)
  expect_equal(sum(m$w[m$class == "G1"]), 300)
})

test_that("uncovered atoms appear with an NA target", {
  s <- scale_example()
  m <- scale_map("unit", "sector", x = s)
  expect_true(any(is.na(m$sector)))
  expect_identical(m$unit[is.na(m$sector)], "OTH")
})

test_that("the split factor is well defined when no weight is declared", {
  df <- data.frame(
    grp = c("A", "A", "B"), unit = c("u1", "u2", "u3"),
    stringsAsFactors = FALSE
  )
  s <- scale_from_leaftable(df, frames = c("grp", "unit"), name = "noweights")
  m <- scale_map("grp", "unit", x = s)
  # every atom weighs 1, so w/w_from == n_overlap/n_from
  expect_equal(m$w / m$w_from, m$n_overlap / m$n_from)
})

test_that("scale_map() rejects a same-frame pair and a missing scale", {
  s <- scale_example()
  expect_error(scale_map("class", "class", x = s), "the same frame")
  expect_error(scale_map("class", "group"), "`x` is required")
  expect_error(scale_map("nope", "group", x = s), "is not a frame")
})

test_that("cross-object maps match on shared atom keys", {
  a <- scale_example()
  df <- data.frame(
    big = c("X", "X", "Y", "Y", "Y", "Y", "Z"),
    unit = c("U1", "U2", "U3", "U4", "U5", "U6", "OTH"),
    size = c(1, 1, 1, 1, 1, 1, 1),
    stringsAsFactors = FALSE
  )
  b <- scale_from_leaftable(df, frames = c("big", "unit"), name = "other")
  m <- scale_map(a, b)
  expect_named(m, c("example", "other", "n_from", "n_overlap", "w", "w_from"))
  expect_identical(nrow(m), 7L)
})

test_that("cross-object maps need distinct names and shared keys", {
  a <- scale_example()
  expect_error(scale_map(a, a), "same name")

  df <- data.frame(
    grp = c("A", "B"), unit = c("z1", "z2"),
    stringsAsFactors = FALSE
  )
  b <- scale_from_leaftable(df, frames = c("grp", "unit"), name = "disjoint")
  expect_error(scale_map(a, b), "share no `unit` keys")
})

test_that("scale_atom_pairs() is the documented seam", {
  s <- scale_example()
  d <- scale_atom_pairs(s, "class", "group")
  expect_named(d, c("from", "to", "w"))
  expect_identical(nrow(d), 7L)
})

test_that("extra map columns are carried through `by`", {
  s <- scale_example()
  d <- scale_atom_pairs(s, "class", "group")
  d2 <- rbind(cbind(d, year = 2020), cbind(d, year = 2021))
  m <- multiscales:::.finish_map(d2, "class", "group", by = "year")
  expect_true("year" %in% names(m))
  # the counts are per year, not doubled
  expect_equal(unique(m$n_from[m$class == "G1"]), 2L)
})

# Registry -------------------------------------------------------------------

test_that("a registered map is returned as-is", {
  s <- scale_example()
  withr::defer(clear_scale_maps())
  fake <- data.frame(
    class = "G1", group = "GC", n_from = 1L, n_overlap = 1L,
    w = 1, w_from = 1, stringsAsFactors = FALSE
  )
  register_scale_map("class", "group", fake, x = s)
  expect_equal(scale_map("class", "group", x = s), fake)
  expect_equal(get_scale_map("class", "group", x = s), fake)
})

test_that("registered maps are scoped to the object", {
  s <- scale_example()
  withr::defer(clear_scale_maps())
  fake <- data.frame(
    class = "G1", group = "GC", n_from = 1L, n_overlap = 1L,
    w = 1, w_from = 1, stringsAsFactors = FALSE
  )
  register_scale_map("class", "group", fake, x = s)
  # a different object's identically-named frames are unaffected
  expect_null(get_scale_map("class", "group", x = "other_scale"))
})

test_that("registering NULL removes the entry", {
  s <- scale_example()
  withr::defer(clear_scale_maps())
  fake <- data.frame(
    class = "G1", group = "GC", n_from = 1L, n_overlap = 1L,
    w = 1, w_from = 1, stringsAsFactors = FALSE
  )
  register_scale_map("class", "group", fake, x = s)
  register_scale_map("class", "group", NULL, x = s)
  expect_null(get_scale_map("class", "group", x = s))
})

test_that("a registered map must have the schema columns", {
  s <- scale_example()
  expect_error(
    register_scale_map("class", "group", data.frame(a = 1), x = s),
    "missing column"
  )
  expect_error(
    register_scale_map("class", "group", "nope", x = s),
    "must be a data.frame"
  )
})

test_that("list_scale_maps() reports the keys", {
  s <- scale_example()
  withr::defer(clear_scale_maps())
  clear_scale_maps()
  fake <- data.frame(
    class = "G1", group = "GC", n_from = 1L, n_overlap = 1L,
    w = 1, w_from = 1, stringsAsFactors = FALSE
  )
  register_scale_map("class", "group", fake, x = s)
  expect_identical(list_scale_maps()$key, "example:class->group")
})
