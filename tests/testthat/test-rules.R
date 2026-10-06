# =========================================================================== #
# The rule registry and per-column rule resolution.
# =========================================================================== #

test_that("register / get / list / clear round-trip", {
  withr::defer(clear_scale_rules())
  clear_scale_rules()
  register_scale_rule("cap", "sum")
  register_scale_rule("eff", "weighted_mean", weight = "size")

  expect_identical(get_scale_rule("cap")$rule, "sum")
  expect_identical(get_scale_rule("eff")$weight, "size")
  expect_null(get_scale_rule("never_registered"))

  tab <- list_scale_rules()
  expect_setequal(tab$param, c("cap", "eff"))
  expect_identical(tab$rule[tab$param == "cap"], "sum")

  clear_scale_rules("cap")
  expect_null(get_scale_rule("cap"))
  expect_identical(get_scale_rule("eff")$rule, "weighted_mean")
})

test_that("rules are scoped, and a scoped lookup falls back to unscoped", {
  withr::defer(clear_scale_rules())
  clear_scale_rules()
  register_scale_rule("load", "weighted_mean", scope = "calendar")
  register_scale_rule("load", "sum", scope = "geoscale")
  register_scale_rule("cost", "sum")

  # each dimension sees its own statement
  expect_identical(get_scale_rule("load", scope = "calendar")$rule,
                   "weighted_mean")
  expect_identical(get_scale_rule("load", scope = "geoscale")$rule, "sum")
  # an unscoped rule is visible from every scope
  expect_identical(get_scale_rule("cost", scope = "calendar")$rule, "sum")
  # and an unscoped lookup does not see the scoped entries
  expect_null(get_scale_rule("load"))
})

test_that("clearing a scope leaves the other scopes alone", {
  withr::defer(clear_scale_rules())
  clear_scale_rules()
  register_scale_rule("load", "weighted_mean", scope = "calendar")
  register_scale_rule("load", "sum", scope = "geoscale")
  clear_scale_rules(scope = "calendar")
  expect_null(get_scale_rule("load", scope = "calendar"))
  expect_identical(get_scale_rule("load", scope = "geoscale")$rule, "sum")
})

test_that("list_scale_rules() can be filtered by scope", {
  withr::defer(clear_scale_rules())
  clear_scale_rules()
  register_scale_rule("a", "sum", scope = "calendar")
  register_scale_rule("b", "sum", scope = "geoscale")
  expect_identical(list_scale_rules(scope = "calendar")$param, "a")
  expect_identical(nrow(list_scale_rules()), 2L)
})

test_that("registration validates its arguments", {
  expect_error(register_scale_rule("", "sum"), "non-empty string")
  expect_error(register_scale_rule("x", "nope"), "should be one of")
  expect_error(register_scale_rule("x", "sum", weight = c("a", "b")),
               "single string")
})

test_that("SCALE_RULES lists the supported rules", {
  expect_setequal(SCALE_RULES,
                  c("sum", "weighted_mean", "mean", "copy", "sd", "share",
                    "logshare"))
})

# Resolution ------------------------------------------------------------------

test_that("a scalar rule applies to every column", {
  r <- multiscales:::.rules_for(c("a", "b"), rule = "sum")
  expect_identical(r$a$rule, "sum")
  expect_identical(r$b$rule, "sum")
})

test_that("a named rule vector selects per column", {
  r <- multiscales:::.rules_for(
    c("a", "b"), rule = c(a = "sum", b = "mean"))
  expect_identical(r$a$rule, "sum")
  expect_identical(r$b$rule, "mean")
})

test_that("a named vector may not name unknown columns", {
  expect_error(
    multiscales:::.rules_for(c("a"), rule = c(a = "sum", zz = "mean")),
    "not value columns")
})

test_that("an unnamed multi-element rule is rejected", {
  expect_error(
    multiscales:::.rules_for(c("a", "b"), rule = c("sum", "mean")),
    "NAMED vector")
})

test_that("a column with no rule anywhere is an error", {
  withr::defer(clear_scale_rules())
  clear_scale_rules()
  expect_error(multiscales:::.rules_for("mystery"),
               "no aggregation rule for value column")
})

test_that("an explicit rule beats the registry", {
  withr::defer(clear_scale_rules())
  register_scale_rule("cap", "sum")
  r <- multiscales:::.rules_for("cap", rule = "mean")
  expect_identical(r$cap$rule, "mean")
})

test_that("the registry supplies the weight when the caller does not", {
  withr::defer(clear_scale_rules())
  register_scale_rule("eff", "weighted_mean", weight = "size")
  r <- multiscales:::.rules_for("eff")
  expect_identical(r$eff$weight, "size")
  # an explicit weight still wins
  r2 <- multiscales:::.rules_for("eff", weight = "count")
  expect_identical(r2$eff$weight, "count")
})
