# =========================================================================== #
# The subclass contract.
#
# `timescales::Calendar` and `geoscales::Geoscale` become S7 subclasses of
# `Scale`. Everything those rebases depend on is asserted here against a
# synthetic subclass, so the engine can never quietly break the arrangement:
#
#   * the parent validator runs, and the child's own checks run after it
#   * an ALIAS property (`timeframes` -> `frames`) reads AND writes
#   * `class()` keeps the child first and gains Scale at the TAIL, so the
#     twins' existing S3 registrations still win
#   * every engine accessor works on the subclass unchanged
#   * `scale_vocab()` can be overridden, which is what keeps the twins'
#     error messages in their own dimension's words
# =========================================================================== #

# A stand-in for Calendar: fixes the key, adds a prop, aliases `frames`,
# and adds one invariant of its own.
Widget <- S7::new_class(
  "Widget",
  parent = Scale,
  properties = list(
    payload = S7::new_property(S7::class_any, default = NULL),
    slots = scale_alias_property("frames")
  ),
  validator = function(self) {
    if (!identical(S7::prop(self, "key"), "widget")) {
      return("`key` must be \"widget\"")
    }
    NULL
  }
)

.widget <- function(...) {
  df <- data.frame(grp = c("A", "A", "B"),
                   widget = c("w1", "w2", "w3"),
                   w = c(1, 2, 3), stringsAsFactors = FALSE)
  Widget(leaftable = df, frames = c("grp", "widget"),
         members = list(grp = c("A", "B"), widget = c("w1", "w2", "w3")),
         key = "widget", meta = list(name = "wx", weights = "w",
                                     default_weight = "w"), ...)
}

test_that("a subclass inherits the parent validator", {
  # the PARENT's rule (members must match the leaftable) still bites
  df <- data.frame(grp = c("A", "A"), widget = c("w1", "w2"),
                   stringsAsFactors = FALSE)
  expect_error(
    Widget(leaftable = df, frames = c("grp", "widget"),
           members = list(grp = c("A", "GHOST"),
                          widget = c("w1", "w2")),
           key = "widget"),
    "exactly the non-NA")
})

test_that("a subclass adds its own invariants on top", {
  df <- data.frame(grp = c("A", "A"), unit = c("w1", "w2"),
                   stringsAsFactors = FALSE)
  expect_error(
    Widget(leaftable = df, frames = c("grp", "unit"),
           members = list(grp = "A", unit = c("w1", "w2")),
           key = "unit"),
    "must be \"widget\"")
})

test_that("class() puts the child first and Scale at the tail", {
  w <- .widget()
  cl <- class(w)
  # The "pkg::Child" spelling leads when the class is defined in a package --
  # which is why the twins keep their `print.geoscales::Geoscale`
  # registrations. Whether this test-local class gets that prefix depends on
  # how the suite runs (R CMD check does, load_all() does not), so assert the
  # invariant that actually matters: the CHILD precedes Scale.
  child <- which(cl %in% c("Widget", "multiscales::Widget"))
  expect_gt(length(child), 0L)
  expect_true("multiscales::Scale" %in% cl)
  expect_true(S7::S7_inherits(w, Scale))
  # this ordering is what lets a child's S3 method win over Scale's
  expect_lt(min(child), match("multiscales::Scale", cl))
  expect_identical(cl[[length(cl)]], "S7_object")
})

test_that("an alias property reads and writes the inherited prop", {
  w <- .widget()
  expect_identical(w@slots, scale_frames(w))
  expect_identical(S7::prop(w, "slots"), c("grp", "widget"))

  w@slots <- c("grp", "widget")
  expect_identical(S7::prop(w, "frames"), c("grp", "widget"))
  # an invalid write is caught by the PARENT validator, which proves the
  # setter really writes `frames` rather than storing a private copy
  expect_error({w@slots <- c("grp", "nope")}, "missing frame columns")
})

test_that("an alias setter no-ops on NULL", {
  # S7's generated constructor writes every dynamic property once with NULL
  # before the real values land; a setter that forwarded it would make the
  # subclass impossible to construct.
  w <- .widget()
  w@slots <- NULL
  expect_identical(scale_frames(w), c("grp", "widget"))
})

test_that("engine accessors work unchanged on a subclass", {
  w <- .widget()
  expect_identical(scale_frames(w), c("grp", "widget"))
  expect_identical(scale_units(w), c("w1", "w2", "w3"))
  expect_identical(scale_key(w), "widget")
  expect_identical(scale_weights(w), "w")
  expect_identical(nrow(scale_leaftable(w)), 3L)
  expect_equal(scale_coverage(w), c(w = 1))
  expect_true(scale_nests(w, "grp", "widget"))
  expect_identical(nrow(scale_family(w, "grp", "widget")), 3L)
})

test_that("a subclass carries its own extra property", {
  w <- .widget(payload = list(1, 2, 3))
  expect_length(S7::prop(w, "payload"), 3L)
  expect_null(S7::prop(.widget(), "payload"))
})

test_that("scale_vocab() can be overridden per dimension", {
  S7::method(scale_vocab, Widget) <- function(x, ...) {
    list(object = "Widget", frame = "slot", frames = "slots",
         unit = "widget", units = "widgets", atoms = "widgetbase")
  }
  w <- .widget()
  expect_identical(scale_vocab(w)$frame, "slot")
  # and the shared checker speaks the subclass's language
  expect_error(scale_units(w, "nope"), "is not a slot of this Widget")
})
