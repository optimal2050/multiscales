# =========================================================================== #
# Shared fixtures.
#
# `scale_example()` is the package's own awkward-on-purpose hierarchy (reused
# code, cross-cutting pair, unassigned atom). These helpers add the small
# well-behaved cases that make single assertions readable.
# =========================================================================== #

# A clean 3-frame nesting hierarchy with one weight. Nothing awkward.
tidy_scale <- function(name = "tidy") {
  df <- data.frame(
    top  = c("T",  "T",  "T",  "T"),
    mid  = c("M1", "M1", "M2", "M2"),
    leaf = c("L1", "L2", "L3", "L4"),
    w    = c(1, 2, 3, 4),
    stringsAsFactors = FALSE
  )
  scale_from_leaftable(df, frames = c("top", "mid", "leaf"), name = name)
}

# A scale whose key column is named something other than a frame.
keyed_scale <- function(key = "id") {
  df <- data.frame(
    grp = c("A", "A", "B"),
    sub = c("a1", "a2", "b1"),
    id  = c("x", "y", "z"),
    w   = c(1, 1, 2),
    stringsAsFactors = FALSE
  )
  scale_from_leaftable(df, frames = c("grp", "sub"), key = key, name = "keyed")
}

# A minimal single-frame scale (the degenerate hierarchy).
flat_scale <- function() {
  scale_from_leaftable(
    data.frame(unit = c("u1", "u2"), w = c(1, 3), stringsAsFactors = FALSE),
    frames = "unit", name = "flat")
}
