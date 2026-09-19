# =============================================================================
# Scale (S7 class) -- the dimension-agnostic core type
# =============================================================================
# A `Scale` is a nested partition of a set of atoms, in ANY dimension:
#
#   * `leaftable` -- flat enumeration of the atoms, with one column per frame
#                    in the hierarchy, a unique key column (named by `key`),
#                    and zero or more named weight columns
#   * `frames`    -- ordered character vector naming the hierarchy, COARSEST
#                    first (e.g. `c("section", "division", "class")`)
#   * `members`   -- named list giving the full ordered code vocabulary at each
#                    frame (e.g. `members$section = c("A", "B", ...)`)
#   * `key`       -- name of the atom key column (default `"unit"`)
#   * `meta`      -- small named list: `name`, `desc`, `weights`,
#                    `default_weight`, the sample bookkeeping triple
#                    (`coverage`, `parent_totals`, `parent_name`),
#                    `residuals`, `labels`, `source`
#
# Anything derivable from these (parent/child tables, ancestry, shares, frame
# ranks) is computed on demand. Nothing is cached on the object.
#
# Frames are *partitions of the atoms*, NOT necessarily a strict tree. Real
# hierarchies cross-cut, which is precisely why the atom layer exists and why
# conversion always routes through it. Test a given pair with `scale_nests()`.
#
# `timescales::Calendar` and `geoscales::Geoscale` are subclasses: they fix the
# key name, add their own invariants, and supply a dimension vocabulary through
# `scale_vocab()`.
# =============================================================================

#' Scale (S7 class)
#'
#' A nested discrete partition: a flat table of atoms plus the ordered
#' hierarchy of frames that groups them. The dimension is not named -- time and
#' space are the subclasses `timescales::Calendar` and `geoscales::Geoscale`;
#' anything else (industries, income brackets, temperature regimes, technology
#' vintages) is declared directly.
#'
#' Construct with [`scale_from_leaftable()`].
#'
#' @param leaftable `data.frame` with a unique key column, one column per frame
#'   in `frames`, and zero or more numeric weight columns.
#' @param frames Ordered character vector of frame names (coarsest first); each
#'   must appear as a column of `leaftable`.
#' @param members Named list; `members[[f]]` is the full ordered set of codes
#'   present at frame `f`. Must equal the non-`NA` values of `leaftable[[f]]`
#'   as a set.
#' @param key Name of the atom key column (a single string, default `"unit"`).
#' @param meta Named list of attributes (`name`, `desc`, `weights`,
#'   `default_weight`, `residuals` (see [`scale_residuals()`]), `labels`,
#'   `source`, sample bookkeeping).
#'
#' @return A `Scale` object.
#'
#' @seealso [`scale_from_leaftable()`], [`scale_frames()`], [`scale_units()`]
#' @examples
#' scale_example()
#' @export
Scale <- S7::new_class(
  "Scale",
  properties = list(
    leaftable = S7::new_property(S7::class_data.frame),
    frames    = S7::new_property(S7::class_character),
    members   = S7::new_property(S7::class_list),
    key       = S7::new_property(S7::class_character, default = "unit"),
    meta      = S7::new_property(S7::class_list, default = list())
  ),
  constructor = function(leaftable, frames, members, key = "unit",
                         meta = list()) {
    S7::new_object(
      S7::S7_object(),
      leaftable = leaftable,
      frames    = frames,
      members   = members,
      key       = key,
      meta      = meta
    )
  },
  validator = function(self) {
    errs <- character()

    leaftable <- S7::prop(self, "leaftable")
    frames    <- S7::prop(self, "frames")
    members   <- S7::prop(self, "members")
    key       <- S7::prop(self, "key")
    meta      <- S7::prop(self, "meta")

    # leaftable ---------------------------------------------------------------
    if (!is.data.frame(leaftable)) {
      return("`leaftable` must be a data.frame")
    }
    if (nrow(leaftable) == 0L) {
      errs <- c(errs, "`leaftable` must have at least one row")
    }

    # key ---------------------------------------------------------------------
    if (!is.character(key) || length(key) != 1L || is.na(key) ||
        !nzchar(key)) {
      return("`key` must be a single non-empty string")
    }
    if (!key %in% names(leaftable)) {
      errs <- c(errs, sprintf(
        "`leaftable` must have a `%s` column (the atom key)", key))
    }

    # frames ------------------------------------------------------------------
    if (!is.character(frames) || length(frames) == 0L) {
      errs <- c(errs, "`frames` must be a non-empty character vector")
    } else if (anyDuplicated(frames)) {
      errs <- c(errs, "`frames` must be unique")
    } else {
      bad <- frames[!is_valid_frame(frames)]
      if (length(bad) > 0L) {
        errs <- c(errs, sprintf(
          "`frames` contains invalid names: %s", .preview(bad)))
      }
      # The key name is allowed as the FINEST frame: there the frame column
      # *is* the key column, so nothing collides. As a coarser frame it would
      # clash with the key column, so it stays reserved there.
      clash <- intersect(frames, .RESERVED_COLS)
      if (key %in% frames && !identical(frames[length(frames)], key)) {
        clash <- c(clash, key)
      }
      if (length(clash) > 0L) {
        errs <- c(errs, sprintf(
          "`frames` may not use reserved names: %s", .preview(clash)))
      }
      missing_cols <- setdiff(frames, names(leaftable))
      if (length(missing_cols) > 0L) {
        errs <- c(errs, sprintf(
          "`leaftable` missing frame columns: %s", .preview(missing_cols)))
      }
    }

    # atom key values ---------------------------------------------------------
    if (key %in% names(leaftable)) {
      uid <- leaftable[[key]]
      if (!is.character(uid) || anyNA(uid) || any(!nzchar(uid))) {
        errs <- c(errs, sprintf(
          "`leaftable$%s` must be a non-empty character vector", key))
      } else if (anyDuplicated(uid)) {
        dup <- unique(uid[duplicated(uid)])
        errs <- c(errs, sprintf(
          paste0("`leaftable$%s` must be unique; duplicated: %s. ",
                 "Parallel dimensions belong in separate Scale objects."),
          key, .preview(dup)))
      }
    }

    # members -----------------------------------------------------------------
    if (!is.list(members)) {
      errs <- c(errs, "`members` must be a list")
    } else if (length(errs) == 0L) {
      missing_mb <- setdiff(frames, names(members))
      if (length(missing_mb) > 0L) {
        errs <- c(errs, sprintf(
          "`members` missing entries for: %s", .preview(missing_mb)))
      }
      for (f in intersect(frames, names(members))) {
        mb <- members[[f]]
        if (!is.character(mb) || length(mb) == 0L || anyNA(mb) ||
            any(!nzchar(mb)) || anyDuplicated(mb)) {
          errs <- c(errs, sprintf(
            "`members[[\"%s\"]]` must be a unique non-empty character vector",
            f))
          next
        }
        seen <- unique(stats::na.omit(as.character(leaftable[[f]])))
        if (!setequal(seen, mb)) {
          errs <- c(errs, sprintf(
            paste0("`members[[\"%s\"]]` must contain exactly the non-NA ",
                   "values present in `leaftable$%s`"), f, f))
        }
      }
    }

    # weights -----------------------------------------------------------------
    wts <- meta$weights
    if (!is.null(wts)) {
      if (!is.character(wts) || anyNA(wts)) {
        errs <- c(errs, "`meta$weights` must be a character vector")
      } else {
        for (w in wts) {
          if (!w %in% names(leaftable)) {
            errs <- c(errs, sprintf(
              "weight column `%s` not found in `leaftable`", w))
            next
          }
          v <- leaftable[[w]]
          if (!is.numeric(v)) {
            errs <- c(errs, sprintf("weight column `%s` must be numeric", w))
          } else if (any(!is.finite(v) & !is.na(v))) {
            errs <- c(errs, sprintf("weight column `%s` must be finite", w))
          } else if (any(v < 0, na.rm = TRUE)) {
            errs <- c(errs, sprintf("weight column `%s` must be >= 0", w))
          } else if (isTRUE(sum(v, na.rm = TRUE) <= 0)) {
            errs <- c(errs, sprintf("weight column `%s` sums to zero", w))
          }
        }
        dw <- meta$default_weight
        if (!is.null(dw) && !(length(dw) == 1L && dw %in% wts)) {
          errs <- c(errs, "`meta$default_weight` must be one of `meta$weights`")
        }
      }
    }

    # residuals ---------------------------------------------------------------
    # Units that carry a real value but are not a destination for allocation:
    # non-regionalized GDP, non-allocable taxes, an "n.e.c." bucket. Declared
    # per frame because codes are not unique across frames.
    res <- meta[["residuals"]]
    if (!is.null(res)) {
      if (!is.list(res) || is.null(names(res)) || any(!nzchar(names(res)))) {
        errs <- c(errs, paste0("`meta$residuals` must be a named list, one ",
                               "entry per frame"))
      } else {
        bad_fr <- setdiff(names(res), frames)
        if (length(bad_fr) > 0L) {
          errs <- c(errs, sprintf(
            "`meta$residuals` names frame(s) that are not frames: %s",
            .preview(bad_fr)))
        }
        for (f in intersect(names(res), frames)) {
          codes <- res[[f]]
          if (!is.character(codes) || anyNA(codes)) {
            errs <- c(errs, sprintf(
              "`meta$residuals[[\"%s\"]]` must be a character vector", f))
            next
          }
          unknown <- setdiff(codes, members[[f]])
          if (length(unknown) > 0L) {
            errs <- c(errs, sprintf(
              "`meta$residuals[[\"%s\"]]` names non-member(s): %s",
              f, .preview(unknown)))
          }
        }
      }
    }

    # meta --------------------------------------------------------------------
    if (!is.list(meta)) {
      errs <- c(errs, "`meta` must be a list")
    }

    # Sample bookkeeping: coverage must be verifiable from the object itself.
    # `[[ ]]`, never `$`: `meta$coverage` would partial-match a subclass's
    # `coverage_class` field.
    if (is.list(meta) && !is.null(meta[["coverage"]])) {
      cov <- meta[["coverage"]]
      wts <- meta[["weights"]] %||% character()
      if (!is.numeric(cov) || is.null(names(cov)) ||
          !all(names(cov) %in% wts)) {
        errs <- c(errs, paste0("`meta$coverage` must be a named numeric ",
                               "over declared weights"))
      } else if (!all(is.finite(cov)) || any(cov <= 0) || any(cov > 1)) {
        errs <- c(errs, "`meta$coverage` values must lie in (0, 1]")
      } else if (!is.null(meta[["parent_totals"]])) {
        pt <- meta[["parent_totals"]]
        for (w in intersect(names(cov), names(pt))) {
          got <- sum(leaftable[[w]], na.rm = TRUE) / pt[[w]]
          if (abs(got - cov[[w]]) > 1e-8) {
            errs <- c(errs, sprintf(
              "`meta$coverage[\"%s\"]` (%.6g) does not match the leaftable (%.6g)",
              w, cov[[w]], got))
          }
        }
      }
      if (!is.null(meta[["parent_name"]]) &&
          !(is.character(meta[["parent_name"]]) &&
            length(meta[["parent_name"]]) == 1L &&
            nzchar(meta[["parent_name"]]))) {
        errs <- c(errs, "`meta$parent_name` must be a single non-empty string")
      }
    }

    if (length(errs) == 0L) NULL else errs
  }
)

# Dimension vocabulary ---------------------------------------------------------

#' The dimension vocabulary of a scale
#'
#' Engine messages are written once and interpolated with the words of the
#' dimension at hand, so a `Calendar` flowing through the shared engine still
#' errors in the language of time ("timeframe", "timeslice") and a `Geoscale`
#' in the language of space ("geoframe", "region"). Subclasses supply their own
#' method; the `Scale` default is the neutral vocabulary.
#'
#' @param x A [`Scale`].
#' @param ... Passed to methods.
#'
#' @return A named list with elements `object`, `frame`, `frames`, `unit`,
#'   `units`, `atoms`.
#'
#' @examples
#' scale_vocab(scale_example())
#' @export
scale_vocab <- S7::new_generic("scale_vocab", "x")

S7::method(scale_vocab, Scale) <- function(x, ...) {
  list(object = "Scale", frame = "frame", frames = "frames",
       unit = "unit", units = "units", atoms = "atoms")
}

# Accessors --------------------------------------------------------------------

#' Frame rank
#'
#' Position of a frame in the hierarchy: 1 is the coarsest.
#'
#' @param x A [`Scale`].
#' @param frame Character vector of frame names.
#'
#' @return An integer vector of ranks; `NA` for names that are not frames of
#'   `x`.
#'
#' @examples
#' s <- scale_example()
#' scale_rank(s, c("group", "unit"))
#' @export
scale_rank <- function(x, frame) {
  .check_scale(x)
  match(frame, S7::prop(x, "frames"))
}

#' Frames of a Scale
#'
#' The hierarchy names, ordered coarsest first. The last entry is the atom
#' frame -- the finest units, which every other frame groups.
#'
#' @param x A [`Scale`].
#' @param finest Return only the finest (atom) frame.
#'
#' @return A character vector of frame names, or a single name when
#'   `finest = TRUE`.
#'
#' @examples
#' s <- scale_example()
#' scale_frames(s)
#' scale_frames(s, finest = TRUE)
#' @export
scale_frames <- function(x, finest = FALSE) {
  .check_scale(x)
  f <- S7::prop(x, "frames")
  if (isTRUE(finest)) f[length(f)] else f
}

#' Units of a Scale
#'
#' The code vocabulary at one frame, in canonical order.
#'
#' @param x A [`Scale`].
#' @param frame A single frame name; defaults to the finest frame (the atoms).
#'
#' @return A character vector of unit codes.
#'
#' @examples
#' s <- scale_example()
#' scale_units(s)
#' scale_units(s, "group")
#' @export
scale_units <- function(x, frame = NULL) {
  .check_scale(x)
  if (is.null(frame)) frame <- scale_frames(x, finest = TRUE)
  .check_frame(x, frame)
  S7::prop(x, "members")[[frame]]
}

#' The atom key column name
#'
#' @param x A [`Scale`].
#' @return A single string.
#' @examples
#' scale_key(scale_example())
#' @export
scale_key <- function(x) {
  .check_scale(x)
  S7::prop(x, "key")
}

#' The leaftable of a Scale
#'
#' The one-row-per-atom table the scale is built on, as a plain `data.frame` --
#' the exported accessor to prefer over reaching for `x@leaftable`.
#' `as.data.frame()` and `ggplot2::fortify()` on a Scale are equivalent, so
#' `ggplot(s) + geom_*()` pipelines work directly.
#'
#' @param x A [`Scale`].
#' @param row.names,optional Ignored (S3 signature compatibility).
#' @param ... Ignored.
#' @return A `data.frame`: one row per atom, with the frame columns plus any
#'   weight columns.
#' @examples
#' head(scale_leaftable(scale_example()))
#' head(as.data.frame(scale_example()))
#' @export
scale_leaftable <- function(x) {
  .check_scale(x)
  S7::prop(x, "leaftable")
}

#' @rdname scale_leaftable
#' @export
#' @method as.data.frame Scale
as.data.frame.Scale <- function(x, row.names = NULL, optional = FALSE, ...) {
  scale_leaftable(x)
}

S7::method(as.data.frame, Scale) <- as.data.frame.Scale

#' @rdname scale_leaftable
#' @export
`as.data.frame.multiscales::Scale` <- as.data.frame.Scale

#' Weight columns of a Scale
#'
#' @param x A [`Scale`].
#'
#' @return A character vector of weight column names (possibly empty).
#'
#' @examples
#' scale_weights(scale_example())
#' @export
scale_weights <- function(x) {
  .check_scale(x)
  S7::prop(x, "meta")$weights %||% character()
}

#' Subset a subclass's per-atom payload
#'
#' The seam a dimension package overrides when it carries per-atom data
#' alongside the leaftable -- `geoscales::Geoscale` and its geometry. Called
#' whenever the engine rebuilds an object from a row subset, with the kept row
#' indices. The `Scale` default has no payload and returns `x` unchanged.
#'
#' @param x A [`Scale`].
#' @param ... Method arguments: `i`, the integer vector of kept leaftable row
#'   indices.
#'
#' @return `x`, with any payload subset to the kept rows.
#'
#' @examples
#' identical(scale_payload_slice(scale_example(), 1:3),
#'           scale_example())
#' @export
scale_payload_slice <- S7::new_generic("scale_payload_slice", "x")

S7::method(scale_payload_slice, Scale) <- function(x, i, ...) x

#' An alias property for a subclass
#'
#' Builds a getter/setter property that reads and writes an inherited property
#' under a different name -- how `timescales::Calendar` keeps `@timeframes` and
#' `geoscales::Geoscale` keeps `@geoframes` working over the inherited
#' `@frames`, so no existing code has to change when they become subclasses.
#'
#' The setter is a no-op on `NULL`. S7's generated constructor writes every
#' dynamic property once with `NULL` before the real values are set, so a
#' setter that forwards `NULL` makes the class impossible to construct.
#'
#' @param name Name of the inherited property to alias.
#'
#' @return An S7 property.
#'
#' @examples
#' Aliased <- S7::new_class("Aliased", parent = Scale,
#'   properties = list(levels = scale_alias_property("frames")))
#' a <- Aliased(leaftable = scale_leaftable(scale_example()),
#'              frames = scale_frames(scale_example()),
#'              members = S7::prop(scale_example(), "members"))
#' a@levels
#' @export
scale_alias_property <- function(name) {
  force(name)
  S7::new_property(
    getter = function(self) S7::prop(self, name),
    setter = function(self, value) {
      if (is.null(value)) return(self)
      S7::prop(self, name) <- value
      self
    }
  )
}

#' Units that are residuals
#'
#' A residual carries a real value but is not a destination for allocation:
#' non-regionalized GDP, taxes that belong to no industry, an "n.e.c." bucket.
#' It aggregates upward like any other unit -- that is the point, it is what
#' makes the parent total reconcile -- but [`recast_scale()`] never splits a
#' coarse figure INTO one.
#'
#' Declare them when building the scale, per frame, because codes are not
#' unique across frames:
#' `scale_from_leaftable(..., residuals = list(unit = "DE_XR"))`.
#'
#' @param x A [`Scale`].
#' @param frame A single frame name for just that frame's residuals, or
#'   `NULL` for the whole named list.
#'
#' @return A character vector of unit codes, or the named list over frames.
#'   Empty when the scale declares none.
#'
#' @examples
#' s <- scale_example()
#' scale_residuals(s)                       # none declared
#'
#' lf <- scale_leaftable(s)
#' r <- scale_from_leaftable(
#'   lf, frames = scale_frames(s), key = "unit",
#'   weights = scale_weights(s), name = "with_residual",
#'   residuals = list(unit = "OTH"))
#' scale_residuals(r, "unit")
#' @export
scale_residuals <- function(x, frame = NULL) {
  .check_scale(x)
  res <- S7::prop(x, "meta")[["residuals"]]
  if (is.null(res)) {
    return(if (is.null(frame)) list() else character())
  }
  if (is.null(frame)) return(res)
  .check_frame(x, frame)
  res[[frame]] %||% character()
}

#' @noRd
.check_scale <- function(x, arg = "x") {
  if (!S7::S7_inherits(x, Scale)) {
    # A product is the near miss worth naming: it holds scales but is not one,
    # and its per-atom table is the thing a caller must ask for deliberately.
    if (S7::S7_inherits(x, ScaleProduct)) {
      .stop(paste0("`%s` is a ScaleProduct, not a single Scale. Its atoms ",
                   "are not materialised -- use `scale_axes()` for the ",
                   "component scales, or `product_atoms()` to build the ",
                   "grid."), arg)
    }
    .stop("`%s` must be a Scale object", arg)
  }
  invisible(TRUE)
}

#' Resolve a frame name against a Scale
#'
#' @param x A [`Scale`].
#' @param frame A single frame name.
#' @param arg Argument name used in the error message.
#' @noRd
.check_frame <- function(x, frame, arg = NULL) {
  f <- S7::prop(x, "frames")
  v <- scale_vocab(x)
  if (is.null(arg)) arg <- v$frame
  if (is.null(frame) || length(frame) != 1L || is.na(frame)) {
    .stop("`%s` must be a single %s name; one of: %s",
          arg, v$frame, paste(f, collapse = ", "))
  }
  if (!frame %in% f) {
    .stop("`%s` = \"%s\" is not a %s of this %s; one of: %s",
          arg, frame, v$frame, v$object, paste(f, collapse = ", "))
  }
  invisible(frame)
}

#' Resolve the weight column to use
#' @noRd
.resolve_weight <- function(x, weight = NULL) {
  wts <- scale_weights(x)
  if (is.null(weight)) {
    weight <- S7::prop(x, "meta")$default_weight %||%
      (if (length(wts) > 0L) wts[[1L]] else NULL)
  }
  if (is.null(weight)) {
    .stop(paste0("no weight column available; declare one via ",
                 "`meta$weights` or pass `weight=`"))
  }
  if (!weight %in% wts) {
    .stop("`weight` = \"%s\" is not a weight column; one of: %s",
          weight, if (length(wts)) paste(wts, collapse = ", ") else "<none>")
  }
  weight
}

# Format / print ---------------------------------------------------------------

S7::method(format, Scale) <- function(x, ...) {
  sprintf("<Scale[%s] atoms=%d>",
          paste(S7::prop(x, "frames"), collapse = "/"),
          nrow(S7::prop(x, "leaftable")))
}

#' @export
#' @method print Scale
print.Scale <- function(x, ...) {
  meta <- S7::prop(x, "meta")
  f    <- S7::prop(x, "frames")
  mb   <- S7::prop(x, "members")
  lf   <- S7::prop(x, "leaftable")
  v    <- scale_vocab(x)

  name <- meta$name %||% ""
  cat(v$object, ": ", if (nzchar(name)) name else "<unnamed>", "\n", sep = "")
  if (!is.null(meta$desc) && nzchar(meta$desc)) {
    cat("Description:", meta$desc, "\n")
  }

  cat(toupper(substring(v$frames, 1, 1)), substring(v$frames, 2),
      " (", length(f), ", coarsest first):\n", sep = "")
  for (i in seq_along(f)) {
    l <- f[i]
    n_code <- length(mb[[l]])
    n_na <- sum(is.na(lf[[l]]))
    cat("  ", strrep("  ", i - 1L), "- ", l, " (", n_code, ")",
        if (n_na > 0L) sprintf("  [%d %s unassigned]", n_na, v$unit) else "",
        "\n", sep = "")
  }
  cat("Atoms: ", nrow(lf), "\n", sep = "")

  wts <- scale_weights(x)
  if (length(wts) > 0L) {
    dw <- meta$default_weight %||% wts[[1L]]
    cat("Weights: ", paste(wts, collapse = ", "),
        " (default: ", dw, ")\n", sep = "")
  }
  if (!is.null(meta$source)) cat("Source: ", meta$source, "\n", sep = "")
  invisible(x)
}

S7::method(print, Scale) <- print.Scale

# Dispatch on the fully-qualified S7 class name: `class()` reports
# `multiscales::Scale` first, and that is the registration base-R `print()`
# finds before falling through to `print.S7_object`.
#' @export
`print.multiscales::Scale` <- print.Scale

# Summary ----------------------------------------------------------------------

#' Summarize a Scale
#'
#' Complements [print()] with the quantitative view: per-weight totals and
#' coverage, and the adjacent-frame nesting table. Returns a `"summary_Scale"`
#' object (a list) with its own print method.
#'
#' @param object A [`Scale`].
#' @param x A `"summary_Scale"` object (the print method's argument).
#' @param ... Ignored.
#' @return `summary()` returns a list of class `"summary_Scale"`: `name`,
#'   `desc`, `frames` (named member counts), `unassigned` (named NA-atom
#'   counts), `n_atoms`, `weights`, `weight_totals`, `default_weight`,
#'   `coverage` (see [scale_coverage()]), `sampled`, `parent_name`, `nesting`
#'   (adjacent-pair table with offender counts, see [scale_nests()]),
#'   `source`, `vocab`.
#' @examples
#' summary(scale_example())
#' @export
#' @method summary Scale
summary.Scale <- function(object, ...) {
  meta <- S7::prop(object, "meta")
  lt   <- S7::prop(object, "leaftable")
  fr   <- S7::prop(object, "frames")
  wts  <- scale_weights(object)
  cov  <- scale_coverage(object)

  nesting <- NULL
  if (length(fr) > 1L) {
    nesting <- do.call(rbind, lapply(seq_len(length(fr) - 1L), function(i) {
      ok <- scale_nests(object, fr[i], fr[i + 1L])
      data.frame(parent = fr[i], child = fr[i + 1L],
                 nests = isTRUE(ok),
                 n_offenders = length(attr(ok, "offenders")),
                 stringsAsFactors = FALSE)
    }))
  }

  out <- list(
    name = meta[["name"]] %||% "",
    desc = meta[["desc"]] %||% "",
    frames = vapply(S7::prop(object, "members")[fr], length, integer(1)),
    unassigned = vapply(fr, function(l) sum(is.na(lt[[l]])), integer(1)),
    n_atoms = nrow(lt),
    weights = wts,
    weight_totals = if (length(wts)) {
      colSums(as.data.frame(lt)[, wts, drop = FALSE], na.rm = TRUE)
    } else numeric(0),
    default_weight = meta[["default_weight"]],
    coverage = cov,
    sampled = any(cov < 1),
    parent_name = meta[["parent_name"]],
    nesting = nesting,
    source = meta[["source"]],
    vocab = scale_vocab(object)
  )
  class(out) <- "summary_Scale"
  out
}

S7::method(summary, Scale) <- summary.Scale

#' @rdname summary.Scale
#' @export
`summary.multiscales::Scale` <- summary.Scale

#' @rdname summary.Scale
#' @export
#' @method print summary_Scale
print.summary_Scale <- function(x, ...) {
  v <- x$vocab %||% list(object = "Scale", frames = "frames", units = "units")
  cat("<summary of ", v$object,
      if (nzchar(x$name)) paste0(" '", x$name, "'"), ">\n", sep = "")
  if (nzchar(x$desc)) cat("  desc:          ", x$desc, "\n", sep = "")
  cat("  ", format(paste0(v$frames, ":"), width = 16),
      paste(sprintf("%s (%d)", names(x$frames), x$frames), collapse = " / "),
      "\n", sep = "")
  cat("  atoms:          ", x$n_atoms, "\n", sep = "")
  if (any(x$unassigned > 0)) {
    ua <- x$unassigned[x$unassigned > 0]
    cat("  unassigned:     ",
        paste(sprintf("%s (%d)", names(ua), ua), collapse = ", "),
        "\n", sep = "")
  }
  if (length(x$weights)) {
    cat("  weight totals:  ",
        paste(sprintf("%s = %s", names(x$weight_totals),
                      format(x$weight_totals, big.mark = ",", digits = 6)),
              collapse = ", "),
        if (!is.null(x$default_weight)) {
          paste0("  (default: ", x$default_weight, ")")
        },
        "\n", sep = "")
  }
  if (isTRUE(x$sampled)) {
    cat("  SAMPLED:        ",
        paste(sprintf("%s %.1f%%", names(x$coverage), 100 * x$coverage),
              collapse = ", "),
        if (!is.null(x$parent_name) && nzchar(x$parent_name)) {
          paste0(" of '", x$parent_name, "'")
        },
        "\n", sep = "")
  }
  if (!is.null(x$nesting)) {
    for (i in seq_len(nrow(x$nesting))) {
      r <- x$nesting[i, ]
      cat("  nesting:        ", r$parent, " > ", r$child, ": ",
          if (r$nests) "nested" else
            paste0("CROSS-CUTTING (", r$n_offenders, " offender(s))"),
          "\n", sep = "")
    }
  }
  if (!is.null(x$source)) cat("  source:         ", x$source, "\n", sep = "")
  invisible(x)
}

# Other base generics ----------------------------------------------------------

#' @rdname scale_frames
#' @export
#' @method names Scale
names.Scale <- function(x) scale_frames(x)

S7::method(names, Scale) <- names.Scale

#' @export
`names.multiscales::Scale` <- names.Scale
