# =============================================================================
# Structure queries over a DiscreteScale
# =============================================================================
# Pure leaftable queries -- no conversion engine involved. The wider navigation
# family (ancestry, children/parents/descendants/ancestors, share) joins this
# file when the engine verbs land.
# =============================================================================

#' Immediate parent-child table between two frames
#'
#' @param x A [`DiscreteScale`].
#' @param parent,child Frame names. Defaults to every adjacent pair in
#'   `x@frames`.
#'
#' @return A `data.frame` with columns `parent_frame`, `parent`, `child_frame`,
#'   `child`. Atoms unassigned at either frame are omitted.
#'
#' @examples
#' s <- scale_example()
#' scale_family(s, "group", "unit")
#' @export
scale_family <- function(x, parent = NULL, child = NULL) {
  .check_scale(x)
  fr <- S7::prop(x, "frames")

  if (is.null(parent) && is.null(child)) {
    if (length(fr) < 2L) {
      return(.empty_family())
    }
    parts <- lapply(seq_len(length(fr) - 1L), function(i) {
      scale_family(x, fr[i], fr[i + 1L])
    })
    out <- do.call(rbind, parts)
    rownames(out) <- NULL
    return(out)
  }
  .check_frame(x, parent, "parent")
  .check_frame(x, child, "child")

  leaves <- S7::prop(x, "leaftable")
  d <- data.frame(
    parent = as.character(leaves[[parent]]),
    child = as.character(leaves[[child]]),
    stringsAsFactors = FALSE
  )
  d <- unique(d[!is.na(d$parent) & !is.na(d$child), , drop = FALSE])
  out <- data.frame(
    parent_frame = parent,
    parent = d$parent,
    child_frame = child,
    child = d$child,
    stringsAsFactors = FALSE
  )
  out <- out[order(out$parent, out$child), , drop = FALSE]
  rownames(out) <- NULL
  out
}

#' @noRd
.empty_family <- function() {
  data.frame(
    parent_frame = character(), parent = character(),
    child_frame = character(), child = character(),
    stringsAsFactors = FALSE
  )
}

#' Do two frames nest?
#'
#' Tests whether every code at the finer frame falls entirely within a single
#' code at the coarser frame. Real hierarchies often fail this: a spatial code
#' can straddle two parents, and a reporting classification can assign one
#' detailed class to several aggregates.
#'
#' Nesting is *not* required by [`DiscreteScale`] -- conversion routes through the atom
#' layer and works either way. This function is a diagnostic.
#'
#' @param x A [`DiscreteScale`].
#' @param parent,child Frame names.
#'
#' @return `TRUE` or `FALSE`. When `FALSE`, the offending child codes are
#'   attached as the `"offenders"` attribute.
#'
#' @examples
#' s <- scale_example()
#' scale_nests(s, "class", "group") # FALSE - they cross-cut
#' @export
scale_nests <- function(x, parent, child) {
  fam <- scale_family(x, parent, child)
  multi <- unique(fam$child[duplicated(fam$child)])
  ok <- length(multi) == 0L
  if (!ok) attr(ok, "offenders") <- multi
  ok
}

#' Ancestry between all frame pairs
#'
#' Every `(coarser, finer)` code pair that shares at least one atom, for all
#' frame pairs.
#'
#' Computed **atom-mediated**, directly from the leaftable -- deliberately not
#' as a transitive closure of [`scale_family()`]. Closing a hierarchy that
#' cross-cuts manufactures false relationships: in the example scale, group
#' `GB` straddles both sectors, so closing `sector -> class -> group -> unit`
#' would wrongly report sector `P` as an ancestor of unit `U5`, which lies in
#' sector `S`.
#'
#' For frames that do not nest this relation is *overlap*, not containment --
#' test a given pair with [`scale_nests()`].
#'
#' Frame columns are retained because codes are not unique across frames: in
#' the example, `"G1"` exists at both `class` and `group`, so a bare
#' `(parent, child)` pair would read as a self-loop.
#'
#' @param x A [`DiscreteScale`].
#'
#' @return A `data.frame` with columns `parent_frame`, `parent`,
#'   `child_frame`, `child`.
#'
#' @examples
#' head(scale_ancestry(scale_example()))
#' @export
scale_ancestry <- function(x) {
  .check_scale(x)
  fr <- S7::prop(x, "frames")
  if (length(fr) < 2L) {
    return(.empty_family())
  }

  parts <- list()
  for (i in seq_len(length(fr) - 1L)) {
    for (j in seq(i + 1L, length(fr))) {
      parts[[length(parts) + 1L]] <- scale_family(x, fr[i], fr[j])
    }
  }
  out <- do.call(rbind, parts)
  out <- out[order(
    match(out$parent_frame, fr), out$parent,
    match(out$child_frame, fr), out$child
  ), , drop = FALSE]
  rownames(out) <- NULL
  out
}

#' Navigate a DiscreteScale's hierarchy
#'
#' Codes related to `unit` at another frame, found through the atoms.
#' `scale_children()`/`scale_parents()` step one frame down/up by default;
#' `scale_descendants()`/`scale_ancestors()` report every finer/coarser frame
#' as a frame-tagged table.
#'
#' @param x A [`DiscreteScale`].
#' @param frame The frame `unit` belongs to.
#' @param unit Character vector of codes.
#' @param to Target frame. `NULL` uses the adjacent frame (children/parents)
#'   or every finer/coarser frame (descendants/ancestors).
#'
#' @return A character vector (children/parents) or a `data.frame` with
#'   columns `frame` and `unit` (descendants/ancestors).
#'
#' @examples
#' s <- scale_example()
#' scale_children(s, "sector", "P")
#' scale_parents(s, "unit", "U5")
#' scale_descendants(s, "sector", "P")
#' scale_ancestors(s, "unit", "U5")
#' @name scale_navigate
NULL

#' @rdname scale_navigate
#' @export
scale_children <- function(x, frame, unit, to = NULL) {
  .check_scale(x)
  .check_frame(x, frame)
  fr <- .nav_levels(x)
  i <- match(frame, fr)
  if (is.null(to)) {
    if (i == length(fr)) {
      .stop(
        "`%s` is the finest %s; it has no children", frame,
        scale_vocab(x)$frame
      )
    }
    to <- fr[i + 1L]
  }
  .check_frame(x, to, "to")
  .related(x, frame, unit, to)
}

#' @rdname scale_navigate
#' @export
scale_parents <- function(x, frame, unit, to = NULL) {
  .check_scale(x)
  .check_frame(x, frame)
  fr <- .nav_levels(x)
  i <- match(frame, fr)
  if (is.null(to)) {
    if (i == 1L) {
      .stop(
        "`%s` is the coarsest %s; it has no parents", frame,
        scale_vocab(x)$frame
      )
    }
    to <- fr[i - 1L]
  }
  .check_frame(x, to, "to")
  .related(x, frame, unit, to)
}

#' @rdname scale_navigate
#' @export
scale_descendants <- function(x, frame, unit, to = NULL) {
  .check_scale(x)
  .check_frame(x, frame)
  fr <- .nav_levels(x)
  i <- match(frame, fr)
  targets <- if (is.null(to)) {
    utils::tail(fr, length(fr) - i)
  } else {
    .check_frame(x, to, "to")
    to
  }
  .related_df(x, frame, unit, targets)
}

#' @rdname scale_navigate
#' @export
scale_ancestors <- function(x, frame, unit, to = NULL) {
  .check_scale(x)
  .check_frame(x, frame)
  fr <- .nav_levels(x)
  i <- match(frame, fr)
  targets <- if (is.null(to)) {
    utils::head(fr, i - 1L)
  } else {
    .check_frame(x, to, "to")
    to
  }
  .related_df(x, frame, unit, targets)
}

#' Related codes across several frames, as a frame-tagged table
#' @noRd
.related_df <- function(x, frame, unit, targets) {
  if (length(targets) == 0L) {
    return(data.frame(
      frame = character(), unit = character(),
      stringsAsFactors = FALSE
    ))
  }
  parts <- lapply(targets, function(t) {
    codes <- .related(x, frame, unit, t)
    data.frame(
      frame = rep(t, length(codes)), unit = codes,
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, parts)
  rownames(out) <- NULL
  out
}

#' Codes at `to` that share at least one atom with `unit` at `frame`
#' @noRd
.related <- function(x, frame, unit, to) {
  leaves <- S7::prop(x, "leaftable")
  unknown <- setdiff(unit, scale_units(x, frame))
  if (length(unknown) > 0L) {
    .stop(
      "code(s) not found at %s `%s`: %s", scale_vocab(x)$frame, frame,
      .preview(unknown)
    )
  }
  hit <- leaves[[frame]] %in% unit
  out <- unique(stats::na.omit(as.character(leaves[[to]][hit])))
  ord <- scale_units(x, to)
  out[order(match(out, ord))]
}

#' Coverage of a sampled DiscreteScale
#'
#' The fraction of the root parent's weight that survives in this object. A
#' scale that was never subset covers all of itself, so every weight reports 1.
#'
#' @param x A [`DiscreteScale`].
#' @param weight A single weight name, or `NULL` for all declared weights.
#'
#' @return A named numeric over the declared weights, or a single unnamed
#'   numeric when `weight` is given.
#'
#' @examples
#' scale_coverage(scale_example())
#' @export
scale_coverage <- function(x, weight = NULL) {
  .check_scale(x)
  wts <- scale_weights(x)
  full <- stats::setNames(rep(1, length(wts)), wts)
  cov <- S7::prop(x, "meta")[["coverage"]]
  if (!is.null(cov)) full[names(cov)] <- unname(cov)
  if (is.null(weight)) {
    return(full)
  }
  if (!weight %in% wts) {
    .stop("unknown weight `%s`; declared: %s", weight, .preview(wts))
  }
  unname(full[[weight]])
}
