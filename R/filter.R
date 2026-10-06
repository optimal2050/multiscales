# =============================================================================
# Subsetting and collapsing a Scale
# =============================================================================
# A genuine subset is a SAMPLE and is book-kept as one, so an object always
# knows what fraction of its parent it carries and can never impersonate that
# parent in a registry or a join column name.
# =============================================================================

#' @include scale-class.R
#' @include navigate.R
NULL

#' Sample bookkeeping: coverage / parent_totals / parent_name / name
#'
#' Coverage is always a fraction of the ROOT parent (an existing
#' `parent_totals` is reused, so filter-of-filter composes), and the mangled
#' name is built from the root parent's name plus `tag`.
#' @noRd
.sample_meta <- function(x, meta, kept, tag) {
  wts <- scale_weights(x)
  leaves <- S7::prop(x, "leaftable")
  totals <- meta$parent_totals
  if (is.null(totals)) {
    totals <- vapply(wts, function(w) sum(leaves[[w]], na.rm = TRUE),
                     numeric(1))
    names(totals) <- wts
  }
  cov <- vapply(wts, function(w) sum(kept[[w]], na.rm = TRUE) / totals[[w]],
                numeric(1))
  names(cov) <- wts
  base <- meta$parent_name %||% meta$name
  meta$parent_totals <- totals
  meta$coverage      <- cov
  meta$parent_name   <- base
  meta$name          <- paste0(base, tag)
  meta
}

#' Rebuild a Scale from a row subset
#'
#' The update is made on a copy of `x` itself, which preserves the concrete
#' class and any extra properties a subclass carries. The payload hook then
#' subsets whatever the subclass keeps per atom, and the object is validated
#' once, when everything is consistent.
#' @noRd
.rebuild <- function(x, keep, fr, drop_empty_frames = FALSE,
                     meta = S7::prop(x, "meta")) {
  leaves <- S7::prop(x, "leaftable")[keep, , drop = FALSE]
  rownames(leaves) <- NULL

  members <- lapply(fr, function(f) {
    old <- S7::prop(x, "members")[[f]]
    seen <- unique(stats::na.omit(as.character(leaves[[f]])))
    old[old %in% seen]
  })
  names(members) <- fr

  if (isTRUE(drop_empty_frames)) {
    nonempty <- fr[vapply(members[fr], length, integer(1)) > 0L]
    if (length(nonempty) == 0L) .stop("every frame would be empty")
    fr <- nonempty
    members <- members[fr]
  } else {
    empty <- fr[vapply(members[fr], length, integer(1)) == 0L]
    if (length(empty) > 0L) {
      .stop(paste0("frame(s) left with no codes: %s; pass ",
                   "drop_empty_frames = TRUE"), .preview(empty))
    }
  }

  # The core props are set unchecked: a subclass's per-atom payload still has
  # the old rows until the hook slices it, and validating in between would
  # reject a payload that is positional (one element per leaftable row).
  out <- S7::set_props(x, leaftable = leaves, frames = fr, members = members,
                       meta = meta, .check = FALSE)
  out <- scale_payload_slice(out, keep)
  S7::validate(out)
  out
}

#' Subset a Scale by unit
#'
#' Keeps only the atoms belonging to `unit` at `frame`, and rebuilds the member
#' vocabularies accordingly.
#'
#' A genuine subset is a SAMPLE and is book-kept as one: `meta$coverage`
#' records, per weight column, the kept fraction of the ROOT parent's total (so
#' filters compose against the original object), `meta$parent_totals` stores
#' those root totals (making the coverage claim verifiable by the validator),
#' `meta$parent_name` records the parent, and `meta$name` is mangled to
#' `"parent[frame:codes]"` so a sample never impersonates its parent in the
#' crosswalk registry or in [`join_scale()`] column names. A filter that keeps
#' every atom is a true no-op. Read the fraction back with [`scale_coverage()`].
#'
#' @param x A [`Scale`].
#' @param frame Frame that `unit` belongs to.
#' @param unit Character vector of codes to keep.
#' @param drop_empty_frames Drop frames left with no codes at all.
#'
#' @return A [`Scale`].
#'
#' @examples
#' s <- scale_example()
#' p <- filter_scale(s, "sector", "P")
#' p
#' scale_coverage(p)
#' @export
filter_scale <- function(x, frame, unit, drop_empty_frames = FALSE) {
  .check_scale(x)
  .check_frame(x, frame)
  leaves <- S7::prop(x, "leaftable")
  fr     <- S7::prop(x, "frames")

  unknown <- setdiff(unit, S7::prop(x, "members")[[frame]])
  if (length(unknown) > 0L) {
    .stop("code(s) not found at %s `%s`: %s", scale_vocab(x)$frame, frame,
          .preview(unknown))
  }

  keep <- which(leaves[[frame]] %in% unit)
  if (length(keep) == 0L) {
    .stop("no atoms remain after filtering")
  }

  meta <- S7::prop(x, "meta")
  if (length(keep) < nrow(leaves)) {          # a real sample, not a no-op
    codes <- unique(as.character(leaves[[frame]][keep]))
    # the tag must IDENTIFY the sample, not just count it -- two different
    # single-unit samples may not share a name
    id <- if (sum(nchar(codes)) + length(codes) <= 24L) {
      paste(codes, collapse = "+")
    } else {
      paste0(length(codes), "~", substr(rlang::hash(sort(codes)), 1, 8))
    }
    meta <- .sample_meta(x, meta, kept = leaves[keep, , drop = FALSE],
                         tag = sprintf("[%s:%s]", frame, id))
  }
  .rebuild(x, keep, fr, drop_empty_frames, meta = meta)
}

#' Subset a Scale
#'
#' `x[frame, unit]` is [`filter_scale()`].
#'
#' @param x A [`Scale`].
#' @param i Frame name.
#' @param j Unit codes to keep.
#' @param ... Unused.
#' @return A [`Scale`].
#' @examples
#' scale_example()["sector", "P"]
#' @export
#' @method [ Scale
`[.Scale` <- function(x, i, j, ...) {
  if (missing(i) || missing(j)) {
    .stop("subset a scale as `x[frame, unit]`")
  }
  filter_scale(x, i, j)
}

# The fully-qualified spelling is what `class()` reports for an installed
# package, so without this alias `x[frame, unit]` falls through to
# `[.S7_object`, which errors.
#' @rdname sub-.Scale
#' @export
`[.multiscales::Scale` <- `[.Scale`

#' Collapse a Scale to a coarser frame
#'
#' Returns a new [`Scale`] whose atom layer is `frame`, dropping every finer
#' frame. Weights are summed over the collapsed atoms.
#'
#' The result is renamed `"name@frame"` with the parent recorded in
#' `meta$parent_name`; every other meta field is preserved. Atoms with no code
#' at `frame` are dropped, and that loss is reflected in `meta$coverage`.
#'
#' @param x A [`Scale`].
#' @param frame The frame to become the new atom layer.
#'
#' @return A [`Scale`].
#'
#' @examples
#' prune_scale(scale_example(), "class")
#' @export
prune_scale <- function(x, frame) {
  .check_scale(x)
  .check_frame(x, frame)
  fr <- S7::prop(x, "frames")
  # The key as atom level is finer than every frame: nothing to prune.
  if (!frame %in% fr) return(x)
  keep_fr <- fr[seq_len(match(frame, fr))]

  leaves0 <- S7::prop(x, "leaftable")
  covered <- !is.na(leaves0[[frame]])
  leaves  <- leaves0[covered, , drop = FALSE]
  if (nrow(leaves) == 0L) {
    .stop("no atoms have a code at %s `%s`", scale_vocab(x)$frame, frame)
  }

  wts <- scale_weights(x)
  grp <- leaves[, keep_fr, drop = FALSE]
  gkey <- do.call(paste, c(unname(as.list(grp)), sep = "\r"))
  idx <- !duplicated(gkey)

  out <- grp[idx, , drop = FALSE]
  for (w in wts) {
    totals <- tapply(leaves[[w]], gkey, sum, na.rm = TRUE)
    out[[w]] <- as.numeric(totals[gkey[idx]])
  }
  akey <- scale_key(x)
  out[[akey]] <- as.character(out[[frame]])
  rownames(out) <- NULL

  # meta: preserve EVERYTHING, then adjust identity and coverage
  meta <- S7::prop(x, "meta")
  new_meta <- meta
  if (!all(covered)) {                    # NA atoms dropped = coverage loss
    new_meta <- .sample_meta(x, new_meta, kept = leaves, tag = "")
  }
  new_meta$parent_name <- meta$name
  new_meta$name <- paste0(meta$name, "@", frame)

  s <- scale_from_leaftable(
    out, frames = keep_fr, key = akey, weights = wts,
    default_weight = meta$default_weight,
    name = new_meta$name, desc = meta$desc %||% "")
  full_meta <- utils::modifyList(new_meta, S7::prop(s, "meta")[
    c("weights", "default_weight")])
  S7::prop(s, "meta") <- full_meta
  s
}

#' Weight shares within a frame
#'
#' Normalised weights, either of the whole object or within each parent group.
#'
#' @param x A [`Scale`].
#' @param frame Frame to report shares for.
#' @param weight Weight column. `NULL` uses the default.
#' @param within Optional coarser frame to normalise within. `NULL` normalises
#'   over the whole object.
#'
#' @return A `data.frame` with a code column named `frame`, the weight, and
#'   `share`. When `within` is given, a column of that name carries the parent
#'   code.
#'
#' @examples
#' s <- scale_example()
#' scale_share(s, "class", weight = "size")
#' scale_share(s, "class", weight = "size", within = "sector")
#' @export
scale_share <- function(x, frame, weight = NULL, within = NULL) {
  .check_scale(x)
  .check_frame(x, frame)
  weight <- .resolve_weight(x, weight)
  leaves <- S7::prop(x, "leaftable")

  d <- data.frame(unit = as.character(leaves[[frame]]),
                  w = as.numeric(leaves[[weight]]),
                  stringsAsFactors = FALSE)
  if (is.null(within)) {
    d$grp <- ""
  } else {
    .check_frame(x, within, "within")
    d$grp <- as.character(leaves[[within]])
  }
  d <- d[!is.na(d$unit), , drop = FALSE]
  d$w[is.na(d$w)] <- 0

  agg <- stats::aggregate(w ~ unit + grp, data = d, FUN = sum)
  agg$share <- agg$w / stats::ave(agg$w, agg$grp, FUN = sum)

  ord <- S7::prop(x, "members")[[frame]]
  agg <- agg[order(match(agg$unit, ord)), , drop = FALSE]

  out <- data.frame(code = agg$unit, stringsAsFactors = FALSE)
  names(out) <- frame
  if (!is.null(within)) out[[within]] <- agg$grp
  out[[weight]] <- agg$w
  out$share <- agg$share
  rownames(out) <- NULL
  out
}
