# =============================================================================
# Backend handling for the conversion verbs
# =============================================================================
# The converters are written on dplyr verbs only, so the SAME pipeline runs on
# an in-memory data.frame/tibble, a data.table (through dtplyr), and an arrow
# Dataset/query. Contract:
#
#   data.frame  -> data.frame          (computed)
#   tibble      -> tibble              (computed)
#   data.table  -> data.table          (computed)
#   dtplyr/arrow (lazy) -> the UNCOLLECTED query, unless collect = TRUE
#
# The scale side of every join (crosswalks, atom attributes) is a small
# in-memory frame, so lazy inputs never have to be materialised for the scale
# arithmetic; only cheap aggregates are collected eagerly.
#
# Two discipline rules hold throughout the engine:
#   1. Bare `rlang::sym()`, never `.data[[...]]` -- dtplyr and arrow both
#      mistranslate pronoun subsetting where plain symbols work. Working
#      columns are declared in `utils::globalVariables()`.
#   2. Only cheap aggregates are pulled eagerly, via `.ms_pull()`.
#
# These helpers are exported (though marked internal) so the dimension
# packages built on this engine can use them without `:::`.
# =============================================================================

# dtplyr generates data.table syntax evaluated with THIS package as the calling
# namespace; without this flag data.table's cedta() check makes `[.data.table`
# fall through to `[.data.frame` and the joins break.
.datatable.aware <- TRUE

#' Which backend does `x` belong to?
#'
#' @param x A data object.
#' @return A single string naming the backend, or `NA_character_` when `x` is
#'   not a supported table.
#' @keywords internal
#' @export
.ms_backend <- function(x) {
  if (inherits(x, c("arrow_dplyr_query", "ArrowObject", "Dataset",
                    "ArrowTabular", "RecordBatchReader"))) {
    return("arrow")
  }
  if (inherits(x, "dtplyr_step")) return("dtplyr")
  if (inherits(x, "data.table")) return("data.table")
  if (inherits(x, "tbl_df")) return("tibble")
  if (is.data.frame(x)) return("data.frame")
  NA_character_
}

#' Is this backend lazy (query-producing)?
#'
#' @param backend A backend name from [`.ms_backend()`].
#' @return `TRUE` for the query-producing backends.
#' @keywords internal
#' @export
.ms_is_lazy <- function(backend) backend %in% c("arrow", "dtplyr")

#' Lift `x` into a dplyr-compatible carrier for the pipeline
#'
#' @param x A data object.
#' @param backend A backend name from [`.ms_backend()`].
#' @return `x`, wrapped where the backend needs it.
#' @keywords internal
#' @export
.ms_lazy <- function(x, backend) {
  if (backend == "data.table") {
    if (!requireNamespace("dtplyr", quietly = TRUE)) {
      # dplyr verbs work on a bare data.table too (it is a data.frame);
      # dtplyr just makes them translate to data.table code
      return(x)
    }
    return(dtplyr::lazy_dt(x))
  }
  x
}

#' A zero-row, correctly typed frame describing `x`'s columns
#'
#' @param x A data object on any supported backend.
#' @return A zero-row `data.frame`.
#' @keywords internal
#' @export
.ms_schema <- function(x) {
  as.data.frame(dplyr::collect(utils::head(x, 0L)))
}

#' Cheap eager evaluation of a small aggregate over any backend
#'
#' @param q A query or table.
#' @return A `data.frame`.
#' @keywords internal
#' @export
.ms_pull <- function(q) {
  as.data.frame(dplyr::collect(q))
}

#' Return `out` (a pipeline result over `x`) in `x`'s own format
#'
#' Lazy inputs stay lazy unless `collect = TRUE`; eager inputs are always
#' computed back to their class.
#'
#' @param out The pipeline result.
#' @param backend The input's backend name from [`.ms_backend()`].
#' @param collect Force materialisation of a lazy result.
#' @return `out`, in the input's class.
#' @keywords internal
#' @export
.ms_restore <- function(out, backend, collect = NULL) {
  lazy <- .ms_is_lazy(backend)
  if (lazy && !isTRUE(collect)) {
    return(out)
  }
  res <- dplyr::collect(out)
  switch(backend,
    "data.table" = if (requireNamespace("data.table", quietly = TRUE)) {
      data.table::as.data.table(res)
    } else as.data.frame(res),
    "tibble" = if (requireNamespace("tibble", quietly = TRUE)) {
      tibble::as_tibble(res)
    } else as.data.frame(res),
    "arrow" = res,
    "dtplyr" = if (requireNamespace("data.table", quietly = TRUE)) {
      data.table::as.data.table(res)
    } else as.data.frame(res),
    as.data.frame(res)
  )
}

#' Bind chunk results into one frame
#'
#' Chunks come out of the same `summarise()`, so their schemas match by
#' construction. `fill = FALSE` is deliberate: a mismatch means something is
#' wrong upstream and should be an error, not a silently widened frame.
#'
#' `data.table::rbindlist()` is used when data.table is installed -- it is
#' markedly faster than the alternatives on many chunks -- and
#' `dplyr::bind_rows()` otherwise. data.table stays a Suggests dependency;
#' this is the only place it is called directly.
#'
#' @param parts List of data frames.
#' @param use_dt Use `data.table::rbindlist()`. Defaults to whether data.table
#'   is installed; an explicit value makes both branches testable.
#' @return One data frame.
#' @keywords internal
#' @export
.ms_bind_rows <- function(parts,
                          use_dt = requireNamespace("data.table",
                                                    quietly = TRUE)) {
  parts <- Filter(function(p) !is.null(p) && nrow(p) > 0L, parts)
  if (length(parts) == 0L) return(NULL)
  if (length(parts) == 1L) return(as.data.frame(parts[[1L]]))
  if (isTRUE(use_dt)) {
    return(as.data.frame(
      data.table::rbindlist(parts, use.names = TRUE, fill = FALSE)))
  }
  as.data.frame(dplyr::bind_rows(parts))
}

#' Split a set of values into chunks of at most `size`
#' @noRd
.ms_chunks <- function(values, size) {
  n <- length(values)
  if (n == 0L) return(list())
  size <- max(1L, as.integer(size))
  split(values, ceiling(seq_len(n) / size))
}

#' Resolve and validate the backend of a data argument
#'
#' The one place the "unsupported input" message is written.
#'
#' @param x A data object.
#' @param arg Argument name used in the error message.
#' @return The backend name.
#' @keywords internal
#' @export
.ms_require_backend <- function(x, arg = "x") {
  backend <- .ms_backend(x)
  if (is.na(backend)) {
    .stop(paste0("`%s` must be a data.frame, tibble, data.table, dtplyr step ",
                 "or arrow table/query"), arg)
  }
  backend
}
