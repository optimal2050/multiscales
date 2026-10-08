# =============================================================================
# The scale-aware dataset store
# =============================================================================
# A folder that reopens as DATA ON SCALES, not as a bare table. The data is a
# hive-partitioned Arrow IPC dataset -- lazily readable, and already a
# first-class input to every verb here (see R/backend.R) -- and the scales
# that index it travel with it in a sidecar.
#
#   path/
#     data/                 hive-partitioned .arrow files
#       year=2021/part-0.arrow
#     _scales/
#       manifest.yaml       what the scales are: axes, keys, frames, meta
#       <axis>-leaftable.arrow
#
# The leaftables are stored as Arrow so they stay readable from any language;
# the structural metadata is YAML for the same reason. A scale whose `meta`
# holds objects YAML cannot represent (an sf CRS, a function) is refused at
# write time rather than silently half-saved.
# =============================================================================

#' @include product-class.R
NULL

#' @noRd
.need_arrow <- function(what) {
  if (!requireNamespace("arrow", quietly = TRUE)) {
    .stop("%s needs the arrow package", what)
  }
}

#' Compression codec for an IPC dataset
#'
#' `write_feather()` takes a compression NAME, while
#' `write_dataset(format = "feather")` takes a `Codec` OBJECT as `codec` and
#' errors on the string. Falls back to no compression when the codec is not
#' available in this arrow build.
#' @noRd
.ipc_codec <- function(compression) {
  cmp <- tolower(compression)
  if (identical(cmp, "uncompressed") || identical(cmp, "none")) {
    return(NULL)
  }
  if (identical(cmp, "lz4")) cmp <- "lz4_frame"
  z <- try(arrow::Codec$create(cmp), silent = TRUE)
  if (inherits(z, "try-error")) {
    .warn(
      "compression codec '%s' is unavailable; writing uncompressed",
      compression
    )
    return(NULL)
  }
  z
}

#' @noRd
.need_yaml <- function(what) {
  if (!requireNamespace("yaml", quietly = TRUE)) {
    .stop("%s needs the yaml package", what)
  }
}

#' Meta entries that survive a round trip through YAML
#' @noRd
.serialisable_meta <- function(meta, nm) {
  ok <- vapply(meta, function(z) {
    is.null(z) || is.character(z) || is.numeric(z) || is.logical(z) ||
      (is.list(z) && all(vapply(z, function(y) {
        is.character(y) || is.numeric(y) || is.logical(y)
      }, logical(1))))
  }, logical(1))
  if (!all(ok)) {
    .stop(
      paste0(
        "the `meta` of scale \"%s\" holds value(s) that cannot be ",
        "written as YAML: %s. Drop them before writing, or store ",
        "them alongside the dataset yourself."
      ),
      nm, .preview(names(meta)[!ok])
    )
  }
  meta
}

#' Describe one scale for the manifest
#' @noRd
.scale_manifest <- function(s) {
  nm <- .scale_name(s, require = FALSE)
  list(
    name    = if (nzchar(nm)) nm else NULL,
    key     = scale_key(s),
    frames  = as.list(scale_frames(s)),
    members = lapply(S7::prop(s, "members"), as.list),
    meta    = .serialisable_meta(S7::prop(s, "meta"), nm)
  )
}

#' Rebuild a scale from its manifest entry and leaftable
#' @noRd
.scale_from_manifest <- function(entry, leaftable) {
  members <- lapply(entry$members, function(z) as.character(unlist(z)))
  meta <- entry$meta %||% list()
  # YAML gives back plain lists; the validator wants atomic vectors for the
  # fields it checks.
  for (f in c("weights", "coverage", "parent_totals")) {
    if (!is.null(meta[[f]]) && is.list(meta[[f]])) {
      meta[[f]] <- unlist(meta[[f]])
    }
  }
  # `residuals` is a named list OF vectors, so each element needs flattening
  # rather than the whole field: YAML hands back list("DE_XR", "FR_XR") where
  # the validator wants c("DE_XR", "FR_XR").
  if (is.list(meta[["residuals"]])) {
    meta[["residuals"]] <- lapply(
      meta[["residuals"]],
      function(z) as.character(unlist(z))
    )
  }
  NestedScale(
    leaftable = as.data.frame(leaftable),
    frames = as.character(unlist(entry$frames)),
    members = members,
    key = entry$key,
    meta = meta
  )
}

#' Write data and the scales that index it to one folder
#'
#' The data is written as a hive-partitioned Arrow IPC dataset and the scales
#' travel with it, so [`open_scale_dataset()`] gives back both and the result
#' can be recast without re-declaring anything.
#'
#' @param data The table to write, in any supported backend.
#' @param x The [`NestedScale`] or [`ScaleProduct`] the data is indexed by.
#' @param path Directory to create. It must not already contain a store
#'   unless `overwrite = TRUE`.
#' @param partitioning Columns to partition the data by (hive style). `NULL`
#'   (default) picks nothing; a good choice is a low-cardinality identifier
#'   such as a year, which lets a filtered read skip whole files.
#' @param compression Codec for the IPC files, e.g. `"zstd"` (default),
#'   `"lz4"` or `"uncompressed"`.
#' @param overwrite Replace an existing store at `path`.
#'
#' @return `path`, invisibly.
#'
#' @examples
#' p <- scale_product(a = scale_example(), b = scale_example2())
#' d <- merge(
#'   data.frame(unit = c("U1", "U2")),
#'   data.frame(period = c("p1", "p2"))
#' )
#' d$v <- 1:4
#' dir <- file.path(tempdir(), "store")
#' if (requireNamespace("arrow", quietly = TRUE) &&
#'   requireNamespace("yaml", quietly = TRUE)) {
#'   write_scale_dataset(d, p, dir, overwrite = TRUE)
#'   ds <- open_scale_dataset(dir)
#'   ds
#' }
#' @export
write_scale_dataset <- function(data, x, path, partitioning = NULL,
                                compression = "zstd", overwrite = FALSE) {
  .need_arrow("write_scale_dataset()")
  .need_yaml("write_scale_dataset()")

  is_prod <- S7::S7_inherits(x, ScaleProduct)
  if (!is_prod) .check_scale(x, "x")

  backend <- .ms_require_backend(data, "data")
  d <- as.data.frame(dplyr::collect(.ms_lazy(data, backend)))

  axes <- if (is_prod) {
    S7::prop(x, "axes")
  } else {
    nm <- .scale_name(x, require = FALSE)
    stats::setNames(list(x), if (nzchar(nm)) nm else "scale")
  }
  keys <- if (is_prod) {
    product_keys(x)
  } else {
    stats::setNames(scale_key(x), names(axes))
  }

  missing_keys <- setdiff(unname(keys), names(d))
  if (length(missing_keys) > 0L) {
    .stop(
      "the data has no column(s) %s (one key column per axis)",
      .preview(missing_keys)
    )
  }
  if (!is.null(partitioning)) {
    bad <- setdiff(partitioning, names(d))
    if (length(bad) > 0L) {
      .stop(
        "`partitioning` names column(s) not in the data: %s",
        .preview(bad)
      )
    }
  }

  if (dir.exists(path)) {
    if (!isTRUE(overwrite)) {
      .stop("`%s` already exists; pass overwrite = TRUE to replace it", path)
    }
    unlink(path, recursive = TRUE)
  }
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  sdir <- file.path(path, "_scales")
  dir.create(sdir, showWarnings = FALSE)

  # Scales first: if one of them cannot be serialised, fail before writing
  # gigabytes of data that would then describe nothing.
  manifest <- list(
    nestedscales = as.character(utils::packageVersion("nestedscales")),
    kind = if (is_prod) "product" else "scale",
    keys = as.list(keys),
    axes = lapply(axes, .scale_manifest)
  )
  for (a in names(axes)) {
    arrow::write_feather(
      arrow::as_arrow_table(scale_leaftable(axes[[a]])),
      file.path(sdir, paste0(a, "-leaftable.arrow")),
      compression = compression
    )
  }
  yaml::write_yaml(manifest, file.path(sdir, "manifest.yaml"))

  ddir <- file.path(path, "data")
  codec <- .ipc_codec(compression)
  args <- list(d,
    path = ddir, format = "feather",
    partitioning = partitioning
  )
  if (!is.null(codec)) args$codec <- codec
  do.call(arrow::write_dataset, args)
  invisible(path)
}

#' Open a scale-aware dataset
#'
#' @param path A folder written by [`write_scale_dataset()`].
#'
#' @return A `scale_dataset`: a list with `data` (a lazily-read arrow
#'   Dataset) and `scale` (the [`NestedScale`] or [`ScaleProduct`] it is indexed
#'   by). Feed them to the verbs as
#'   `recast_scale(ds$data, ds$scale, ...)`.
#'
#' @examples
#' # see write_scale_dataset()
#' @export
open_scale_dataset <- function(path) {
  .need_arrow("open_scale_dataset()")
  .need_yaml("open_scale_dataset()")
  sdir <- file.path(path, "_scales")
  mf <- file.path(sdir, "manifest.yaml")
  if (!file.exists(mf)) {
    .stop("`%s` is not a scale dataset (no _scales/manifest.yaml)", path)
  }
  manifest <- yaml::read_yaml(mf)

  scales <- lapply(names(manifest$axes), function(a) {
    lt <- as.data.frame(arrow::read_feather(
      file.path(sdir, paste0(a, "-leaftable.arrow"))
    ))
    .scale_from_manifest(manifest$axes[[a]], lt)
  })
  names(scales) <- names(manifest$axes)

  sc <- if (identical(manifest$kind, "product")) {
    keys <- unlist(manifest$keys)
    do.call(
      scale_product,
      c(scales, list(keys = keys[names(scales)]))
    )
  } else {
    scales[[1L]]
  }

  structure(
    list(
      data = arrow::open_dataset(file.path(path, "data"),
        format = "feather"
      ),
      scale = sc,
      path = path
    ),
    class = "scale_dataset"
  )
}

#' Describe a stored dataset without opening it fully
#'
#' @param path A folder written by [`write_scale_dataset()`].
#' @return A list with `kind`, `axes`, `keys`, `files` and `bytes`.
#' @examples
#' # see write_scale_dataset()
#' @export
scale_dataset_info <- function(path) {
  .need_yaml("scale_dataset_info()")
  mf <- file.path(path, "_scales", "manifest.yaml")
  if (!file.exists(mf)) {
    .stop("`%s` is not a scale dataset (no _scales/manifest.yaml)", path)
  }
  manifest <- yaml::read_yaml(mf)
  files <- list.files(file.path(path, "data"),
    recursive = TRUE,
    full.names = TRUE
  )
  list(
    kind = manifest$kind,
    axes = names(manifest$axes),
    keys = unlist(manifest$keys),
    files = length(files),
    bytes = sum(file.info(files)$size, na.rm = TRUE)
  )
}

#' @param x A `scale_dataset`.
#' @param ... Ignored.
#' @rdname open_scale_dataset
#' @export
#' @method print scale_dataset
print.scale_dataset <- function(x, ...) {
  info <- scale_dataset_info(x$path)
  cat("<scale_dataset>", x$path, "\n")
  cat("  rows:  ", tryCatch(format(nrow(x$data), big.mark = ","),
    error = function(e) "?"
  ), "\n", sep = "")
  cat("  files: ", info$files, " (",
    format(round(info$bytes / 1024), big.mark = ","), " KB)\n",
    sep = ""
  )
  cat("  scale: ", info$kind, " over ",
    paste(info$axes, collapse = " x "), "\n",
    sep = ""
  )
  invisible(x)
}
