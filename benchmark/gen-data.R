# =========================================================================== #
# Synthetic data for the benchmarks.
#
# Two scales built from scratch -- no dependency on timescales or geoscales,
# so the harness cannot be broken by their churn. The shapes mirror the real
# motivating case: an hourly year crossed with a European region hierarchy.
# =========================================================================== #

suppressMessages(library(multiscales))

BENCH_SCALE <- Sys.getenv("MULTISCALES_BENCH_SCALE", "small")
BENCH_DIR   <- file.path("benchmark", "_data")

.sizes <- list(
  small  = list(slices = 24 * 7, regions = 50,   years = 2),
  medium = list(slices = 24 * 90, regions = 400,  years = 2),
  large  = list(slices = 8760,    regions = 1477, years = 2)
)
if (!BENCH_SCALE %in% names(.sizes)) {
  stop("MULTISCALES_BENCH_SCALE must be one of: ",
       paste(names(.sizes), collapse = ", "))
}
SZ <- .sizes[[BENCH_SCALE]]

# A time-like scale: slice -> day -> month-ish block -----------------------
bench_clock <- function(n_slices) {
  hour <- seq_len(n_slices)
  day  <- ((hour - 1L) %/% 24L) + 1L
  blk  <- ((day - 1L) %/% 30L) + 1L
  scale_from_leaftable(
    data.frame(block = sprintf("b%02d", blk),
               day   = sprintf("d%03d", day),
               slice = sprintf("h%05d", hour),
               span  = 1,
               stringsAsFactors = FALSE),
    frames = c("block", "day", "slice"), key = "slice",
    weights = "span", name = "clock")
}

# A space-like scale: zone -> country -> market ----------------------------
bench_grid <- function(n_zones) {
  n_country <- max(2L, as.integer(round(n_zones / 36)))
  country <- ((seq_len(n_zones) - 1L) %% n_country) + 1L
  scale_from_leaftable(
    data.frame(market  = "EU",
               country = sprintf("c%03d", country),
               zone    = sprintf("z%05d", seq_len(n_zones)),
               pop     = as.numeric(1 + (seq_len(n_zones) %% 17)),
               stringsAsFactors = FALSE),
    frames = c("market", "country", "zone"), key = "zone",
    weights = "pop", name = "grid")
}

bench_scales <- function(sz = SZ) {
  list(clock = bench_clock(sz$slices), grid = bench_grid(sz$regions))
}

# The data: one row per (slice, zone, year) --------------------------------
bench_write <- function(sz = SZ, dir = BENCH_DIR) {
  if (!requireNamespace("arrow", quietly = TRUE)) {
    stop("the benchmark data set needs the arrow package")
  }
  sc <- bench_scales(sz)
  slices <- scale_units(sc$clock)
  zones  <- scale_units(sc$grid)
  unlink(dir, recursive = TRUE)
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)

  set.seed(1)
  for (yr in seq_len(sz$years)) {
    d <- expand.grid(slice = slices, zone = zones,
                     stringsAsFactors = FALSE)
    d$year <- yr
    d$load_mw <- round(stats::runif(nrow(d), 50, 150), 3)
    # One file per year, partitioned so a filtered read can prune.
    arrow::write_dataset(d, path = dir, format = "feather",
                         partitioning = "year")
    rm(d); gc(FALSE)
  }
  invisible(dir)
}

bench_open <- function(dir = BENCH_DIR) {
  arrow::open_dataset(dir, format = "feather")
}

if (sys.nframe() == 0L) {
  message("generating '", BENCH_SCALE, "' benchmark data ...")
  bench_write()
  ds <- bench_open()
  message("rows: ", format(nrow(ds), big.mark = ","), " in ", BENCH_DIR)
}
