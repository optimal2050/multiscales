# =========================================================================== #
# The benchmark grid.
#
# Every case carries a conservation check, so a configuration that is fast
# because it computes the wrong thing shows up as ok = FALSE rather than as a
# win.
# =========================================================================== #

source(file.path("benchmark", "gen-data.R"))
source(file.path("benchmark", "measure.R"))

if (!dir.exists(BENCH_DIR)) {
  message("no benchmark data yet -- generating '", BENCH_SCALE, "'")
  bench_write()
}

sc <- bench_scales()
clock <- sc$clock
grid  <- sc$grid
prod  <- scale_product(time = clock, space = grid, name = "bench")

ds <- bench_open()
total_rows <- bench_nrow(ds)
message("benchmarking '", BENCH_SCALE, "' -- ",
        format(total_rows, big.mark = ","), " rows")

# The reference total, computed once, for the conservation checks.
ref_total <- ds |>
  dplyr::summarise(s = sum(load_mw)) |>
  dplyr::collect() |>
  getElement("s")

.conserves <- function(x) {
  got <- sum(as.data.frame(x)$load_mw, na.rm = TRUE)
  isTRUE(all.equal(got, ref_total, tolerance = 1e-6))
}
.is_lazy <- function(x) !is.data.frame(x)

rows <- list()
add <- function(...) rows[[length(rows) + 1L]] <<- bench_case(...)

# -- the lazy contract: building a query must not touch the data ------------
add("recast_scale lazy (query only)",
    recast_scale(ds, clock, from = "slice", to = "day", key = "slice",
                 values = "load_mw", rule = "sum"),
    check = .is_lazy)

add("join_scale lazy (should be zero-scan)",
    join_scale(ds, grid, key = "zone", frame = "zone", frames = TRUE),
    check = .is_lazy)

# -- the real work ----------------------------------------------------------
add("recast_scale time: slice -> day",
    recast_scale(ds, clock, from = "slice", to = "day", key = "slice",
                 values = "load_mw", rule = "sum", collect = TRUE),
    check = .conserves)

add("recast_scale space: zone -> country",
    recast_scale(ds, grid, from = "zone", to = "country", key = "zone",
                 values = "load_mw", rule = "sum", collect = TRUE),
    check = .conserves)

add("recast_product both axes",
    recast_product(ds, prod, to = list(time = "day", space = "country"),
                   values = "load_mw", rules = "sum", collect = TRUE),
    check = .conserves)

# -- what the switches cost -------------------------------------------------
add("diagnostics = on",
    recast_scale(ds, clock, from = "slice", to = "day", key = "slice",
                 values = "load_mw", rule = "sum", collect = TRUE,
                 diagnostics = "on"),
    check = .conserves)

add("missing_sources = ignore",
    recast_scale(ds, clock, from = "slice", to = "day", key = "slice",
                 values = "load_mw", rule = "sum", collect = TRUE,
                 missing_sources = "ignore"),
    check = .conserves)

add("batch = 1 year per chunk",
    recast_scale(ds, clock, from = "slice", to = "day", key = "slice",
                 values = "load_mw", rule = "sum", collect = TRUE,
                 batch = 1, batch_by = "year"),
    check = .conserves)

# -- an intensive quantity, which takes the weighted path -------------------
add("weighted_mean space: zone -> country",
    recast_scale(ds, grid, from = "zone", to = "country", key = "zone",
                 values = "load_mw", rule = "weighted_mean", weight = "pop",
                 collect = TRUE),
    check = function(x) all(is.finite(as.data.frame(x)$load_mw)))

tab <- do.call(rbind, rows)
tab$scale <- BENCH_SCALE
tab$rows <- total_rows
print(tab, row.names = FALSE)

if (any(!tab$ok & !is.na(tab$ok))) {
  warning("some cases FAILED their conservation check -- these are bugs, ",
          "not benchmark results")
}
bench_write_results(tab, BENCH_SCALE)
