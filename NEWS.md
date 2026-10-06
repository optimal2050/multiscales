# multiscales 0.0.0.9000

## Clustering

* `clusterscales` is merged in. It was an experiment, never published, and
  existed only because this package did -- its whole premise is that a
  clustering IS scale construction. `cluster_scale()`, `cluster_contiguous()`,
  `cluster_medoids()`, `cluster_sweep()`, `scale_distance()` and its registry
  (`register_scale_distance()` and friends, `SCALE_DISTANCES`) are now exported
  here, unchanged.
* It brings no new hard dependency -- its imports were already this package's
  -- and one new optional one, `cluster`, for `pam`.
* Keeping them apart had a cost beyond the extra repo: S7 class identity is
  per-package, so a `Scale` built here and checked in the other package failed
  `is this a Scale` whenever the two installs were out of step. One namespace
  makes that impossible rather than merely unlikely.

## The export surface

* New `recast_crosswalk()`: the recast engine over a crosswalk built
  elsewhere, for dimension packages whose conversion is not read off one
  scale -- timescales maps calendars onto each other through a datetime grid.
  Same rules, backends and missing-source handling as `recast_scale()`.
* Navigation (`scale_children()` and friends), `prune_scale()` and
  `cluster_scale()` accept the key column as a level when it holds the atoms
  (a `Calendar`'s timeslices). Before, they failed with a missing-value error.
* The `"copy"` constancy error names the target in the scale's own words
  ("timeslice" for a `Calendar`).
* `join_scale()`'s `frames` argument is now `attach`: `frame` is the level the
  codes are at, `attach` the coarser frames to add as columns.
* New vignette, "Writing a dimension package": defining a `Scale` subclass,
  its vocabulary, per-atom payloads, and registering methods from a package.
* The S7 classes are no longer exported. `Scale` collided with
  `ggplot2::Scale`, which every plotting user attaches, and a bare `recast`
  generic collided with the one `timescales` owns and `geoscales` extends --
  a second, unrelated generic of the same name would have masked theirs, and
  this one has no `Calendar` method.
* Reach them through `scale_class()` and `scale_product_class()`, which is what
  subclassing needs (`S7::new_class(..., parent = scale_class())`, the point of
  `scale_alias_property()`). Ask what an object is with `scale_is()` and
  `scale_is_product()`. Named with the object prefix because `ggplot2` exports
  `is_scale()` as well.
* `recast_scale()` and `recast_product()` are unchanged and remain the verbs.
  `print()`, `names()`, `as.data.frame()` and `[` still dispatch: `S3method()`
  entries are NAMESPACE directives, not exports.
* `tests/testthat/test-exports.R` asserts no export collides with `ggplot2`,
  `scales`, `timescales` or `geoscales`, so this cannot come back.

The dimension-agnostic engine shared by `timescales` and `geoscales`.

## The conversion engine

* `recast_scale()` converts values between resolutions in one call, in either
  direction: aggregation and disaggregation are the same operation, and the
  direction falls out of the crosswalk. Rules `sum`, `weighted_mean`, `mean`,
  `copy`, `sd`, `share` and `logshare` (`SCALE_RULES`), scalar or one per
  value column. Frames that cross-cut work, because the route always goes
  through the atoms.
* `recast_to_atoms()` / `recast_from_atoms()` are the two halves of that
  route; their composition IS `recast_scale()`, and going out through one
  scale's atoms and back in through another's recasts across objects.
* `recast()` is the bare S7 generic, so pipelines chain across dimensions.
* `scale_map()` materialises the crosswalk. `scale_atom_pairs()` is the seam a
  dimension overrides when its atom layer is generated rather than enumerated
  (time), including extra identifier columns carried through `by=`.
* `register_scale_map()` installs exact, hand-audited crosswalks;
  `register_scale_rule()` records how a named parameter is recast. Both are
  scoped, so the same name means different things in different dimensions
  without collision. A value column with neither an explicit rule nor a
  registry entry is an error, never a guess.
* `join_scale()` attaches a scale's labels, memberships, share and weight,
  all named after the scale -- so several scales live side by side on one
  dataset, which is what makes that dataset a crosswalk between them.
* Every verb runs on `data.frame`, tibble, `data.table`, dtplyr and arrow;
  results come back in the input's class and lazy inputs stay lazy unless
  `collect = TRUE`.

## Residuals and reconciliation

Hierarchical data does not add up, for two different reasons that need
different answers.

* A **structural residual** is a real component belonging to the parent and to
  none of the children: non-regionalized GDP, taxes allocable to no industry,
  an "n.e.c." bucket. Declare such units per frame --
  `scale_from_leaftable(..., residuals = list(unit = "DE_XR"))` -- and read
  them back with `scale_residuals()`.
* A declared residual **aggregates upward like any other unit**, which is the
  point: it is what makes the parent total reconcile. But `recast_scale()`
  now never splits a coarse figure INTO one. The split shares are taken over
  the allocable units and renormalised, so protecting the residual does not
  quietly lose its share of the parent total.
* That closes a silent trap. A residual with positive weight, or a scale with
  no declared weights at all (where every atom weighs 1), previously received
  an allocated share. Declaring residuals is opt-in, so no existing result
  changes.
* A group whose targets are all residuals has nowhere allocable to put the
  parent's value, and is now an error rather than a silent zero.

* `scale_reconcile()` answers the other question -- every child is present and
  they still do not sum to the published parent figure. It aggregates the fine
  data and compares it with an independent total, reporting `aggregated`,
  `target`, `gap`, `rel_gap` and an `ok` flag per parent group, identifier
  combination and value column. `balance = "residual"` parks the gap on a
  declared residual; `balance = "proportional"` scales the children to hit the
  target; either way the gap table travels back as the `"reconciliation"`
  attribute. The default changes nothing.
* This discrepancy is deliberately NOT stored on the scale. It varies by
  variable, period and vintage, so a number parked on the scale would go stale
  without saying so. `meta$coverage` remains the separate matter of a scale
  deliberately holding a subset: eleven months of twelve is coverage, twelve
  months that fail to add up is a discrepancy.

## Large and on-disk data

* `recast_scale()` gains `diagnostics = c("auto", "on", "off")`. The checks
  that need a scan of the data -- unknown source codes, dropped shares,
  source units missing from the data -- now run on eager inputs and are
  skipped on arrow/dtplyr, where a scan defeats the point of returning a
  query. Errors that do not depend on the data are raised regardless.
* **A lazy input no longer triggers eager work.** Building a query over an
  arrow table previously performed two full scans and materialised the
  distinct (identifier, key) pairs -- potentially approaching the size of the
  data itself -- purely to produce a warning, before returning the
  uncollected query. Everything that scans or materialises now happens after
  the lazy return.
* **The identifier cross join is gone.** The engine used to build
  `cross_join(identifier combinations, crosswalk)` as an in-memory
  `data.frame` -- for the flagship shape that is millions of rows, roughly
  the size of the source data -- purely so a right join would inject `NA`
  rows for absent (identifier, source) pairs. It now joins the data against
  the crosswalk itself and reproduces that effect from the aggregate: a
  count of the distinct sources each group saw, compared against what the
  crosswalk expects. The count rides the summarise that was already
  happening, so it costs no extra pass, and the results are unchanged.
* `missing_sources = c("na", "ignore")` makes that behaviour explicit.
  `"na"` (the default, and what the engine has always done) treats a
  partially-supplied target as unanswerable; `"ignore"` aggregates what is
  present, which is faster but turns incomplete data into a plausible
  number, so you have to ask for it.
* **One pass per call, whatever the weights.** Value columns using different
  weight columns used to cost a full pass over the data each, plus a join of
  the large intermediates. The crosswalks for different weights differ only
  in the weight itself, so they are now folded into one table carrying each
  weight's split factor side by side.
* The `copy` constancy guard no longer runs its own grouped pass over the
  data before the lazy return; its per-group min and max ride the main
  summarise.
* `join_scale()` is now zero-scan on a lazy input, and its two checks are
  filtered queries rather than a full distinct scan: the match test asks for
  one row and stops, and the unknown-code query returns nothing at all on
  healthy data.
* `recast_scale(batch =, batch_by =)` processes a job in chunks of identifier
  values and binds the pieces with `data.table::rbindlist()` when data.table
  is installed (it stays a Suggests dependency). Chunking by an identifier
  partitions the aggregation groups exactly, so the numbers are unchanged;
  any other column is refused rather than silently producing wrong results.
* The per-group NA law is now stated as a test in its own right
  (`test-poisoning-law.R`): when a target's sources are only partially
  present in the data, its result is `NA` rather than a partial answer, for
  every rule and on every backend.
* A `benchmark/` harness measures time and three separate memory figures
  (R heap, Arrow pool, OS RSS) across the verbs and switches, and checks
  conservation in every case so a configuration that is fast because it is
  wrong cannot look like a win.

## Products of scales

* `scale_product()` combines two or more scales into one multi-index --
  time x space, time x space x industry. The atoms of a product stay LAZY:
  nothing is built at construction, and `product_atoms()` is the guarded way
  to realise the grid. `product_size()`, `product_keys()`, `product_frames()`,
  `product_weights()` and `product_coverage()` answer everything else without
  touching a row.
* Data on a product carries one key column per axis, never a pasted composite
  key -- which is what lets each axis's key ride through the other axes'
  passes as an ordinary identifier column.
* `recast_product()` converts any subset of the axes in one call, with a rule
  per (value column, axis): `rules = list(load = c(time = "weighted_mean",
  space = "sum"))`. `NULL` resolves each column on each axis through that
  dimension's own registry scope, which is where an intensive/extensive
  asymmetry between dimensions belongs. `recast()` dispatches on a product
  too.
* Execution is a SEQUENCE of ordinary single-axis recasts, ordered so the
  axis that reduces the data most runs first. For `sum`, `mean`,
  `weighted_mean` and `copy` that is provably identical to a joint recast in
  either order, so nothing new computes the numbers and the product crosswalk
  is never built. The test suite states this as a law over every rule pair.
* A per-axis `"sd"` is refused: the sd of aggregated totals and the total of
  per-group sds are different quantities, and the error names both explicit
  pipelines. A scalar `"sd"` means the pooled sd over all product atoms in a
  target block, which is well defined -- computed by chaining the small
  per-axis crosswalks, again without materialising the product.
* `join_product()` and `filter_product()` are the componentwise verbs.
* `joint_weights` is reserved on the class for a weight that varies jointly
  across axes; its shape is validated but not yet consumed.

## Structure

* `filter_scale()` (and `x[frame, unit]`) subsets, `prune_scale()` collapses
  to a coarser atom layer. Both book-keep the result as a SAMPLE: coverage is
  recorded against the ROOT parent so filters compose, and the name is mangled
  so a sample can never impersonate its parent.
* `scale_ancestry()`, `scale_children()`, `scale_parents()`,
  `scale_descendants()`, `scale_ancestors()` and `scale_share()` complete the
  navigation surface. Ancestry is computed atom-mediated, never as a
  transitive closure -- closing a cross-cutting hierarchy invents
  relationships that are not there.
* `scale_payload_slice()` is the seam a subclass overrides to subset whatever
  it keeps per atom (geometry, in space).

## Verified against the dimension it generalises

`tests/testthat/test-differential.R` builds a `Scale` from
`geoscales::geoscale_example()`'s own leaftable and runs every operation
through both engines, demanding identical results -- no hand-written expected
values. Every rule, both directions, the route halves, the crosswalk, join,
filter, prune, share, ancestry and navigation agree exactly.

## The class

* `Scale` (S7): a nested partition of atoms in any dimension --
  `leaftable` + `frames` (coarsest first) + `members` + `key` + `meta`.
  Generalises the two sibling classes: named plural weights, `NA`-tolerant
  memberships (partial coverage is a first-class state), no nesting
  assumption, and the sample-bookkeeping triple validated against the
  leaftable.
* `scale_from_leaftable()` and `scale_example()` build one; `scale_frames()`,
  `scale_units()`, `scale_rank()`, `scale_key()`, `scale_leaftable()`,
  `scale_weights()` read one; `print()`, `format()`, `summary()`, `names()`
  and `as.data.frame()` are the base generics.
* `scale_family()`, `scale_nests()` and `scale_coverage()` answer the
  structure questions. Nesting is a diagnostic, never a requirement.
* `scale_vocab()` supplies the words a dimension uses, interpolated into
  shared error messages so a `Calendar` flowing through this engine still
  says "timeframe".
* `scale_alias_property()` builds the getter/setter used by a subclass to
  keep its own name for an inherited property (`@timeframes`, `@geoframes`).
* The backend quintet (`.ms_backend()` and friends) is exported for the
  packages built on this engine: `data.frame`/tibble/`data.table`/dtplyr/
  arrow in, the same class out, lazy inputs staying lazy unless
  `collect = TRUE`.
