# The export surface must not collide with packages users attach alongside it.
#
# This is the test that would have caught both of the collisions this file was
# written for: `Scale` against ggplot2's ggproto base class, and `recast`
# against the generic timescales owns and geoscales extends. It also caught a
# third, introduced while fixing them -- ggplot2 exports `is_scale` too.

test_that("no export collides with a package users attach alongside", {
  ours <- getNamespaceExports("modelscales")
  for (pkg in c("ggplot2", "scales", "timescales", "geoscales")) {
    skip_if_not_installed(pkg)
    expect_equal(intersect(ours, getNamespaceExports(pkg)), character(),
                 info = pkg)
  }
})

test_that("the classes are deliberately NOT exported", {
  # `Scale` would mask ggplot2::Scale, and a bare `recast` would mask the
  # generic timescales owns. Reached through scale_class() and the verbs.
  ours <- getNamespaceExports("modelscales")
  expect_false("Scale" %in% ours)
  expect_false("ScaleProduct" %in% ours)
  expect_false("recast" %in% ours)
  expect_true(all(c("scale_class", "scale_is", "recast_scale") %in% ours))
})

test_that("scale_is / scale_is_product answer what an object is", {
  s <- scale_example()
  p <- scale_product(a = scale_example(), b = scale_example2())
  expect_true(scale_is(s))
  expect_false(scale_is(p))          # a product holds scales but is not one
  expect_false(scale_is(data.frame(unit = "U1")))
  expect_false(scale_is(NULL))
  expect_true(scale_is_product(p))
  expect_false(scale_is_product(s))
})

test_that("scale_class() is the class, so subclassing still works", {
  expect_true(S7::S7_inherits(scale_example(), scale_class()))
  expect_true(S7::S7_inherits(
    scale_product(a = scale_example(), b = scale_example2()),
    scale_product_class()))
  Child <- S7::new_class("Child", parent = scale_class())
  k <- Child(leaftable = scale_leaftable(scale_example()),
             frames = scale_frames(scale_example()),
             members = S7::prop(scale_example(), "members"))
  expect_true(scale_is(k))
})

test_that("base methods still dispatch with the class unexported", {
  # S3method() entries are NAMESPACE directives, not exports, so print/names/
  # as.data.frame/`[` are unaffected by dropping export(Scale).
  s <- scale_example()
  expect_output(print(s))
  expect_type(names(s), "character")
  expect_s3_class(as.data.frame(s), "data.frame")
  f <- scale_frames(s)[1]
  expect_true(scale_is(s[f, scale_units(s, f)[1]]))
})
