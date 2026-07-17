test_that("`plot.see_dw_groupmeans_list()` works with single selection", {
  x <- datawizard::means_by_group(iris, select = "Sepal.Length", by = "Species")
  expect_s3_class(plot(x), "gg")
})

test_that("`plot.see_dw_groupmeans_list()` works with multiple tables", {
  x <- datawizard::means_by_group(
    iris,
    by = "Species",
    select = starts_with("Sepal")
  )
  expect_s3_class(plot(x), "gg")
})
