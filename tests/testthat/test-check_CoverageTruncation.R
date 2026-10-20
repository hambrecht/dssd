library(dssd)
library(testthat)

context("Coverage truncation tracking")

test_that("Changing the truncation after run.coverage gives a warning", {

  outer <- matrix(c(0,0,15,0,15,10,0,10,0,0),ncol=2, byrow=TRUE)
  region <- make.region(region.name = "study.area",
                        shape = sf::st_polygon(list(outer)))
  cover <- make.coverage(region, n.grid.points = 100)
  design <- make.design(region = region,
                        transect.type = "line",
                        design = "systematic",
                        spacing = 2,
                        truncation = 1,
                        coverage.grid = cover)
  set.seed(1)
  design <- run.coverage(design, reps = 3, quiet = TRUE)
  expect_equal(attr(design@design.statistics, "truncation"), 1)

  # No warning while the truncation is unchanged
  expect_warning(get.coverage(design), NA)
  expect_warning(capture.output(print(design)), NA)

  # Changing the truncation leaves the coverage out of date
  design.copy <- design
  design.copy@truncation <- 0.5
  expect_warning(get.coverage(design.copy),
                 "Coverage was computed with truncation 1 but the design truncation is now 0.5")
  expect_warning(capture.output(print(design.copy)), "re-run run.coverage")
  expect_warning(plot(design.copy), "re-run run.coverage")

  # Re-running the coverage clears the warning and updates the covered area
  design.copy <- run.coverage(design.copy, reps = 3, quiet = TRUE)
  expect_warning(get.coverage(design.copy), NA)
  expect_lt(design.copy@design.statistics$cov.area["Mean", "Total"],
            design@design.statistics$cov.area["Mean", "Total"])
})
