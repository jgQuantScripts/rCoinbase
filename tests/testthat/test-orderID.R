library(testthat)
test_that("cb_get_order_id creates a valid id", {
  id <- cb_get_order_id()
  expect_type(id, "character")
})
