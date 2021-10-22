test_that("geometric mean is computed", {
  # two items
  expect_equal(meanG(c(4, 9)), 6)
  # three items
  expect_equal(meanG(c(4, 6, 9)), 6)
  # NAs included
  expect_equal(meanG(c(4, 6, 9, NA)), NA_real_)
  # NAs drpped
  expect_equal(meanG(c(4, 6, 9, NA), na.rm = TRUE), 6)
})
