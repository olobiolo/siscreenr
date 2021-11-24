test_that("powers are computed", {
  expect_equal(pow(3), exp(1) ^ 3)
  expect_equal(pow(3, 4), 4 ^ 3)
  expect_equal(pow2(3), 2 ^ 3)
  expect_equal(pow10(3), 10 ^ 3)
})

test_that("operation is vectorized", {
  expect_equal(pow(1:3), exp(1) ^ (1:3))
  expect_equal(pow(1:3, 4), 4 ^ (1:3))
  expect_equal(pow2(1:3), 2 ^ (1:3))
  expect_equal(pow10(1:3), 10 ^ (1:3))
})

test_that("arguments are chceked", {
  expect_error(pow2("2"))
  expect_error(pow(1:3, 1:2))
})
