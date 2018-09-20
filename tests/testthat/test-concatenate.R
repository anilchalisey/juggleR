context("concatenate")

test_that("%p% basic functionality works", {
  expect_equal("a" %p% "b", "ab")
})

test_that("concatenate and %p% are the same", {
  lhs <- concatenate("a", "B")
  rhs <- "a" %p% "B"
  expect_equal(lhs, rhs)
})

test_that("concatenate and paste0 are the same", {
  lhs <- concatenate("a", "B")
  rhs <- paste0("a", "B")
  expect_equal(lhs, rhs)
})
