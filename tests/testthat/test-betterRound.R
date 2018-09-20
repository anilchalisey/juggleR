context("betterRound")

test_that("betterRound preserves trailing zeros", {
  expect_true(betterRound(5.1, 3) == "5.100")
})
