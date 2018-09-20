context("orderedFactor")

test_that("orderedFactor preserves order of factors", {
  charvec <- c("cat", "banana", "dog", "apple")
  expect_equal(orderedFactor(charvec), factor(charvec, levels = charvec))
  expect_equal(orderedFactor(3:1), factor(3:1, levels = 3:1))
  expect_false(identical(orderedFactor(charvec), factor(charvec)))
  expect_false(identical(orderedFactor(3:1), factor(3:1)))
})


