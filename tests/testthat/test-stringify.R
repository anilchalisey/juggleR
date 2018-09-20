context("stringify")

test_that("stringify converts a vector to a single character string", {
  expect_true(length(stringify(letters[1:5])) == 1)
})

test_that("stringify produces a character string in the correct format", {
  expect_true(stringify(letters[1:4]) == "a, b, c, and d")
})
