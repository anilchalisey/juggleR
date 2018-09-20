context("dfCharacterize")

df <- data.frame(a = 1:3, b = letters[1:3], c = LETTERS[1:3],
                 stringsAsFactors = TRUE)

test_that("dfCharacterize basic functionality works", {
  expect_equal(as.logical(sapply(dfCharacterize(df), is.character)),
               c(FALSE, TRUE, TRUE))
})

test_that("dfCharacterize only argument works", {
  expect_equal(as.logical(sapply(dfCharacterize(df, only = "b"), is.character)),
               c(FALSE, TRUE, FALSE))
})

test_that("dfCharacterize ignore argument works", {
  expect_equal(as.logical(sapply(dfCharacterize(df, ignore = "b"), is.character)),
               c(FALSE, FALSE, TRUE))
  expect_equal(dfCharacterize(df, ignore = c("b", "c")),
               df)
})

test_that("dfCharacterize does not convert non-character columns", {
  expect_equal(dfCharacterize(df, only = "a"),
               df)
})

test_that("dfCharacterize throws an error when a column name doesn't exist", {
  expect_error(dfCharacterize(df, only = "z"))
  expect_error(dfCharacterize(df, only = c("a", "z")))
  expect_error(dfCharacterize(df, ignore = "z"))
  expect_error(dfCharacterize(df, ignore = c("a", "z")))
})

test_that("dfCharacterize throws an error when both only and ignore are set", {
  expect_error(dfCharacterize(df, only = "b", ignore = "c"))
})
