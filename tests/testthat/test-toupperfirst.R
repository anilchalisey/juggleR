context("toupperfirst")

test_that("toupperfirst basic functionality works", {
  expect_equal(
    toupperfirst("alllower"),
    "Alllower"
  )
})

test_that("toupperfirst works on vectors", {
  expect_equal(
    toupperfirst(c("camelCase", "alllower")),
    c("CamelCase", "Alllower")
  )
})

test_that("toupperfirst only changes alphabetical characters", {
  strings <- c("_one", "1234")
  expect_equal(toupperfirst(strings), strings)
})

test_that("toupperfirst only accepts character vectors", {
  errMsg <- "is.character\\(.*\\)"
  expect_error(toupperfirst(5), errMsg)
  expect_error(toupperfirst(data.frame()), errMsg)
})
