
context("stack")

test_that("stack operations", {

  s <- stack()
  s$push(1L)
  expect_identical(s$pop(), 1L)

  s$push(1L)
  s$push(2L)
  expect_identical(s$pop(), 2L)
  expect_identical(s$pop(), 1L)
})

test_that("stack allocates more storage if needed", {

  s <- stack(2)

  s$push(1L)
  s$push(2L)
  s$push(3L)
  s$push(4L)
  s$push(5L)

  expect_identical(s$pop(), 5L)
  expect_identical(s$pop(), 4L)
  expect_identical(s$pop(), 3L)
  expect_identical(s$pop(), 2L)
  expect_identical(s$pop(), 1L)
})

test_that("error for empty stack", {

  s <- stack()
  expect_error(s$pop(), "Nothing to pop")
  expect_error(s$peek(), "Nothing to peek")

  s$push(1L)
  s$pop()
  expect_error(s$pop(), "Nothing to pop")
})

test_that("arbitrarly objects are fine", {

  s <- stack()
  m <- lm(mpg ~ disp, data = mtcars)

  s$push(m)
  s$push(iris)

  expect_identical(s$pop(), iris)
  expect_identical(s$pop(), m)
})

test_that("pushing NULL is fine", {

  s <- stack()
  s$push(NULL)
  s$push(1L)
  s$push(NULL)
  s$push(NULL)

  expect_identical(s$pop(), NULL)
  expect_identical(s$pop(), NULL)
  expect_identical(s$pop(), 1L)
  expect_identical(s$pop(), NULL)
})

test_that("peek", {

  s <- stack()
  s$push(1L)

  expect_identical(s$peek(), 1L)
})

test_that("size", {

  s <- stack()
  expect_identical(s$size(), 0L)

  s$push(100)
  expect_identical(s$size(), 1L)

  s$pop()
  expect_identical(s$size(), 0L)
})

test_that("is_empty", {

  s <- stack()
  expect_identical(s$is_empty(), TRUE)

  s$push(100)
  expect_identical(s$is_empty(), FALSE)

  s$pop()
  expect_identical(s$is_empty(), TRUE)
})
