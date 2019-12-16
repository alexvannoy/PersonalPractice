library(testthat)
library(future)

context("Test eigen_function asynchronously")

test_that(
  "eigen_function works",
  {
    a = future::value(future::future(eigen_function()))
    b = future::value(future::future(eigen_function()))

    expect_equal(a, b)
  }
)

test_that(
  "eigen_function seed type error handling works",
  {
    #Intentionally ignore the warning I expect.
    a = suppressWarnings(future::value(future::future(eigen_function(seed = "a"))))
    b = future::value(future::future(eigen_function()))
    expect_equal(a, b)
  }
)

test_that(
  "eigen_function seed length error handling works",
  {
    #Intentionally ignore the warning I expect.
    a = suppressWarnings(future::value(future::future(eigen_function(seed = c(1,"a")))))
    b = future::value(future::future(eigen_function()))
    expect_equal(a, b)
  }
)

test_that(
  "eigen_function n type error handling works",
  {
    #Intentionally ignore the warning I expect.
    a = suppressWarnings(future::value(future::future(eigen_function(n = "a"))))
    b = future::value(future::future(eigen_function()))
    expect_equal(a, b)
  }
)

test_that(
  "eigen_function n length error handling works",
  {
    #Intentionally ignore the warning I expect.
    a = suppressWarnings(future::value(future::future(eigen_function(n = c(5,"a")))))
    b = future::value(future::future(eigen_function()))
    expect_equal(a, b)
  }
)
