# ==================================================================
# title: tests
# description: tests from testthat for functions in functions.R
# ==================================================================



context("remove_missing")
test_that("remove_missing is correct",{
expect_equal(remove_missing(c(1,NA)), c(1))
expect_equal(remove_missing(c(1,2)), c(1,2))
expect_equal(remove_missing(c(1,3,5,NA,9)), c(1,3,5,9))
expect_equal(remove_missing(c(NA,4,6)), c(4,6))
})


context("get_minimum")
test_that("get_minimum is correct",{
  expect_equal(get_minimum(c(1,NA)), c(1))
  expect_equal(get_minimum(c(1,2)), c(1))
  expect_equal(get_minimum(c(1,3,5,NA,9)), c(1))
  expect_equal(get_minimum(c(NA,4,6)), c(4))
})


context("get_maximum")
test_that("get_maximum is correct",{
  expect_equal(get_maximum(c(1,NA)), c(1))
  expect_equal(get_maximum(c(1,2)), c(2))
  expect_equal(get_maximum(c(1,3,5,NA,9)), c(9))
  expect_equal(get_maximum(c(NA,4,6)), c(6))
})


context("get_range")
test_that("get_range is correct",{
  expect_equal(get_range(c(1,NA)), c(0))
  expect_equal(get_range(c(1,2)), c(1))
  expect_equal(get_range(c(1,3,5,NA,9)), c(8))
  expect_equal(get_range(c(NA,4,6)), c(2))
})


context("get_percentile10")
test_that("rget_percentile10 is correct",{
  expect_equal(get_percentile10(c(1,NA)), c(1))
  expect_equal(get_percentile10(c(1,2)), c(1.1))
  expect_equal(get_percentile10(c(1,3,5,NA,9)), c(1.6))
  expect_equal(get_percentile10(c(NA,4,6)), c(4.2))
})


context("get_percentile90")
test_that("get_percentile90 is correct",{
  expect_equal(get_percentile90(c(1,NA)), c(1))
  expect_equal(get_percentile90(c(1,2)), c(1.9))
  expect_equal(get_percentile90(c(1,3,5,NA,9)), c(7.8))
  expect_equal(get_percentile90(c(NA,4,6)), c(5.8))
})


context("get_quartile1")
test_that("get_quartile1 is correct",{
  expect_equal(get_quartile1(c(1,NA)), c(1))
  expect_equal(get_quartile1(c(1,2)), c(1.25))
  expect_equal(get_quartile1(c(1,3,5,NA,9)), c(2.5))
  expect_equal(get_quartile1(c(NA,4,6)), c(4.5))
})


context("get_quartile3")
test_that("get_quartile3 is correct",{
  expect_equal(get_quartile3(c(1,NA)), c(1))
  expect_equal(get_quartile3(c(1,2)), c(1.75))
  expect_equal(get_quartile3(c(1,3,5,NA,9)), c(6))
  expect_equal(get_quartile3(c(NA,4,6)), c(5.5))
})


context("get_median")
test_that("get_median is correct",{
  expect_equal(get_median(c(1,NA)), c(1))
  expect_equal(get_median(c(1,2)), c(1.5))
  expect_equal(get_median(c(1,3,5,NA,9)), c(4))
  expect_equal(get_median(c(NA,4,6)), c(5))
})


context("get_average")
test_that("get_average is correct",{
  expect_equal(get_average(c(1,NA)), c(1))
  expect_equal(get_average(c(1,2)), c(1.5))
  expect_equal(get_average(c(1,3,5,NA,9)), c(4.5))
  expect_equal(get_average(c(NA,4,6)), c(5))
})


context("get_stdev")
test_that("get_stdev is correct",{
  expect_equal(get_stdev(c(1,NA)), c(NaN))
  expect_equal(get_stdev(c(1,2)), c(sd(c(1,2))))
  expect_equal(get_stdev(c(1,3,5,NA,9)), c(sd(c(1,3,5,9))))
  expect_equal(get_stdev(c(NA,4,6)), c(sd(c(4,6))))
})


context("count_missing")
test_that("count_missing is correct",{
  expect_equal(count_missing(c(1,NA)), c(1))
  expect_equal(count_missing(c(1,2)), c(0))
  expect_equal(count_missing(c(1,3,5,NA,9)), c(1))
  expect_equal(count_missing(c(NA,4,6)), c(1))
})


context("summary_stats")
test_that("summary_stats is correct",{
  expect_equal(length(summary_stats(c(1,NA))), c(10))
  expect_equal(typeof(summary_stats(c(1,2))[5]), c("list"))
  expect_equal(names(summary_stats(c(1,3,5,NA,9))), c("minimum", "percent10", "quartile1", "median", "quartile3",
                                               "percent90", "maximum", "range", "stdev", "missing"))
  expect_equal(summary_stats(c(NA,4,6))[[1]], c(4))
})


context("drop_lowest")
test_that("drop_lowest is correct",{
  expect_equal(drop_lowest(c(1,NA, 9)), c(NA,9))
  expect_equal(drop_lowest(c(1,2)), c(2))
  expect_equal(drop_lowest(c(1,3,5,NA,9)), c(3,5,NA,9))
  expect_equal(drop_lowest(c(4,6)), c(6))
})


context("rescale100")
test_that("rescale100 is correct",{
  expect_equal(rescale100(c(1,NA), 1, 1), c(NaN, NA))
  expect_equal(rescale100(c(1,2), 1, 2), c(0, 100))
  expect_equal(rescale100(c(1,3,5,NA,9), 1, 9), c(0, 25, 50, NA, 100))
  expect_equal(rescale100(c(NA,4,6), 4, 6), c(NA, 0, 100))
})

context("score_homework")
test_that("score_homework is correct",{
  expect_equal(score_homework(c(1,NA), drop = FALSE), c(1))
  expect_equal(score_homework(c(1,2), drop = FALSE), c(1.5))
  expect_equal(score_homework(c(1,3,5,NA,9)), c(5.6666667))
  expect_equal(score_homework(c(NA,4,6), drop = FALSE), c(5))
})

context("score_quiz")
test_that("score_quiz is correct",{
  expect_equal(score_quiz(c(1,NA), drop = FALSE), c(1))
  expect_equal(score_quiz(c(1,2), drop = FALSE), c(1.5))
  expect_equal(score_quiz(c(1,3,5,NA,9)), c(5.66666667))
  expect_equal(score_quiz(c(NA,4,6), drop = FALSE), c(5))
})

context("score_lab")
test_that("score_lab is correct",{
  expect_equal(score_lab(c(1)), c(0))
  expect_equal(score_lab(c(7)), c(20))
  expect_equal(score_lab(c(9)), c(60))
  expect_equal(score_lab(c(12)), c(100))
})

