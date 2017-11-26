## Function Tests

# This script tests the functions created in the functions to be used in this 
# homework.

context("Remove missing elements")

test_that("there are no missing elements", {
  expect_equal(remove_missing(c(1, 4, NA, 5)), c(1, 4, 5))
  expect_equal(remove_missing(c(4, 5, 6)), c(4, 5, 6))
  expect_equal(remove_missing(c(NA, NA)), logical(0))
  expect_equal(remove_missing(logical(0)), logical(0))
})

context("Find the minimum element")

test_that("the minimum element is found", {
  expect_equal(get_minimum(c(1, 4, 5)), 1)
  expect_equal(get_minimum(c(100, 100, 200)), 100)
  expect_equal(get_minimum(c(1, 4, 5, NA), na.rm = TRUE), 1)
  expect_equal(get_minimum(c(NA, NA), na.rm = TRUE), 'non-numeric argument')
})

context("Find the maximum element")

test_that("the maximum element is found", {
  expect_equal(get_maximum(c(1, 4, 5)), 5)
  expect_equal(get_maximum(c(100, 200, 200)), 200)
  expect_equal(get_maximum(c(1, 4, 5, NA), na.rm = TRUE), 5)
  expect_equal(get_maximum(c(NA, NA), na.rm = TRUE), 'non-numeric argument')
})

context("Find the range of the vector")

test_that("the maximum minus the minimum is found", {
  expect_equal(get_range(c(1, 4, 5)), 4)
  expect_equal(get_range(c(1, 1, 1)), 0)
  expect_equal(get_range(c(1, 4, 5, NA), na.rm = TRUE), 4)
  expect_equal(get_range(c("Not a number")), 'non-numeric argument')
})

context("Find the tenth percentile element of the vector")

test_that("the tenth percentile is found", {
  expect_equal(as.numeric(get_percentile10(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))), 1.9)
  expect_equal(as.numeric(get_percentile10(c(4, 4, 4, 4))), 4)
  expect_equal(as.numeric(get_percentile10(c(5, 8, 20, 34, NA), na.rm = TRUE)), 5.9)
  expect_equal(get_percentile10(c('Not a number')), 'non-numeric argument')
})

context("Find the ninetieth percentile element of the vector")

test_that("the ninetieth percentile is found", {
  expect_equal(as.numeric(get_percentile90(c( 1, 2, 3, 4, 5, 6, 7, 8, 9, 10))), 9.1)
  expect_equal(as.numeric(get_percentile90(c(4, 4, 4, 4))), 4)
  expect_equal(as.numeric(get_percentile90(c(5, 8, 20, 34, NA), na.rm = TRUE)), 29.8)
  expect_equal(get_percentile90(c('Not a number')), 'non-numeric argument')
})

context("Find the first quartile of the vector")

test_that("the first quartile is found", {
  expect_equal(as.numeric(get_quartile1(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))), 3.25)
  expect_equal(as.numeric(get_quartile1(c(4, 4, 4, 4))), 4)
  expect_equal(as.numeric(get_quartile1(c(5, 8, 20, 34, NA), na.rm = TRUE)), 7.25)
  expect_equal(get_quartile1(c('Not a number')), 'non-numeric argument')
})

context('Find the third quartile of the vector')

test_that('the third quartile is found', {
  expect_equal(as.numeric(get_quartile3(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))), 7.75)
  expect_equal(as.numeric(get_quartile3(c(4, 4, 4, 4))), 4)
  expect_equal(as.numeric(get_quartile3(c(5, 8, 20 ,34, NA), na.rm = TRUE)), 23.5)
  expect_equal(get_quartile3(c('Not a number')), 'non-numeric argument')
})

context('Find the median value of the vector')

test_that('the median is found', {
  expect_equal(get_median(c(1, 2, 3, 4, 5)), 3)
  expect_equal(get_median(c(5, 7, 9, 100)), 8)
  expect_equal(get_median(c(2, 5, 9, 33, NA), na.rm = TRUE), 7)
  expect_equal(get_median(c('Not a number')), 'non-numeric argument')
})

context('Find the average value of the vector')

test_that('the average is found', {
  expect_equal(get_average(c(1, 2, 3, 4, 5)), 3)
  expect_equal(get_average(c(100, 200, 0)), 100)
  expect_equal(get_average(c(2, 45, 1, NA), na.rm = TRUE), 16)
  expect_equal(get_average(c('Not a number')), 'non-numeric argument')
})

context('Find the standard deviation of the vector')

test_that('the standard deviation is found', {
  expect_equal(round(get_stdev(c(1, 2, 3, 4, 5)), 2), 1.58)
  expect_equal(get_stdev(c(4, 4, 4, 4)), 0)
  expect_equal(get_stdev(c(5, 10, 15, NA), na.rm = TRUE), 5)
  expect_equal(get_stdev(c('Not a number')), 'non-numeric argument')
})

context('Count the number of missing elements')

test_that('number of missing elements is the same', {
  expect_equal(count_missing(c(0, 1, 2, 3)), 0)
  expect_equal(count_missing(c(100, 30, NA)), 1)
  expect_equal(count_missing(c(200, NA, NA)), 2)
  expect_equal(count_missing(c(NA, NA, NA)), 3)
})

context('Create a summary of test statistics') 

test_that('summary stats are created', {
  expect_equal(summary_stats(c(1, 2, 3))$mean, 2)
  expect_equal(summary_stats(c(1, 2, 3, NA), na.rm = TRUE)$mean, 2)
  expect_equal(summary_stats(c(1, 3, 5))$stdev, 4)
  expect_equal(summary_stats(c(1, 3, 5))$minimum, 1)
})

context('Prints statistics')

test_that('printed statistics are created', {
  expect_equal(as.character(print_stats(c(1, 2, 3))[1]), "minimum : 1.0000")
  expect_equal(as.character(print_stats(c(1, 2, 3))[3]), "quartile1 : 1.5000")
  expect_equal(as.character(print_stats(c(1, 2, 3, NA), na.rm = TRUE)[1]), "minimum : 1.0000")
  expect_equal(as.character(print_stats(c(1, 2, 3, NA), na.rm = TRUE)[3]), "quartile1 : 1.5000")
})

context('Drops the lowest element')

test_that('lowest element is dropped', {
  expect_equal(drop_lowest(c(1, 2, 3)), c(2, 3))
  expect_equal(drop_lowest(c(1, 1, 2, 3)), c(1, 2, 3))
  expect_equal(drop_lowest(c(1, 2, 3, NA), na.rm = TRUE), c(2, 3))
  expect_equal(drop_lowest(c(1, 1, 2, 3, NA), na.rm = TRUE), c(1, 2, 3))
})

context('Rescales the vector into a range from 0 to 100')

test_that('vector is rescaled', {
  expect_equal(rescale100(c(10, 25, 30), 0, 50), c(20, 50, 60))
  expect_equal(rescale100(c(1, 2, 3), 0, 200), c(0.5, 1, 1.5))
  expect_equal(rescale100(c(10, 25, 30, NA), 0, 50, na.rm = TRUE), c(20, 50, 60))
  expect_equal(rescale100(c(1, 2, 3, NA), 0, 200, na.rm = TRUE), c(0.5, 1, 1.5))
})

context('Scores the homework')

test_that('homework is scored', {
  expect_equal(score_homework(c(10, 10, 10)), 10)
  expect_equal(score_homework(c(200, 150, 40)), 130)
  expect_equal(score_homework(c(10, 10, 10), drop = TRUE), 10)
  expect_equal(score_homework(c(200, 150, 40), drop = TRUE), 175)
})

context('Scores the quizzes')

test_that('quizzes is scored', {
  expect_equal(score_quiz(c(10, 10, 10)), 10)
  expect_equal(score_quiz(c(200, 150, 40)), 130)
  expect_equal(score_quiz(c(10, 10, 10), drop = TRUE), 10)
  expect_equal(score_quiz(c(200, 150, 40), drop = TRUE), 175)
})

context('Scores a lab')

test_that('labs are scored', {
  expect_equal(score_lab(12), 100)
  expect_equal(score_lab(11), 100)
  expect_equal(score_lab(10), 80)
  expect_equal(score_lab(9), 60)
  expect_equal(score_lab(8), 40)
  expect_equal(score_lab(7), 20)
  expect_equal(score_lab(6), 0)
})
