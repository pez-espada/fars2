# this is my testing file. I use testthat to implemet some tests on my functions
library(testthat)
context("fars functions")

test_that("test_fars_read", {
  my_file <- fars_read(system.file("extdata", "accident_2014.csv.bz2", package = "fars2"))
  expect_that(my_file, is_a("data.frame"))
} )

test_that("test_make_filename", {
  my_filename <- make_filename(2001)
  expect_that(my_filename, is_a("character"))
  #expect_that(nchar(my_filename), equals(21))
  #expect_that(stringr::str_length(my_filename), equals(21))
})

test_that("test_fars_read_years", {
  my_years <- fars_read_years(2013)
  expect_that(my_years, is_a("list"))
})

test_that("test_fars_summarize_years", {
  my_summed_years <- fars_summarize_years(2014)
  expect_that(my_summed_years, is_a("data.frame"))
})

test_that("test_fars_map_state", {
  my_map <- fars_map_state(state.num = 51, year = 2014)
  expect_that(my_map, is_a("NULL"))
})
