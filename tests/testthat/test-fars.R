data_dir <- system.file("extdata", package = "farsr")

test_that("make_filename builds the expected names", {
  expect_equal(make_filename(2013), "accident_2013.csv.bz2")
  expect_equal(make_filename("2014"), "accident_2014.csv.bz2")
  expect_equal(make_filename(c(2013, 2015)),
               c("accident_2013.csv.bz2", "accident_2015.csv.bz2"))
})

test_that("fars_read returns a tibble and fails for a missing file", {
  old <- setwd(data_dir)
  on.exit(setwd(old))
  dat <- fars_read("accident_2013.csv.bz2")
  expect_s3_class(dat, "tbl_df")
  expect_true(all(c("STATE", "MONTH", "LONGITUD", "LATITUDE") %in% names(dat)))
  expect_error(fars_read("accident_1900.csv.bz2"), "does not exist")
})

test_that("fars_read_years keeps MONTH and year and warns for invalid years", {
  old <- setwd(data_dir)
  on.exit(setwd(old))
  res <- fars_read_years(2013)
  expect_length(res, 1)
  expect_named(res[[1]], c("MONTH", "year"))
  expect_warning(res <- fars_read_years(c(2013, 1999)), "invalid year: 1999")
  expect_null(res[[2]])
})

test_that("fars_summarize_years gives one row per month and one column per year", {
  old <- setwd(data_dir)
  on.exit(setwd(old))
  res <- fars_summarize_years(c(2013, 2014))
  expect_equal(nrow(res), 12)
  expect_named(res, c("MONTH", "2013", "2014"))
})

test_that("fars_map_state rejects an invalid state number", {
  old <- setwd(data_dir)
  on.exit(setwd(old))
  expect_error(fars_map_state(99, 2013), "invalid STATE number")
})
