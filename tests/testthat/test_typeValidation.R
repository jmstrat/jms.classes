context("Type Validation")

test_that("numeric", {
  expect_equal(verify_numeric(0),TRUE)
  expect_equal(verify_numeric(-99999),TRUE)
  expect_equal(verify_numeric(99999),TRUE)
  expect_equal(verify_numeric('99'),TRUE)
  expect_equal(verify_numeric('-99'),TRUE)
  expect_equal(verify_numeric('notanumber'),FALSE)
  expect_equal(verify_numeric(FALSE),TRUE)

  expect_equal(assert_numeric(0),0)
  expect_equal(assert_numeric(-99999),-99999)
  expect_equal(assert_numeric(99999),99999)
  expect_equal(assert_numeric('99'),99)
  expect_equal(assert_numeric('-99'),-99)
  expect_error(assert_numeric('notanumber'))
  expect_equal(assert_numeric(FALSE),0)
})

test_that("positive", {
  expect_equal(verify_positive(0),TRUE)
  expect_equal(verify_positive(-99999),FALSE)
  expect_equal(verify_positive(99999),TRUE)
  expect_equal(verify_positive('99'),TRUE)
  expect_equal(verify_positive('-99'),FALSE)
  expect_equal(verify_positive('notanumber'),FALSE)
  expect_equal(verify_positive(FALSE),TRUE)

  expect_equal(assert_positive(0),0)
  expect_error(assert_positive(-99999))
  expect_equal(assert_positive(99999),99999)
  expect_equal(assert_positive('99'),99)
  expect_error(assert_positive('-99'))
  expect_error(assert_positive('notanumber'))
  expect_equal(assert_positive(FALSE),0)
})

test_that("character", {
  #assert_character
})

test_that("list", {
  #assert_list
})

test_that("date", {
  #is.Date
  #assert_date
})

test_that("file", {
  #assert_file
})

test_that("directory", {
  #assert_directory
})

test_that("logical", {
  #assert_logical
})

test_that("colour", {
  #assert_colour
})

test_that("function", {
  #assert_function
})
