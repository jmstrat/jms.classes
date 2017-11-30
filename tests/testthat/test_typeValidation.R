context("Type Validation")

test_that("numeric", {
  expect_equal(verify_numeric(0),TRUE)
  expect_equal(verify_numeric(-99999),TRUE)
  expect_equal(verify_numeric(99999),TRUE)
  expect_equal(verify_numeric('99'),TRUE)
  expect_equal(verify_numeric('-99'),TRUE)
  expect_equal(verify_numeric('notanumber'),FALSE)
  expect_equal(verify_numeric(FALSE),TRUE)
  expect_equal(verify_numeric(NULL),FALSE)

  expect_equal(assert_numeric(0),0)
  expect_equal(assert_numeric(-99999),-99999)
  expect_equal(assert_numeric(99999),99999)
  expect_equal(assert_numeric('99'),99)
  expect_equal(assert_numeric('-99'),-99)
  expect_error(assert_numeric('notanumber'))
  expect_equal(assert_numeric(FALSE),0)
  expect_error(assert_numeric(NULL))
})

test_that("positive", {
  expect_equal(verify_positive(0),TRUE)
  expect_equal(verify_positive(-99999),FALSE)
  expect_equal(verify_positive(99999),TRUE)
  expect_equal(verify_positive('99'),TRUE)
  expect_equal(verify_positive('-99'),FALSE)
  expect_equal(verify_positive('notanumber'),FALSE)
  expect_equal(verify_positive(FALSE),TRUE)
  expect_equal(verify_positive(NULL),FALSE)

  expect_equal(assert_positive(0),0)
  expect_error(assert_positive(-99999))
  expect_equal(assert_positive(99999),99999)
  expect_equal(assert_positive('99'),99)
  expect_error(assert_positive('-99'))
  expect_error(assert_positive('notanumber'))
  expect_equal(assert_positive(FALSE),0)
  expect_error(assert_positive(NULL))
})

test_that("character", {
  expect_equal(assert_character(0),'0')
  expect_equal(assert_character(-99999),'-99999')
  expect_equal(assert_character('99'),'99')
  expect_equal(assert_character('notanumber'),'notanumber')
  expect_equal(assert_character(FALSE),'FALSE')
  expect_error(assert_character(NULL))
})

test_that("list", {
  expect_equal(assert_list(list(0,1,2)),list(0,1,2))
  expect_equal(assert_list(list(test1=0,test2=1,test3=2)),list(test1=0,test2=1,test3=2))
  expect_equal(assert_list(c(0,1,2)),list(0,1,2))
  expect_equal(assert_list(0),list(0))
  expect_error(assert_list(NULL))
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
