context("jms.database")

test_that("Empty database has correct class", {
  db<-jms.database()
  expect_is(db,'jms.database')
  expect_true(is.jms.database(db))
})

test_that("Adding and getting a table", {
  db<-jms.database()
  table<-jms.database.table(testcolumn=c(1,2,3),testcolumn2=c('test','test','test'))
  expect_error(db[['test']]<-table,NA)
  expect_error(table2<-db[['test']],NA)
  expect_equal(table2,table)
})

test_that("Saving & loading", {

})

test_that("Fetches updates from another session", {

})

test_that("Prevents updates when locked", {

})
