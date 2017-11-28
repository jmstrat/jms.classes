context("jms.database.table.id")

test_that("Empty table has correct class", {
  table<-jms.database.table.id()
  expect_is(table,'jms.database.table.id')
  expect_true(is.jms.database.table.id(table))
})

test_that("Empty table with columns has correct class", {
  table<-jms.database.table.id(testcolumn=numeric(),testcolumn2=character())
  expect_is(table,'jms.database.table.id')
  expect_true(is.jms.database.table.id(table))
})

test_that("Non-empty table with columns has correct class", {
  table<-jms.database.table.id(testcolumn=c(1,2,3),testcolumn2=c('test','test','test'))
  expect_is(table,'jms.database.table.id')
  expect_true(is.jms.database.table.id(table))
})

test_that("Conversion to data.frame", {
  table<-jms.database.table.id(testcolumn=c(1,2,3),testcolumn2=c('test','test','test'))
  df=as.data.frame(table)
  expect_is(df,'data.frame')
  expect_equal(df$testcolumn,c(1,2,3))
  expect_equal(df$testcolumn2,c('test','test','test'))
})

test_that("Conversion from data.frame", {
  table<-data.frame(testcolumn=c(1,2,3),testcolumn2=c('test','test','test'))
  df=as.jms.database.table.id(table)
  expect_is(df,'jms.database.table.id')
})

test_that("New table has correct ids", {
  table<-jms.database.table.id(testcolumn=c(1,2,3),testcolumn2=c('test','test','test'))
  ids=table[,'id']
  expect_equal(ids,c(1,2,3))
})

test_that("Adding a row", {
  table<-jms.database.table.id(testcolumn=c(1,2,3),testcolumn2=c('test','test','test'))
  table[]<-c(9,'new')
  expect_equal(nrow(table),4)
  expect_equal(ncol(table),3)
  expect_is(table,'jms.database.table.id')
  df<-as.data.frame(table)
  expect_equal(df,data.frame(id=c(1,2,3,4),testcolumn=c(1,2,3,9),testcolumn2=c('test','test','test','new'),stringsAsFactors = FALSE))
})

test_that("Updating a row", {
  table<-jms.database.table.id(testcolumn=c(1,2,3),testcolumn2=c('test1','test2','test3'))
  table[2]<-c(99,'updated')
  expect_equal(nrow(table),3)
  expect_equal(ncol(table),3)
  expect_is(table,'jms.database.table.id')
  df<-as.data.frame(table)
  expect_equal(df,data.frame(id=c(1,2,3),testcolumn=c(1,99,3),testcolumn2=c('test1','updated','test3'),stringsAsFactors = FALSE))
})

test_that("Getting a value", {
  table<-jms.database.table.id(testcolumn=c(1,2,3),testcolumn2=c('test1','test2','test3'))
  expect_equal(table[1,1],1)
  expect_equal(table[2,'testcolumn2'],'test2')
  expect_equivalent(table[1,],data.frame(1,1,'test1',stringsAsFactors = FALSE))
  expect_is(table,'jms.database.table.id')
})

test_that("Removing a row", {
  table<-jms.database.table.id(testcolumn=c(1,2,3),testcolumn2=c('test1','test2','test3'))
  table[-2]
  expect_equal(nrow(table),2)
  expect_equal(ncol(table),3)
  expect_is(table,'jms.database.table.id')
  df<-as.data.frame(table)
  expect_equal(df,data.frame(id=c(1,3),testcolumn=c(1,3),testcolumn2=c('test1','test3'),stringsAsFactors = FALSE))
})

test_that("Removing then adding a row", {
  table<-jms.database.table.id(testcolumn=c(1,2,3),testcolumn2=c('test1','test2','test3'))
  table[-2]
  table[]<-c(9,'new')
  expect_equal(nrow(table),3)
  expect_equal(ncol(table),3)
  expect_is(table,'jms.database.table.id')
  df<-as.data.frame(table)
  expect_equal(df,data.frame(id=c(1,3,4),testcolumn=c(1,3,9),testcolumn2=c('test1','test3','new'),stringsAsFactors = FALSE))
})

test_that("Invalid IDs", {
  table<-jms.database.table.id(testcolumn=c(1,2,3),testcolumn2=c('test1','test2','test3'))
  expect_true(all(is.na(table[4])))
  expect_error(table[-4])
  expect_error(table[-1]<-'anything')
})
