context("jms.database.table")

test_that("Empty table has correct class", {
  table<-jms.database.table()
  expect_is(table,'jms.database.table')
  expect_true(is.jms.database.table(table))
})

test_that("Empty table with columns has correct class", {
  table<-jms.database.table(testcolumn=numeric(),testcolumn2=character())
  expect_is(table,'jms.database.table')
  expect_true(is.jms.database.table(table))
})

test_that("Non-empty table with columns has correct class", {
  table<-jms.database.table(testcolumn=c(1,2,3),testcolumn2=c('test','test','test'))
  expect_is(table,'jms.database.table')
  expect_true(is.jms.database.table(table))
})

test_that("Conversion to data.frame", {
  table<-jms.database.table(testcolumn=c(1,2,3),testcolumn2=c('test','test','test'))
  df=as.data.frame(table)
  expect_is(df,'data.frame')
  expect_equal(df,data.frame(testcolumn=c(1,2,3),testcolumn2=c('test','test','test'),stringsAsFactors = FALSE))
})

test_that("Conversion from data.frame", {
  table<-data.frame(testcolumn=c(1,2,3),testcolumn2=c('test','test','test'))
  df=as.jms.database.table(table)
  expect_is(df,'jms.database.table')
})

test_that("Adding a row", {
  table<-jms.database.table(testcolumn=c(1,2,3),testcolumn2=c('test','test','test'))
  expect_error(table[4,]<-list(9,'new'),NA)
  expect_equal(nrow(table),4)
  expect_equal(ncol(table),2)
  expect_is(table,'jms.database.table')
  expect_error(df<-as.data.frame(table),NA)
  expect_equal(df,data.frame(testcolumn=c(1,2,3,9),testcolumn2=c('test','test','test','new'),stringsAsFactors = FALSE))
})

test_that("Updating a row", {
  table<-jms.database.table(testcolumn=c(1,2,3),testcolumn2=c('test1','test2','test3'))
  expect_error(table[2]<-list(99,'updated'),NA)
  expect_equal(nrow(table),3)
  expect_equal(ncol(table),2)
  expect_is(table,'jms.database.table')
  expect_error(df<-as.data.frame(table),NA)
  expect_equal(df,data.frame(testcolumn=c(1,99,3),testcolumn2=c('test1','updated','test3'),stringsAsFactors = FALSE))
})

test_that("Getting a value", {
  table<-jms.database.table(testcolumn=c(1,2,3),testcolumn2=c('test1','test2','test3'))
  expect_equal(table[1,1],1)
  expect_equal(table[2,'testcolumn2'],'test2')
  expect_equivalent(table[1,],data.frame(1,'test1',stringsAsFactors = FALSE))
  expect_is(table,'jms.database.table')
})

test_that("Removing a row", {
  table<-jms.database.table(testcolumn=c(1,2,3),testcolumn2=c('test1','test2','test3'))
  expect_error(table[-2],NA)
  expect_equal(nrow(table),2)
  expect_equal(ncol(table),2)
  expect_is(table,'jms.database.table')
  expect_error(df<-as.data.frame(table),NA)
  expect_equal(df,data.frame(testcolumn=c(1,3),testcolumn2=c('test1','test3'),stringsAsFactors = FALSE))
})

test_that("Removing then adding a row", {
  table<-jms.database.table(testcolumn=c(1,2,3),testcolumn2=c('test1','test2','test3'))
  expect_error(table[-2],NA)
  expect_error(table[3]<-list(9,'new'),NA)
  expect_equal(nrow(table),3)
  expect_equal(ncol(table),2)
  expect_is(table,'jms.database.table')
  expect_error(df<-as.data.frame(table),NA)
  expect_equal(df,data.frame(testcolumn=c(1,3,9),testcolumn2=c('test1','test3','new'),stringsAsFactors = FALSE))
})

test_that("Invalid rows", {
  table<-jms.database.table(testcolumn=c(1,2,3),testcolumn2=c('test1','test2','test3'))
  expect_true(all(is.na(table[4])))
  expect_equal(table[-4],as.data.frame(table))
  expect_error(table[-1]<-'anything')
})

test_that("2D subsetting",{
  table<-jms.database.table(testcolumn=c(1,2,3),testcolumn2=c('test1','test2','test3'))
  expect_error(table[4:5,]<-list(c(9,10),c('new1','new2')),NA)
  expect_equal(nrow(table),5)
  expect_equal(ncol(table),2)
  expect_is(table,'jms.database.table')
  expect_error(df<-as.data.frame(table),NA)
  expect_equal(df,data.frame(testcolumn=c(1,2,3,9,10),testcolumn2=c('test1','test2','test3','new1','new2'),stringsAsFactors = FALSE))

  table<-jms.database.table(testcolumn=c(1,2,3),testcolumn2=c('test1','test2','test3'))
  expect_error(table[4:5,]<-data.frame(testcolumn=c(9,10),testcolumn2=c('new1','new2'),stringsAsFactors = F),NA)
  expect_equal(nrow(table),5)
  expect_equal(ncol(table),2)
  expect_is(table,'jms.database.table')
  expect_error(df<-as.data.frame(table),NA)
  expect_equal(df,data.frame(testcolumn=c(1,2,3,9,10),testcolumn2=c('test1','test2','test3','new1','new2'),stringsAsFactors = FALSE))

  table<-jms.database.table(testcolumn=c(1,2,3),testcolumn2=c('test1','test2','test3'))
  expect_error(table[4:5,]<-data.frame(testcolumn2=c('new1','new2'),testcolumn=c(9,10),stringsAsFactors = F),NA)
  expect_equal(nrow(table),5)
  expect_equal(ncol(table),2)
  expect_is(table,'jms.database.table')
  expect_error(df<-as.data.frame(table),NA)
  expect_equal(df,data.frame(testcolumn=c(1,2,3,9,10),testcolumn2=c('test1','test2','test3','new1','new2'),stringsAsFactors = FALSE))

  table<-jms.database.table(testcolumn=c(1,2,3),testcolumn2=c('test1','test2','test3'))
  expect_equal(table[1:2,],data.frame(testcolumn=c(1,2),testcolumn2=c('test1','test2'),stringsAsFactors = FALSE))
  expect_equal(table[,1:2],data.frame(testcolumn=c(1,2,3),testcolumn2=c('test1','test2','test3'),stringsAsFactors = FALSE))
  expect_equal(table[1:2,1],c(1,2))
  expect_equal(table[1:2,1:2],data.frame(testcolumn=c(1,2),testcolumn2=c('test1','test2'),stringsAsFactors = FALSE))
})

test_that("Validating", {
  validate <- function(testcolumn=numeric(),testcolumn2=character()) {
    testcolumn=assert_numeric(testcolumn,"Invalid testcolumn")
    testcolumn2=assert_character(testcolumn2,"Invalid testcolumn2")
    list(testcolumn=testcolumn,testcolumn2=testcolumn2)
  }
  table<-jms.database.table(testcolumn=c(1,2,3),
                            testcolumn2=c('test1','test2','test3'),
                            validator=validate)
  expect_is(table,'jms.database.table')
  expect_error(table[4,]<-list(9,'new1'),NA)
  expect_error(table[4,]<-list('notanumber','fail'))
  expect_error(table[5,]<-list(testcolumn=2,testcolumn2='new2'),NA)
  expect_error(table[6,]<-list(testcolumn2='new3',testcolumn=10),NA)
  expect_error(table[7:8,]<-list(testcolumn2=c('new4','new5'),testcolumn=11:12),NA)
  expect_is(table,'jms.database.table')
  df<-as.data.frame(table)
  expect_equal(df,data.frame(testcolumn=c(1,2,3,9,2,10,11,12),testcolumn2=c('test1','test2','test3','new1','new2','new3','new4','new5'),stringsAsFactors = FALSE))
})
