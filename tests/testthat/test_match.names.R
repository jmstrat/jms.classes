context("match.names")

expected_names=c('test1','test2','test3','test4')
test_that("match.names works with unnamed list", {
  valueList=list(1,2,3,4)
  result=match.names(expected_names,valueList)
  expect_equal(result,list(test1=1,test2=2,test3=3,test4=4))
})
test_that("match.names works with named list", {
  valueList=list(test1=1,test2=2,test3=3,test4=4)
  result=match.names(expected_names,valueList)
  expect_equal(result,valueList)

  result=match.names(expected_names,valueList[c(2,4,1,3)])
  expect_equal(result,valueList)
})

test_that("match.names works with mixed named list", {
  valueList=list(test1=1,test2=2,3,4)
  result=match.names(expected_names,valueList)
  expect_equal(result,list(test1=1,test2=2,test3=3,test4=4))

  valueList=list(test2=2,test4=4,1,3)
  expect_warning(result<-match.names(expected_names,valueList))
  expect_equal(result,list(test1=1,test2=2,test3=3,test4=4))
})

test_that("match.names works with incorrectly named list", {
  valueList=list(test7=7,test8=8,test9=9,test10=10)
  expect_error(match.names(expected_names,valueList))
})

test_that("match.names works with different length lists", {
  valueList=list(test1=1,test2=2)
  expect_warning(result<-match.names(expected_names,valueList))
  expect_equal(result,valueList)

  valueList=list(test3=3,test2=2)
  expect_warning(result<-match.names(expected_names,valueList))
  expect_equal(result,list(test2=2,test3=3))

  valueList=list(test1=1,test2=2,test3=3,test4=4,test5=5)
  expect_warning(result<-match.names(expected_names,valueList))
  expect_equal(result,list(test1=1,test2=2,test3=3,test4=4))
})
