context("jms.database")

test_that("Empty database has correct class", {
  expect_error(db<-jms.database(),NA)
  expect_is(db,'jms.database')
  expect_true(is.jms.database(db))
})

test_that("Adding and getting a table", {
  expect_error(db<-jms.database(),NA)
  table<-jms.database.table(testcolumn=c(1,2,3),testcolumn2=c('test','test','test'))
  expect_error(db[['test']]<-table,NA)
  expect_error(table2<-db[['test']],NA)
  expect_equal(table2,table)
})

test_that("Saving & loading tables", {
  path<-tempfile()
  table<-jms.database.table(testcolumn=c(1,2,3),testcolumn2=c('test','test','test'))
  expect_error(dg<-saveTable(table,path),NA)
  expect_error(ll<-loadTable(path),NA)
  expect_equal(ll$table,table)
  expect_equal(ll$hash,dg)
  unlink(path)
})

test_that("Saving & loading using [[]]", {
  existing_temp_files=list.files(tempdir(), full.names = T)
  expect_error(db<-jms.database(tempdir()),NA)
  #If the database doesn't exist we should still be able to call load (i.e. a new database)
  expect_error(load(db),NA)
  table<-jms.database.table(testcolumn=c(1,2,3),testcolumn2=c('test','test','test'))
  expect_error(db[['test']]<-table,NA)

  #Now we load a fresh database
  expect_error(db2<-jms.database(tempdir()),NA)
  expect_error(load(db2),NA)
  expect_error(table2<-db2[['test']],NA)
  expect_equal(table2,table)

  created_files=list.files(tempdir(), full.names = T)
  created_files=created_files[!created_files%in%existing_temp_files]
  if(length(created_files)) unlink(created_files)
})

test_that("Saving & loading tables with different classes", {
  existing_temp_files=list.files(tempdir(), full.names = T)
  expect_error(db<-jms.database(tempdir()),NA)
  table<-jms.database.table(testcolumn=c(1,2,3),testcolumn2=c('test','test','test'))
  expect_error(db[['test']]<-table,NA)

  tableid<-jms.database.table.id(testcolumnid=c(1,2,3),testcolumnid2=c('test','test','test'))
  expect_error(db[['testid']]<-tableid,NA)

  #Now we load a fresh database
  expect_error(db2<-jms.database(tempdir()),NA)
  expect_error(load(db2),NA)
  expect_error(table2<-db2[['test']],NA)
  expect_equal(table2,table)

  expect_error(tableid2<-db2[['testid']],NA)
  expect_equal(tableid2,tableid)

  created_files=list.files(tempdir(), full.names = T)
  created_files=created_files[!created_files%in%existing_temp_files]
  if(length(created_files)) unlink(created_files)
})

test_that("Fetches updates from another session", {
  existing_temp_files=list.files(tempdir(), full.names = T)

  #We create 2 identical databases
  expect_error(db<-jms.database(tempdir()),NA)
  expect_error(db2<-jms.database(tempdir()),NA)

  #Add a table to one
  table<-jms.database.table(testcolumn=c(1,2,3),testcolumn2=c('test','test','test'))
  expect_error(db[['test']]<-table,NA)

  #Load it from the other
  expect_error(table2<-db2[['test']],NA)
  expect_equal(table2,table)

  #Update a value in the 1st
  table[1,1]<-99

  #Check it in the 2nd
  expect_equal(table2[1,1],99)

  #Add a value to the 2nd
  table2[4,]<-list(0,'new')

  #Check the tables are still the same
  expect_equal(table2,table)

  created_files=list.files(tempdir(), full.names = T)
  created_files=created_files[!created_files%in%existing_temp_files]
  if(length(created_files)) unlink(created_files)
})

test_that("Prevents updates when locked", {
  skip_long()
  #We need separate sessions here because the lock is obtained per session, not per object
  existing_temp_files=list.files(tempdir(), full.names = T)
  expect_error(db<-jms.database(tempdir()),NA)

  job<-parallel::mcparallel({lock(db);Sys.sleep(20); unlock(db)})
  Sys.sleep(1)
  expect_error(suppressWarnings(lock(db)))
  parallel::mccollect(job)

  created_files=list.files(tempdir(), full.names = T)
  created_files=created_files[!created_files%in%existing_temp_files]
  if(length(created_files)) unlink(created_files)
})
