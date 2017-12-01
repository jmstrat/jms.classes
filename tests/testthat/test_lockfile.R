context("lockfile")

test_that("lockfile is working", {
  path=tempfile()
  expect_error(make_lockfile(path),NA)
  expect_true(dir.exists(path))
  expect_error(suppressWarnings(make_lockfile(path,timeout=0,unique=FALSE)),NA)
  expect_error(suppressWarnings(make_lockfile(path,timeout=0,unique=TRUE)))
  expect_error(remove_lockfile(path),NA)
  expect_error(remove_lockfile(path),NA)
  expect_error(remove_lockfile(path))
})
