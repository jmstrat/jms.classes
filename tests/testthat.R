library(testthat)
library(jms.classes)

ProgressReporter$public_fields$show_praise=FALSE
formals(ProgressReporter$public_methods$initialize)$show_praise=FALSE
test_check("jms.classes")
