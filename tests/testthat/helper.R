if(!exists('run_all_tests')) run_all_tests=FALSE

skip_long <- function() {
  if (!run_all_tests) skip("Long task. Set run_all_tests=TRUE to run.")
}
