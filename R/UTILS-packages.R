#' Raises an error if packages are not installed.
#' @param ... Packages to test
#' @param purpose Used in error message
#' @export
assert_packages <- function(..., purpose="This function") {
  package_list <- c(...)
  missing_packages <- c()
  for (p in package_list) {
    if (!requireNamespace(p, quietly=TRUE)) {
      missing_packages <- c(missing_packages, p)
    }
  }
  if (length(missing_packages) > 0) {
    command <- sprintf("install.packages(c('%s'))", paste0(missing_packages, collapse="', '"))

    stop(purpose, " requires additional packages. Run the following command to install them, then try again:\n",
         command, "\n\nYou may need to restart R after running this command to complete the installation.", call.=FALSE)
  }
}
