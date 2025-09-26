#' Test the entropy function
#'
#' Calls entropy_test.R file and runs tests

#' @export
test_entropy <- function(){

    testthat::test_file(file.path("tests", "testthat", "test_entropy.R"))

}


test_entropy
