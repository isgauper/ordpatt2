#' Unit tests for the OPprob2 function


test_that("Test whether outputs are the same as StatOrdPattHxC", {
  # Get test data
  # set.seed(123)  # for reproducibility
  # x <- rnorm(1000)


  expect_equal(
    as.numeric(OPprob2(x, emb=4)),
    StatOrdPattHxC::OPprob(x, emb=4)
  )

  expect_equal(
    as.numeric(OPprob2(x, emb=5)),
    StatOrdPattHxC::OPprob(x, emb=5)
  )


})


test_that("Test whether missing embedding dimension returns an error", {

  expect_error(OPprob2(x))

})

test_that("Test whether missing time series returns an error", {

  expect_error(OPprob2(emb=5))

})
