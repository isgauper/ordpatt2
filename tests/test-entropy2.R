#' Unit tests for Entropy function


test_that("Test whether outputs are the same as StatOrdPattHxC", {
  # Get test data
  set.seed(123)  # for reproducibility
  x <- rnorm(1000)

  # Get ordinal pattern probabilities
  op.wn.4 <- StatOrdPattHxC::OPprob(x, emb = 4)
  op.wn.5 <- StatOrdPattHxC::OPprob(x, emb = 5)


  expect_equal(
    entropy(op.wn.4, method = "Shannon")[1],
    StatOrdPattHxC::HShannon(op.wn.4)
  )

  expect_equal(
    entropy(op.wn.4, method = "Fisher")[1],
    StatOrdPattHxC::HFisher(op.wn.4)
  )

  expect_equal(
    entropy(op.wn.5, method = "Renyi")[1],
    StatOrdPattHxC::HRenyi(op.wn.5)
  )

  expect_equal(
    entropy(op.wn.5, method = "Tsallis")[1],
    StatOrdPattHxC::HTsallis(op.wn.5)
  )


})


test_that("Test  whether output type is double", {

  expect_type(entropy(op.wn.4,"F")[1], "double")

})


test_that("Test  whether an incorrect method input returns an error", {

  expect_error(entropy(op.wn.4,"Fischer"))

})


test_that("Test  whether missing p returns an error", {

  expect_error(entropy(method="Fischer"))

})


