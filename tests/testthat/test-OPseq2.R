#' Unit tests for the OPprob2 function


test_that("Test whether outputs are the same as StatOrdPattHxC", {

  expect_equal(
    OPseq2(x, emb=4, lag=2),
    StatOrdPattHxC::OPseq(x, emb=4, lag=2)
  )

  expect_equal(
    OPseq2(lynx, emb=5, lag=3),
    StatOrdPattHxC::OPseq(lynx, emb=5, lag=3)
  )


})




test_that("Test whether missing embedding dimension returns an error", {

  expect_error(OPseq2(x))

})

test_that("Test whether missing time series returns an error", {

  expect_error(OPseq2(emb=5))

})


test_that("Test whether missing embedding dimension returns an error", {

  expect_error(OPseq2(x))

})

test_that("Test whether missing time series returns an error", {

  expect_error(OPseq2(emb=5))

})

test_that("Test for error if emb < 2", {

  expect_error(OPseq2(x, emb=1), "'emb' must be greater than 1" )

})


test_that("Test for error if emb is not a whole number", {

  expect_error(OPseq2(x, emb=2.5), "'emb' must be an integer")

})


test_that("Test for error if all columns in TS are not numeric", {

   expect_error(OPseq2(cbind(1:4, c("A","b","c","d")),emb=2), "All columns in 'TS' must be numeric.")

})


test_that("Test for error if TS is a list", {

    expect_error(OPseq2(list(lynx),emb=2,lag=2), "'TS' must not be a list")

})

