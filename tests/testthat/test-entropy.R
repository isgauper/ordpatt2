#' Unit tests for Entropy function


test_that("Test whether outputs are the same as StatOrdPattHxC", {

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


test_that("Test  whether p of length 1 returns an error", {

  expect_error(entropy(1), "ERROR: Not a valid probability function")

})


test_that("Test  whether p of length 1 returns an error", {

  expect_error(entropy(1), "ERROR: Not a valid probability function")

})


test_that("Test  whether a negative value in p returns an error", {

  expect_error(entropy(c(.2,.4,-.1,.5)), "ERROR: Not a valid probability function")

})


test_that("Test whether sum(p) < 1 returns an error", {

  expect_error(entropy(c(.4,.5, .05)), "ERROR: Not a valid probability function")

})

test_that("Test whether sum(p) < 1 returns an error", {

  expect_error(entropy(c(.4,.5, .15)), "ERROR: Not a valid probability function")

})





