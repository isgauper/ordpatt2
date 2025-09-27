#' Unit tests for Entropy function


test_that("entropy function works with different embedding dimensions", {
  # Generate test data
  set.seed(123)  # for reproducibility
  x <- rnorm(1000)

  # Generate ordinal pattern probabilities with embedding dimension 5
  op.wn.y <- StatOrdPattHxC::OPprob(x, emb = 5)

  # Test with embedding dimension 5
  expect_equal(
    entropy(op.wn.y, method = "Shannon")[1],
    StatOrdPattHxC::HShannon(op.wn.y)
  )

  expect_equal(
    entropy(op.wn.y, method = "Fisher")[1],
    StatOrdPattHxC::HFisher(op.wn.y)
  )
})
