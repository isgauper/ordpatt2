#' Unit tests for Entropy function
#'
#' File for running tests on the Entropy function
#' @keywords Distributions
#' @export


x <- rnorm(1000)
y <- rnorm(1000)
op.wn.x <- StatOrdPattHxC::OPprob(x, emb=4)
op.wn.y <- StatOrdPattHxC::OPprob(x, emb=5)

testthat::expect_equal(entropy(op.wn.x, method="Shannon")[1], StatOrdPattHxC::HShannon(op.wn.x)  )
testthat::expect_equal(entropy(op.wn.x, method="Fisher")[1], StatOrdPattHxC::HFisher(op.wn.x)  )

testthat::expect_equal(entropy(op.wn.y, method="Shannon")[1], StatOrdPattHxC::HShannon(op.wn.y)  )
testthat::expect_equal(entropy(op.wn.y, method="Fisher")[1], StatOrdPattHxC::HFisher(op.wn.y)  )

#----------Note: The following defaults to Fisher. Should I change that?-----------#
# entropy(op.wn.y)

#---------Note: this results in an error because different quotes are used. Changing to them  to “
#---------also results in an error----------------#
#testthat::expect_error(entropy(op.wn.y, "T"), "'arg' should be one of \"Fisher\", \"Shannon\"")
testthat::expect_type(entropy(op.wn.y,"F")[1], "double")


