
test_that("Test whether outputs are the same as StatOrdPattHxC", {
  
  expect_equal(
    .Call("_ordpatt2_pi_i2", c(1,2,5,3,4,0)), # Accesses C++ function
    StatOrdPattHxC:::pi_i(c(1,2,5,3,4,0))  # Accesses internal functions
  )
  
  expect_equal(
    .Call("_ordpatt2_pi_i2",4:8),
    StatOrdPattHxC:::pi_i(4:8)
  )
  
  
})


#-----------More tests?---------#
#------expect NA-----#

