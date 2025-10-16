
test_that("Test whether outputs are the same as StatOrdPattHxC", {
  
  expect_equal(
    .Call("_ordpatt2_perm2", 0:3), # Accessess C++ function
    StatOrdPattHxC:::perm(0:3)  # Accesses internal functions
  )
  
  expect_equal(
    .Call("_ordpatt2_perm2",4:8),
    StatOrdPattHxC:::perm(4:8)
  )
  
  
})


#-----------More tests?---------#

