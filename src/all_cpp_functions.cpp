#include <Rcpp.h>
#include <algorithm>
using namespace Rcpp;



// source: https://iq.opengenus.org/generating-all-permutations-of-an-array-using-cpp-stl/

// Get all possible permutations of a series of numbers

// [[Rcpp::export]]
IntegerMatrix perm2(IntegerVector v) {

  // get length of the sequence
  int n = v.size();

  // get n! for total number of permutations
  int total = 1;
  for (int i = 2; i <= n; i++)
    total = total * i;

  // create matrix for X
  IntegerMatrix X(total, n);

  // use next_permutation to get all permutations
  int row = 0;
  do {
    for (int j = 0; j < n; j++) {
      X(row, j) = v[j];
    }
    row++;
  } while (std::next_permutation(v.begin(), v.end()));

  return X;
}



// Gets the row number of a sequence of numbers from the list of all permutations

// [[Rcpp::export]]
int pi_i2(IntegerVector pat) {
  int a = pat.size() - 1;

  // get sequence from 0 to a
  IntegerVector seq_vec(a + 1);
  for (int i = 0; i <= a; i++) {
    seq_vec[i] = i;
  }

  // get all permutations using perm2
  IntegerMatrix op = perm2(seq_vec);

  // do row matching
  for (int i = 0; i < op.nrow(); i++) {
    IntegerVector row = op(i, _);  // get row i
    if (is_true(all(row == pat)))  // check if the row matches the pattern
      return i + 1;       // R indexing starts at 1. C++ starts at 0
  }


  // Return NA if no match found
  return NA_INTEGER;
}








