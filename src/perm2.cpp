#include <Rcpp.h>
#include <algorithm>
using namespace Rcpp;

// source: https://iq.opengenus.org/generating-all-permutations-of-an-array-using-cpp-stl/

// [[Rcpp::export]]
IntegerMatrix perm2(IntegerVector input) {
  std::vector<int> a(input.begin(), input.end());
  int n = a.size();

  // Calculate n! for total number of permutations
  int total = 1;
  for (int i = 2; i <= n; i++) total *= i;

  // Prepare result matrix
  IntegerMatrix result(total, n);

  // Sort first to ensure lexicographic order (required for next_permutation)
  std::sort(a.begin(), a.end());

  int row = 0;
  do {
    for (int j = 0; j < n; j++) {
      result(row, j) = a[j];
    }
    row++;
  } while (std::next_permutation(a.begin(), a.end()));

  return result;
}



// [[Rcpp::export]]
int pi_i2(IntegerVector pat) {
  int a = pat.size() - 1;

  // Create sequence from 0 to a
  IntegerVector seq_vec(a + 1);
  for (int i = 0; i <= a; i++) {
    seq_vec[i] = i;
  }

  // Get all permutations using perm2 function
  DataFrame op = perm2(seq_vec);

  // Convert DataFrame to matrix for easier comparison
  int n_rows = op.nrows();
  int n_cols = op.size();

  // Search for matching row
  for (int i = 0; i < n_rows; i++) {
    bool match = true;
    for (int j = 0; j < n_cols; j++) {
      IntegerVector col = op[j];
      if (col[i] != pat[j]) {
        match = false;
        break;
      }
    }
    if (match) {
      return i + 1; // R uses 1-based indexing
    }
  }

  // Return NA if no match found
  return NA_INTEGER;
}
