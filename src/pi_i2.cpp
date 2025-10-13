#include <Rcpp.h>
using namespace Rcpp;

// Forward declaration of perm2 (if it's defined later in the same file)
DataFrame perm2(IntegerVector x);

// Or if perm2 returns a different type, adjust accordingly
// For example: IntegerMatrix perm2(IntegerVector x);

// [[Rcpp::export]]
int pi_i(IntegerVector pat) {
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