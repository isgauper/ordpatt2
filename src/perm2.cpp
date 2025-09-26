#include <Rcpp.h>
#include <algorithm>
#include <vector>
using namespace Rcpp;
using namespace std;

// source: https://iq.opengenus.org/generating-all-permutations-of-an-array-using-cpp-stl/


//Display elements of the array
void display(vector<int> a, int n){
  for(int i=0;i<n;i++) Rcout << a[i] << " ";
  Rcout << endl;
}


// [[Rcpp::export]]
List perm2(IntegerVector input, int emb=4) {
  //Converting R vector to C++ vector
  vector<int> a(input.begin(), input.end());
  int n = a.size();

  List result;
  int count = 0;

  do{
    //Display the current permutation
    display(a, n);
  }while(next_permutation(a.begin(), a.end())); //Generate next permutation till it is not lexicographically largest


  return result;
}


