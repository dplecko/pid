
#include <Rcpp.h>
using namespace Rcpp;
#define FOR(i, a, b) for (int i=(a); i<(b); i++)
#define REP(i, n) FOR(i, 0, n)

// [[Rcpp::export]]
NumericMatrix precalc_bin(double p) {
  
  const int MAX = 30;
  NumericMatrix bin_p(MAX, MAX);
  bin_p(0, 0) = 1;
  REP(n, MAX-1) {
    REP(k, n+1) {
      bin_p(n+1, k) += bin_p(n, k) * (1-p);
      bin_p(n+1, k+1) += bin_p(n, k) * p;
    }
  }
  
  return bin_p;
}