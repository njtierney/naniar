#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::plugins(cpp11)]]

template <int RTYPE>
bool count_na_template( IntegerVector& n_miss, Vector<RTYPE> x){
  using STORAGE = typename Vector<RTYPE>::stored_type ;
  int n = x.size() ;

  // incrementing the n_miss when the value in x is NA
  std::transform( x.begin(), x.end(), n_miss.begin(), n_miss.begin(), [](STORAGE y, int n){
    return n + Vector<RTYPE>::is_na(y) ;
  } ) ;
  return true ;
}

// dispatches on the type of x to call the appropriate template
bool count_na_dispatch( IntegerVector& n_miss, SEXP x ){
  switch( TYPEOF(x) ){
  // only dealing with the types of vectors where NA makes sense
  // i.e. there's no concept of missing data for RAWSXP or VECSXP
  case INTSXP: return count_na_template<INTSXP>(n_miss, x) ;
  case REALSXP: return count_na_template<REALSXP>(n_miss, x) ;
  case STRSXP: return count_na_template<STRSXP>(n_miss, x) ;
  case CPLXSXP: return count_na_template<CPLXSXP>(n_miss, x) ;
  }
  return false ;
}

// [[Rcpp::export]]
IntegerVector count_na_cpp__impl(DataFrame df, IntegerVector indices) {
  int n = df.nrow() ;
  // allocating and initializing values to 0
  // this will be passed as reference to the `participate` function below
  IntegerVector n_miss(n) ;

  int nc = indices.size() ;
  for(int i=0; i<nc; i++){
    // we assume that the `indices` has been passed correctly
    // and as 1-based (R-style) indexing, so -1
    count_na_dispatch( n_miss, df[indices[i]-1] ) ;
  }
  return n_miss ;

}
