#include <Rcpp.h>
#include <RcppParallel.h>

using namespace Rcpp;

// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::depends(RcppParallel)]]

class NaCounter{
public:
  NaCounter( DataFrame df, IntegerVector indices ) :
    columns(indices.size()),
    n(df.nrow()),
    n_miss( no_init(n) )
  {
    for( int i=0; i<indices.size(); i++){
      columns[i] = df[ indices[i] - 1];
    }
  }

  IntegerVector get(){
    // not worth doing it in parallel if too small
    // the number is arbitrary, may not be the best
    if( n > 10000 ){
      process_parallel() ;
    } else {
      process_serial() ;
    }

    return n_miss ;
  }

private:

  std::vector<SEXP> columns ;
  int n ;
  IntegerVector n_miss ;

  // not using parallelization when not worth it
  // (i.e. the overhead of parallelisation > performance gain)
  void process_serial(){
    process_chunk( 0, n ) ;
  }

  // parallel algorithm
  void process_parallel(){
    tbb::parallel_for( tbb::blocked_range<int>(0, n), [this]( const tbb::blocked_range<int>& r){
      this->process_chunk(r.begin(), r.end()) ;
    });
  }

  // process the chunk between indices begin and end
  // this is used by both parallel and serial versions
  void process_chunk( int begin, int end ){
    std::fill( n_miss.begin() + begin, n_miss.begin() + end, 0 ) ;
    std::for_each( columns.begin(), columns.end(), [&]( SEXP x ){
      par_count_na_dispatch( n_miss, x, begin, end ) ;
    }) ;
  }

  // dispatches on the type of x to call the appropriate template
  bool par_count_na_dispatch( IntegerVector& n_miss, SEXP x, int begin, int end ){
    switch( TYPEOF(x) ){
    // only dealing with the types of vectors where NA makes sense
    // i.e. there's no concept of missing data for RAWSXP or VECSXP
    case INTSXP: return par_count_na_template<INTSXP>(x, begin, end) ;
    case LGLSXP: return par_count_na_template<LGLSXP>(x, begin, end) ;
    case REALSXP: return par_count_na_template<REALSXP>(x, begin, end) ;
    case STRSXP: return par_count_na_template<STRSXP>(x, begin, end) ;
    case CPLXSXP: return par_count_na_template<CPLXSXP>(x, begin, end) ;
    }
    return false ;
  }

  template <int RTYPE>
  bool par_count_na_template( SEXP x, int begin, int end){
    auto p_x = Rcpp::internal::r_vector_start<RTYPE>(x) + begin ;
    auto p_miss = n_miss.begin() + begin  ;

    // incrementing the n_miss when the value in x is NA
    for( int i=begin; i<end; i++, p_miss++, p_x++){
      *p_miss += Vector<RTYPE>::is_na(*p_x) ;
    }

    return true ;
  }

};


// [[Rcpp::export]]
IntegerVector par_count_na_cpp__impl(DataFrame df, IntegerVector indices) {
  return NaCounter(df, indices).get() ;
}
