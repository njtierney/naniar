#include <Rcpp.h>
#include <RcppParallel.h>

#ifndef NANIAR_PARALLEL_THRESHOLD
#define NANIAR_PARALLEL_THRESHOLD 10000
#endif

using namespace Rcpp;

// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::depends(RcppParallel)]]

namespace naniar {

class PropNaRow {

private:

  // the number of input columns
  int nc ;

  // the actual columns. This is a vector of R objects
  // using this instead of Rcpp::DataFrame to avoid some complexity from Rcpp
  std::vector<SEXP> columns ;

  // the n umber of rows of the data frame, hence the length of the result
  int n ;

  // the result vector, a vector of size `n` with proportions
  NumericVector prop_miss ;

public:

  PropNaRow( DataFrame df ) :
  nc(df.size()),
  columns(nc),
  n(df.nrow()),
  prop_miss( no_init(n) )
  {
    // grab the vectors from the data frame
    for( int i=0; i<nc; i++){
      columns[i] = df[i];
    }
  }

  // make prop_miss using parallelisation if parallel is `true`
  NumericVector get( ){
    if( n > NANIAR_PARALLEL_THRESHOLD ){
      process_parallel() ;
    } else {
      process_serial() ;
    }
    return prop_miss ;
  }

private:

  // serial version
  // makes sense to us this when the overhead of parallelisation > performance gain
  void process_serial(){
    process_chunk( 0, n ) ;
  }

  // parallel algorithm - tbb::parallel_for splits the rows into chunks
  // each chunk is done is parallel
  void process_parallel(){
    tbb::parallel_for( tbb::blocked_range<int>(0, n), [this]( const tbb::blocked_range<int>& r){
      this->process_chunk(r.begin(), r.end()) ;
    });
  }

  // process the chunk between indices begin and end
  // this is used by both parallel and serial versions
  //
  // it fills n_miss[ begin:end ] by counting NA in each columns
  // conceptually this does something like this pseudo code
  //
  // prop_miss[begin:end] <- 0
  // for( x in columns ){
  //   prop_miss[begin:end] <- prop_miss[begin:end] + is.na(x[begin:end])
  // }
  // prop_miss[begin:end] <- prop_miss[begin:end] / nc
  //
  inline void process_chunk( int begin, int end ){

    // filling prop_miss[begin:end] with 0
    // this was not done when we construct n_miss because this way we can do it in parallel
    std::fill( prop_miss.begin() + begin, prop_miss.begin() + end, 0 ) ;

    // for each column (i.e. each elements of `columns`)
    // increment prop_miss[begin:end]
    std::for_each( columns.begin(), columns.end(), [&]( SEXP x ){
      process_one_dispatch( x, begin, end ) ;
    }) ;

    // divide counts by the number of columns to get proportions
    std::transform(
      prop_miss.begin() + begin, prop_miss.begin() + end,
      prop_miss.begin() + begin,
      [=](double value){ return value / nc ;}
    ) ;
  }

  // dispatches on the type of x to call the appropriate template
  bool process_one_dispatch( SEXP x, int begin, int end ){
    switch( TYPEOF(x) ){
    // only dealing with the types of vectors where NA makes sense
    // i.e. there's no concept of missing data for RAWSXP or VECSXP
    case INTSXP:  return process_one_template<INTSXP>(x, begin, end) ;
    case LGLSXP:  return process_one_template<LGLSXP>(x, begin, end) ;
    case REALSXP: return process_one_template<REALSXP>(x, begin, end) ;
    case STRSXP:  return process_one_template<STRSXP>(x, begin, end) ;
    case CPLXSXP: return process_one_template<CPLXSXP>(x, begin, end) ;
    }
    return false ;
  }

  // this is where the actual work is done, conceptually this does
  // n_miss[begin:end] <- n_miss[begin:end] + is.na( x[begin:end] )
  template <int RTYPE>
  bool process_one_template( SEXP x, int begin, int end){
    // pointer to the start of the part of `x` we deal with
    auto p_x = Rcpp::internal::r_vector_start<RTYPE>(x) + begin ;

    // pointer to the start of the part of `n_miss` we make
    auto p_miss = prop_miss.begin() + begin  ;

    // incrementing the n_miss when the value in x is NA
    // similar to :
    //
    // for( i in begin:end ){  prop_miss[i] <- prop_miss[i] + is.na(x[i])  }
    //
    for( int i=begin; i<end; i++, p_miss++, p_x++){
      *p_miss += Vector<RTYPE>::is_na(*p_x) ;
    }

    return true ;
  }

};

}

// [[Rcpp::export]]
NumericVector prop_row_na_cpp(DataFrame df) {
  return naniar::PropNaRow(df).get() ;
}
