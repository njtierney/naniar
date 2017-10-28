#include <Rcpp.h>
#include <RcppParallel.h>

using namespace Rcpp;

// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::depends(RcppParallel)]]

namespace naniar {

class CountNaRow {

private:

  // the number of input columns
  int nc ;

  // the actual columns. This is a vector of R objects
  // using this instead of Rcpp::DataFrame to avoid some complexity from Rcpp
  std::vector<SEXP> columns ;

  // the n umber of rows of the data frame, hence the length of the result
  int n ;

  // the result vector, a vector of size `n` with counts
  IntegerVector n_miss ;

public:

  CountNaRow( DataFrame df ) :
  nc(df.size()), // nc = number of columns of the df (nc = 6 if columns)
  columns(nc),   // columns = multiple vector, one for each column of the dataframe
                 // so, columns[0] is the first column of the dataframe.
  n(df.nrow()),  // n is the number of rows
  n_miss( no_init(n) ) // create n values, either int of dbl, initialise them with
                       // nonsense numbers
  {
    // grab the vectors from the data frame
    for( int i=0; i<nc; i++){ // identify pointers to the columns of the dataframe
      columns[i] = df[i];
    }
  }

  // make n_miss using parallelisation if parallel is `true`
  IntegerVector get( bool parallel ){
    if( parallel ){
      process_parallel() ;
    } else {
      process_serial() ;
    }
    return n_miss ;
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
  // n_miss[begin:end] <- 0
  // for( x in columns ){
  //   n_miss[begin:end] <- n_miss[begin:end] + is.na(x[begin:end])
  // }
  //
  inline void process_chunk( int begin, int end ){

    // filling n_miss[begin:end] with 0
    // this was not done when we construct n_miss because this way we can do it in parallel
    std::fill( n_miss.begin() + begin, n_miss.begin() + end, 0 ) ;


    // for each column (i.e. each elements of `columns`)
    // increment n_miss[begin:end]
    // [&]( SEXP x) is a lambda function, stating it takes an R object x.
    // This is saying, process each of the columns of the dataframe
    // "for each column, do this".

    std::for_each( columns.begin(), columns.end(), [&]( SEXP x ){
      process_one_dispatch( x, begin, end ) ;
      // loop on the columns
      // for each column, call the par_count_na_dispatch function
      // effectively, add 1 to n_miss based on whether there is an NA.
    }) ;

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

  // efficient way of calculating number of rowwise missings
  // this is where the actual work is done, conceptually this does
  // n_miss[begin:end] <- n_miss[begin:end] + is.na( x[begin:end] )
  template <int RTYPE>
  bool process_one_template( SEXP x, int begin, int end){
    // start of the slice of the vector that we want to scan
    // the begin and end are helpers for if this is processed in parallel.
    // pointer to the start of the part of `x` we deal with
    auto p_x = Rcpp::internal::r_vector_start<RTYPE>(x) + begin ;

    // p_miss is a pointer to the start of the thing you use to count.
    // incrementing the n_miss when the value in x is NA
    // this is where the magic happens.
    // pointer to the start of the part of `n_miss` we make
    auto p_miss = n_miss.begin() + begin  ;

    // incrementing the n_miss when the value in x is NA
    // similar to :
    //
    // for( i in begin:end){  n_miss[i] <- n_miss[i] + is.na(x[i])  }
    //
    for( int i=begin; i<end; i++, p_miss++, p_x++){
      // call is_NA for the right R vector type (that is what Vector<RTYPE> does)
      // *p_x refers to value of p_x
      // p_miss is , the result of is_NA is 0 if !NA and 1 if NA
      *p_miss += Vector<RTYPE>::is_na(*p_x) ;
    }

    return true ;
  }

};

}

// [[Rcpp::export]]
IntegerVector count_row_na_cpp(DataFrame df, bool parallel) {
  return naniar::CountNaRow(df).get(parallel) ;
}
