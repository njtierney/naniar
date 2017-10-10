#include <Rcpp.h>
#include <RcppParallel.h>

using namespace Rcpp;

// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::depends(RcppParallel)]]

namespace naniar {

  enum Result {
    COUNT,
    PROP
  } ;

  template <Result res> struct result {
    using type = IntegerVector ;
  } ;

  template <>
  struct result<PROP>{
    typedef NumericVector type ;
  };

  template <Result res>
  class NaCounter{
  public:
    using ResultVector = typename result<res>::type ;

    NaCounter( DataFrame df ) :
      nc(df.size()),
      columns(nc),
      n(df.nrow()),
      n_miss( no_init(n) )
    {
      for( int i=0; i<nc; i++){
        columns[i] = df[i];
      }
    }

    ResultVector get(){
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
    int nc ;
    std::vector<SEXP> columns ;
    int n ;
    ResultVector n_miss ;

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
    inline void process_chunk( int begin, int end ){
      std::fill( n_miss.begin() + begin, n_miss.begin() + end, 0 ) ;
      std::for_each( columns.begin(), columns.end(), [&]( SEXP x ){
        par_count_na_dispatch( x, begin, end ) ;
      }) ;
      finish_process_chunk( begin, end) ;
    }

    inline void finish_process_chunk( int begin, int end ){} ;

    // dispatches on the type of x to call the appropriate template
    bool par_count_na_dispatch( SEXP x, int begin, int end ){
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

  // this is the only difference.
  template <>
  void NaCounter<PROP>::finish_process_chunk( int begin, int end ){
    std::transform(
      n_miss.begin() + begin, n_miss.begin() + end,
      n_miss.begin() + begin,
      [=](double value){ return value / nc ;}
    ) ;
  }


}

// [[Rcpp::export]]
IntegerVector count_na_cpp(DataFrame df) {
  return naniar::NaCounter<naniar::COUNT>(df).get() ;
}

// [[Rcpp::export]]
NumericVector prop_na_cpp(DataFrame df) {
  return naniar::NaCounter<naniar::PROP>(df).get() ;
}

