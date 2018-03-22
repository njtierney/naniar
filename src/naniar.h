#ifndef NANIAR_NANIAR_H
#define NANIAR_NANIAR_H

#ifndef NANIAR_PARALLEL_THRESHOLD
#define NANIAR_PARALLEL_THRESHOLD 10000
#endif

namespace naniar{

  template <int RTYPE>
  bool isna( typename Rcpp::Vector<RTYPE>::stored_type x ){
    return Rcpp::Vector<RTYPE>::is_na(x) ;
  }

  template <>
  inline bool isna<REALSXP>( double x){
    return isnan(x) ;
  }

}

#endif
