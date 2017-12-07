#ifndef NANIAR_NANIAR_H
#define NANIAR_NANIAR_H

namespace naniar{

  template <int RTYPE>
  bool isna( typename Rcpp::Vector<RTYPE>::stored_type x ){
    return Rcpp::Vector<RTYPE>::is_na(x) ;
  }

  typedef union {
    double value;
    unsigned int word[2];
  } ieee_double;

  template <>
  inline bool isna<REALSXP>( double x){
    if (isnan(x)) {
      ieee_double y;
      y.value = x;
      return (y.word[0] == 1954);
    }
    return false ;
  }

}

#endif
