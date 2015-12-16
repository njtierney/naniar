#' missing_summary
#'
#' @description \code{missing_summary} computes the numeric summary of the missingness
#'
#' @param dat A data frame.
#'
#' @return A list including (1) a data frame 'missingsummary' that provides
#' a table of missingness; (2) the total missing percentage; (3) the percent
#' of variables that contain missing values; (4) the ratio of observations
#' that have missings.
#'
#' @author Xiaoyue Cheng <\email{xycheng@@iastate.edu}>
#'
#' @examples
#' data(tao)
#' compute_missing_pct(tao)
#'
#' @export
compute_missing_pct <- function(dat){

  stopifnot(is.data.frame(dat))

  n <- ncol(dat)

  totalmissingpct <- mean(is.na(dat))

  varmissingpct <- mean(sapply(dat,function(avec){any(is.na(avec))}))*100

  casemissingpct <- 1-mean(complete.cases(dat))*100

  No_of_Case_missing <- table(apply(dat, 1, function(avec){sum(is.na(avec))}))

  No_of_Case <- rep(0,(n+1))

  No_of_Case[n+1-as.integer(names(No_of_Case_missing))]=No_of_Case_missing[names(No_of_Case_missing)]

  No_of_Case[n+1] <- sum(complete.cases(dat))

  variablesummary <- data.frame(variable=colnames(dat),
                        pct=round(apply(dat, 2, function(avec){sum(is.na(avec))})/nrow(dat)*100, 2))

  variablesummary <- variablesummary[order(variablesummary$pct, decreasing=TRUE), ]

  missingsummary <- data.frame(No_of_miss_by_case=n:0,
                               No_of_Case=No_of_Case,
                               Percent=as.character(round(No_of_Case/nrow(dat)*100,1)))

  missingsummary <- missingsummary[order(missingsummary$No_of_miss_by_case, decreasing=FALSE),]

  return(
    list(
      missingsummary = missingsummary,
      variablesummary = variablesummary,
      vartotalmissingpct = totalmissingpct,
      varmissingpct = varmissingpct,
      casemissingpct = casemissingpct
      )
    )
}
