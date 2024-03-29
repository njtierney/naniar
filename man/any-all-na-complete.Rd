% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/any-na-complete.R
\name{any-all-na-complete}
\alias{any-all-na-complete}
\alias{any_na}
\alias{any_miss}
\alias{any_complete}
\alias{all_na}
\alias{all_miss}
\alias{all_complete}
\title{Identify if there are any or all missing or complete values}
\usage{
any_na(x)

any_miss(x)

any_complete(x)

all_na(x)

all_miss(x)

all_complete(x)
}
\arguments{
\item{x}{an object to explore missings/complete values}
}
\description{
It is useful when exploring data to search for cases where
there are \strong{any} or \strong{all} instances of missing or complete values. For
example, these can help you identify and potentially remove or keep
columns in a data frame that are all missing, or all complete.

For the \strong{any} case, we provide two functions: \code{any_miss} and
\code{any_complete}. Note that \code{any_miss} has an alias, \code{any_na}. These both
under the hood call \code{anyNA}. \code{any_complete} is the complement to
\code{any_miss} - it returns TRUE if there are any complete values. Note
that in a dataframe \code{any_complete} will look for complete cases, which
are complete rows, which is different to complete variables.

For the \strong{all} case, there are two functions: \code{all_miss}, and
\code{all_complete}.
}
\examples{

# for vectors
misses <- c(NA, NA, NA)
complete <- c(1, 2, 3)
mixture <- c(NA, 1, NA)

all_na(misses)
all_na(complete)
all_na(mixture)
all_complete(misses)
all_complete(complete)
all_complete(mixture)

any_na(misses)
any_na(complete)
any_na(mixture)

# for data frames
all_na(airquality)
# an alias of all_na
all_miss(airquality)
all_complete(airquality)

any_na(airquality)
any_complete(airquality)

# use in identifying columns with all missing/complete

library(dplyr)
# for printing
aq <- as_tibble(airquality)
aq
# select variables with all missing values
aq \%>\% select(where(all_na))
# there are none!
#' # select columns with any NA values
aq \%>\% select(where(any_na))
# select only columns with all complete data
aq \%>\% select(where(all_complete))

# select columns where there are any complete cases (all the data)
aq \%>\% select(where(any_complete))

}
\seealso{
\code{\link[=all_miss]{all_miss()}} \link{all_complete}
}
