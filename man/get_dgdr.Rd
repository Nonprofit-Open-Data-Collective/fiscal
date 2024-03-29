% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get-dgdr.R
\name{get_dgdr}
\alias{get_dgdr}
\title{Donation/Grant Dependence Ratio}
\usage{
get_dgdr( df, total.contributions = "F9_08_REV_CONTR_TOT", fund.income = "F9_08_REV_OTH_FUNDR_NET_TOT", total.revenue = "F9_08_REV_TOT_TOT", numerator = NULL, denominator = NULL, winsorize = 0.98 )
}
\arguments{
\item{df}{A \code{data.frame} containing the required field for computing the metric. The metric will be appended to this dataset.}

\item{total.contributions}{Column name for total contributions (can be quoted or unquoted) (On 990: Part VIII, Line 1h(A); On EZ: Not Available) with the default name supplied.}

\item{fund.income}{Column name for fundraising income (can be quoted or unquoted) (On 990: Part VIII, Line 8c(A); On EZ: Not Available) with the default name supplied.}

\item{total.revenue}{Column name for fundraising income (can be quoted or unquoted) (On 990: Part VIII, Line 12A; On EZ: Part I, Line 9) with the default name supplied.}

\item{numerator}{A character string indicating the user-supplied column name for a pre-aggregated variable for the numerator (CHANGE). Do not combine with numerator column component arguments (\code{total.contributions}, \code{fund.income}).}

\item{denominator}{A character string indicating the user-supplied column name for a pre-aggregated variable for the denominator (CHANGE). Do not combine with denominator column component arguments (\code{total.revenue}).}

\item{winsorize}{The winsorization value (between 0 and 1), defaults to 0.98 which winsorizes at 99th and 1st percentile values.}
}
\value{
Object of class \code{data.frame}: the original dataframe appended with the donation/grant dependence ratio (\code{dgdr}),
a winsorized version (\code{dgdr.w}), a standardized z-score version (\code{dgdr.z}),
and a percentile version (\code{dgdr.p}).
}
\description{
Calculate the donation/grant dependence ratio and append it to the dataframe.
}
\details{
This metric shows how much of an organization’s total revenues come from contributions, government
grants, and special event revenues. High levels in this indicator are undesirable since that means that an
organization’s revenues are volatile insofar as it is dependent on contributions that are highly likely to
contract during economic downturns. Low values in this indicator mean an organization is not dependent on
donations. Note: computation of this metric is available to only 990 filers and not for 990-EZ filers. The default
inputs use column names for variables available only to 990 filers.
}
\examples{
library( fiscal )
x1 <- rnorm( 1000, 100, 30 )
x2 <- rnorm( 1000, 200, 30 )
x3 <- rnorm( 1000, 200, 30 )
x3[ c( 15, 300, 600 ) ] <- 0

dat <- data.frame( x1, x2, x3 )

# specify own column names
d <- get_dgdr( df = dat, total.contributions = "x1", fund.income = "x2", total.revenue = "x3" )


head( d ) 

# run with default column names
x1[ seq( from = 1, to = 1000, 50 ) ] <- NA
x2[ seq( from = 1, to = 1000, 71 ) ] <- NA

dat_01 <- data.frame( x1, x2, x3 )

colnames( dat_01 ) <- c( "F9_08_REV_CONTR_TOT", "F9_08_REV_OTH_FUNDR_NET_TOT", "F9_08_REV_TOT_TOT" )

d <- get_dgdr( dat_01 )

# coerce one column to factor
dat_01$F9_08_REV_OTH_FUNDR_NET_TOT <- as.factor( dat_01$F9_08_REV_OTH_FUNDR_NET_TOT )

d <- get_dgdr( dat_01 )

# winsorize at 0.025 and 0.975 percentiles instead of 0.01 and 0.99
d <- get_dgdr( df = dat, total.contributions = "x1", fund.income = "x2", total.revenue = "x3", winsorize = 0.95 )

d <- get_dgdr( dat_01, winsorize = 0.95 )


# using 990 data
data( part010810 )
d <- get_dgdr( df = part010810 )

# now coerce one of the variables to numeric
part010810$F9_09_EXP_TOT_TOT <- as.character( part010810$F9_09_EXP_TOT_TOT )

d <- get_dgdr( df = part010810 )

\dontrun{
## Errors ##

# numerator not specified
d <- get_dgdr( df = dat, total.contributions = NULL, fund.income = NULL, total.revenue = "x3" )

# denominator not specified
d <- get_dgdr( df = dat, total.contributions = "x1", fund.income = "x2", total.revenue = NULL )

# neither numerator nor denominator specified
d <- get_dgdr( df = dat, total.contributions = NULL, fund.income = NULL, total.revenue = NULL )

# column names vector not of correct length
d <- get_dgdr( df = dat, total.contributions = c("e","b","c"), fund.income = "x2", total.revenue = "e")

# column names vector not of correct length
d <- get_dgdr( df = dat, total.contributions = "e", total.revenue = c( "e", "b", "c"), fund.income = "x2" )
}

}
