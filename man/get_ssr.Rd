% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get-ssr.R
\name{get_ssr}
\alias{get_ssr}
\title{Self Sufficiency Ratio}
\usage{
get_ssr( df, prog.serv.rev = c( "F9_08_REV_PROG_TOT_TOT", "F9_01_REV_PROG_TOT_CY"), 
total.expense = c( "F9_09_EXP_TOT_TOT", "F9_01_EXP_TOT_CY"), 
winsorize = 0.98 )
}
\arguments{
\item{df}{A \code{data.frame} containing the required field for computing the metric. The metric will be appended to this dataset.}

\item{prog.serv.rev}{Column name(s) for program service revenue (must be quoted), EOY (Part 8, Line 2g(A); On EZ: Part 1, Line 2). If specifying column names for both PC and EZ scope variables, they must be specified as a vector of class character.}

\item{total.expense}{Column name(s) for total expenses (must be quoted), EOY (On 990: Part IX, line 25A; On EZ: Part 1, Line 17). If specifying column names for both PC and EZ scope variables, they must be specified as a vector of class character.}

\item{winsorize}{The winsorization value (between 0 and 1), defaults to 0.98 which winsorizes at 99th and 1st percentile values.}
}
\value{
Object of class \code{data.frame}: the original dataframe appended with the self sufficiency ratio (\code{ssr}),
a winsorized version (\code{ssr.w}), a standardized z-score version (\code{ssr.z}),
and a percentile version (\code{ssr.p}).
}
\description{
Calculate the self sufficiency ratio and append it to the dataframe.
}
\details{
The Self Sufficiency Ratio measures the proportion of operating expenses that are covered by earned income.
This metric is a good measure of how long an organization can survive without grants. Higher values
mean it is more self-sufficient, meaning it could cover its costs longer without collecting any grants,
rents, royalties, or sales of inventory. This ratio is primarily useful for organizations that have
earned revenue through developers’ fees, management fees, memberships, or tuition. Higher values mean
organizations have more autonomy and flexibility. They generally improve over time as an organization
grows. In the early stages, these ratios tend to be lower but the goal is to make them as high as possible.
}
\examples{
library( fiscal )
x1 <- rnorm( 1000, 100, 30 )
x2 <- rnorm( 1000, 200, 30 )
x2[ c( 15, 300, 600 ) ] <- 0

dat <- data.frame( x1, x2 )

# specify own column names
d <- get_ssr( df = dat, prog.serv.rev = "x1", total.expense = "x2" )


head( d )

# run with default column names
x3 <- rnorm( 1000, 100, 30 )
x4 <- rnorm( 1000, 200, 30 )
x3[ seq( from = 1, to = 1000, 50 ) ] <- NA
x4[ seq( from = 1, to = 1000, 71 ) ] <- NA

dat_01 <- data.frame( x1, x2, x3, x4 )

colnames( dat_01 ) <- c( "F9_08_REV_PROG_TOT_TOT", "F9_01_REV_PROG_TOT_CY",
                         "F9_09_EXP_TOT_TOT", "F9_01_EXP_TOT_CY" )

d <- get_ssr( dat_01 )

# specify only PC variables
d <- get_ssr( dat_01, prog.serv.rev = "F9_08_REV_PROG_TOT_TOT", total.expense = "F9_09_EXP_TOT_TOT" )

# coerce one column to factor
dat_01$F9_08_REV_PROG_TOT_TOT <- as.factor( dat_01$F9_08_REV_PROG_TOT_TOT )

d <- get_ssr( dat_01 )

# winsorize at 0.025 and 0.975 percentiles instead of 0.01 and 0.99
d <- get_ssr( df = dat, prog.serv.rev = "x1", total.expense = "x2", winsorize = 0.95 )

d <- get_ssr( dat_01, winsorize = 0.95 )

# assume only one PC variable for the numerator or denominator is present in the dataset and we run with default parameters
dat_02 <- dat_01

colnames( dat_02 ) <- c( "F9_08_REV_PROG_TOT_TOT", "F9_01_REV_PROG_TOT_CY",
                         "x", "F9_01_EXP_TOT_CY" )


d <- get_ssr( dat_02, winsorize = 0.95 )

colnames( dat_02 ) <- c( "F9_08_REV_PROG_TOT_TOT", "x",
                         "F9_09_EXP_TOT_TOT", "F9_01_EXP_TOT_CY" )

d <- get_ssr( dat_02, winsorize = 0.95 )


# using 990 data
data( part010810 )
d <- get_ssr( df = part010810 )

# now coerce one of the variables to numeric
part010810$F9_01_EXP_TOT_CY <- as.character( part010810$F9_01_EXP_TOT_CY )

d <- get_ssr( df = part010810 )

\dontrun{
## Errors ##

# numerator not specified
d <- get_ssr( df = dat, prog.serv.rev = NULL, total.expense = "x2" )

# denominator not specified
d <- get_ssr( df = dat, prog.serv.rev = "x1", total.expense = NULL )

# neither numerator nor denominator specified
d <- get_ssr( df = dat, prog.serv.rev = NULL, total.expense = NULL )

# column names vector not of correct length
d <- get_ssr( df = dat, prog.serv.rev = c("e","b","c"), total.expense = "e")

# column names vector not of correct length
d <- get_ssr( df = dat, prog.serv.rev = "e", total.expense = c( "e", "b", "c") )
}

}
