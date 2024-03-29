% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get-moch.R
\name{get_moch}
\alias{get_moch}
\title{Months of Operating Cash on Hand}
\usage{
get_moch( df, cash = 'F9_10_ASSET_CASH_EOY', 
short.invest = 'F9_10_ASSET_SAVING_EOY', 
pledges.receive = 'F9_10_ASSET_PLEDGE_NET_EOY', 
accounts.receive = 'F9_10_ASSET_ACC_NET_EOY', 
tot.func.exp = 'F9_09_EXP_TOT_TOT', 
dda = 'F9_09_EXP_DEPREC_TOT', numerator = NULL, denominator = NULL, winsorize = 0.98 )
}
\arguments{
\item{df}{A \code{data.frame} containing the required field for computing the metric. The metric will be appended to this dataset.}

\item{cash}{A character string indicating the column name for cash, EOY (On 990: Part X, line 1B; On EZ: Part II, line 22B (cash and short-term investments only)) with the default name supplied.}

\item{short.invest}{A character string indicating the column name for short-term investments, EOY (On 990: Part X, line 2B; On EZ: Part II, line 22B (cash and short-term investments only)) with the default name supplied.}

\item{pledges.receive}{A character string indicating the column name for pledges and grant receivables, EOY On 990: Part X, line 3B; On EZ: Not Available) with the default name supplied.}

\item{accounts.receive}{A character string indicating the column name for accounts receivables, EOY (On 990: Part X, line 4B; On EZ: Not Available) with the default name supplied.}

\item{tot.func.exp}{A character string indicating the column name for total functional expenses (On 990: Part IX, line 25A; On EZ: Not Available) with the default name supplied.}

\item{dda}{A character string indicating the column name for depreciation, depletion, and amortization (On 990: Part IX, line 22A; On EZ: Not Available) with the default name supplied.}

\item{numerator}{A character string indicating the user-supplied column name for a pre-aggregated variable for the numerator (CHANGE). Do not combine with numerator column component arguments (\code{cash}, \code{short.invest},\code{pledges.receive}, \code{accounts.receive}). Users may also use this argument to supply the column variable for EZ-filers: cash, savings, and investment, EOY On EZ: Part II, line 22B (cash and short-term investments only)).}

\item{denominator}{A character string indicating the user-supplied column name for a pre-aggregated variable for the denominator (CHANGE). Do not combine with denominator column component arguments (\code{tot.func.exp}, \code{dda}). Users may also use this argument to supply the column variable for EZ-filers: Total operating expenses, EOY (On EZ: Part I, line 17 (operating expenses only)).}

\item{winsorize}{The winsorization value (between 0 and 1), defaults to 0.98 which winsorizes at 99th and 1st percentile values.}
}
\value{
Object of class \code{data.frame}: the original dataframe with the months of operating cash on hand (\code{moch}),
a winsorized version (\code{moch.w}), a standardized z-score version (\code{moch.z}),
and a percentile version (\code{moch.p}).
}
\description{
Calculate the months of operating cash on hand and append it to the dataframe.
}
\details{
Months of cash on hand is the number of months that an organization can continue to pay its
operating expenses, given the amount of cash available. This metric is good to review at startup of
organization, during periods of low revenues, and prior to undertaking new major activity.The metric is
limited though in the following ways. First, it is based on an average monthly cash outflow, which is not
really the case. Instead, cash tends to be spent in a lumpy manner, such as when rent or payroll are paid.
Also, management tends to take drastic action to reduce expenses as cash reserves decline, so that the
actual months of operation tend to be longer than indicated by this ratio. Thus, it is better to use a
detailed cash flow analysis to determine the precise duration of the available cash, with regular updates.
}
\examples{
library( fiscal )
x1 <- rnorm( 1000,100,30 )
x2 <- rnorm( 1000,200,30 )
x3 <- rnorm( 1000,200,30 )
x4 <- rnorm( 1000,100,30 )
x5 <- rnorm( 1000,200,30 )
x6 <- rnorm( 1000,200,30 )
dat <- data.frame( x1, x2, x3, x4, x5, x6 )

# specify own column names
d <- get_moch( df=dat, cash='x1', short.invest='x2', pledges.receive='x3', accounts.receive='x4', tot.func.exp='x5', dda = 'x6', winsorize=0.98 )

head( d )

# run with default column names
dat_01 <- dat

colnames( dat_01 ) <- c( "F9_10_ASSET_CASH_EOY", "F9_10_ASSET_SAVING_EOY", "F9_10_ASSET_PLEDGE_NET_EOY",
                         "F9_10_ASSET_ACC_NET_EOY", "F9_09_EXP_TOT_TOT", "F9_09_EXP_DEPREC_TOT" )


d <- get_moch( dat_01 )

# coerce one column to factor
dat_01$F9_10_ASSET_ACC_NET_EOY <- as.factor( dat_01$F9_10_ASSET_ACC_NET_EOY )

d <- get_moch( dat_01 )

# winsorize at 0.025 and 0.975 percentiles instead of 0.01 and 0.99
d <- get_moch( df=dat, cash='x1', short.invest='x2', pledges.receive='x3', accounts.receive='x4', tot.func.exp='x5', dda = 'x6', winsorize = 0.95 )

d <- get_moch( dat_01, winsorize = 0.95 )

# aggregate variables into single numerator and denominator
x.den <- x5 + x6
x.num <- x1 + x2 + x3 + x4

dat_02 <- cbind( dat, x.den, x.num)

d <- get_moch( dat_02, numerator = "x.num", denominator = "x.den" )

# using 990 data
data( part010810 )
d <- get_moch( df = part010810 )

# now coerce one of the variables to numeric
part010810$F9_09_EXP_TOT_TOT <- as.character( part010810$F9_09_EXP_TOT_TOT )

d <- get_moch( df = part010810 )

\dontrun{
## Errors ## 
# incorrectly specify denominator
get_moch( df = dat_02, cash = 'x1', short.invest = 'x2', pledges.receive = 'x3', accounts.receive = 'x4', 
          tot.func.exp = NULL, numerator = NULL, denominator = NULL, winsorize=0.98 )

# incorrectly specify numerator with conflicting arguments
get_moch( df = dat_02, cash = 'x1', short.invest = 'x2', pledges.receive = 'x3', accounts.receive = 'x4', 
          tot.func.exp = NULL, dda = NULL, numerator = 'x.num', denominator = 'x.den' , winsorize=0.98 )


# incorrectly specify denominator with conflicting arguments
get_moch( df = dat_02, cash = 'x1', short.invest = 'x2', pledges.receive = 'x3', accounts.receive = 'x4', 
          tot.func.exp = 'x5', dda = 'x6', numerator = NULL, denominator = 'x.den' , winsorize=0.98 )  

# supplying no arguments for the numerator
get_moch( df = dat_02, cash = NULL, short.invest = NULL, pledges.receive = NULL, accounts.receive = NULL, 
          tot.func.exp = 'x5', dda = 'x6', numerator = NULL, denominator = NULL , winsorize=0.98 )  

# supplying no arguments for the denominator
get_moch( df = dat_02, cash = 'x1', short.invest = 'x2', pledges.receive = 'x3', accounts.receive = 'x4', 
          tot.func.exp = NULL, dda = NULL, numerator = NULL, denominator = NULL , winsorize=0.98 )  

# supplying argument for one of the parameters in the numerator that is greater than length 1
get_moch( df = dat_02, cash = 'x1', short.invest = 'x2', pledges.receive = 'x3', accounts.receive = 'x4', 
          tot.func.exp = c( 'x5', 'x6' ), dda = 'x6', numerator = NULL, denominator = NULL, winsorize=0.98 )

get_moch( df = dat_02, cash = NULL, short.invest = NULL, pledges.receive = NULL, accounts.receive = NULL, 
          tot.func.exp = NULL, dda = NULL, numerator = c( 'x5', 'x6' ), denominator = 'x.den' , winsorize=0.98 )
          }  
}
