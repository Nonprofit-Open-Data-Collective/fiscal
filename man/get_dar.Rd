% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get-dar.R
\name{get_dar}
\alias{get_dar}
\title{Debt to Asset Ratio}
\usage{
get_dar( df, 
debt = c( "F9_10_LIAB_TOT_EOY", "F9_01_NAFB_LIAB_TOT_EOY" ) , 
assets = c("F9_10_ASSET_TOT_EOY", "F9_01_NAFB_ASSET_TOT_EOY" ), 
winsorize = 0.98 )
}
\arguments{
\item{df}{A \code{data.frame} containing the required field for computing the metric. The metric will be appended to this dataset.}

\item{debt}{Column name(s) for total liabilities (must be quoted). (On 990: Part X, line 26B; On EZ: Part II, line 26B) with the default name supplied.}

\item{assets}{Column name(s) for total assets, EOY (must be quoted). (On 990: Part X, line 16B; On EZ: Part II, line 25B) with the default name supplied.}

\item{winsorize}{The winsorization value (between 0 and 1), defaults to 0.98 which winsorizes at 99th and 1st percentile values.}
}
\value{
Object of class \code{data.frame}: the original dataframe appended with the debt to asset ratio (\code{dar}),
a winsorized version (\code{dar.w}), a standardized z-score version (\code{dar.z}),
and a percentile version (\code{dar.p}).
}
\description{
Calculate the debt to asset ratio and append it to the dataframe.
}
\details{
Total-debt-to-total-assets is a leverage ratio that defines the total
amount of debt relative to assets owned by a company. Using this metric,
analysts can compare one company's leverage with that of other companies
in the same industry. This information can reflect how financially stable
a company is. The higher the ratio, the higher the degree of leverage (DoL) and,
consequently, the higher the risk (\href{Investopedia}{https://www.investopedia.com/terms/t/totaldebttototalassets.asp}).
Note: computation of this metric is available to both 990 and 990-EZ filers.
}
\examples{
library( fiscal )
x1 <- rnorm( 1000,100,30 )
x2 <- rnorm( 1000,200,30 )
x2[ c(15,300,600) ] <- 0

dat <- data.frame( x1,x2 )

# specify own column names
d <- get_dar( df = dat, debt = "x1", assets = "x2" )


head( d )

# run with default column names
x3 <- rnorm( 1000,100,30 )
x4 <- rnorm( 1000,200,30 )
x3[ seq( from = 1, to = 1000, 50 ) ] <- NA
x4[ seq( from = 1, to = 1000, 71 ) ] <- NA

dat_01 <- data.frame( x1, x2, x3, x4 )

colnames( dat_01 ) <- c( "F9_10_LIAB_TOT_EOY", "F9_01_NAFB_LIAB_TOT_EOY",
                         "F9_10_ASSET_TOT_EOY", "F9_01_NAFB_ASSET_TOT_EOY")

d <- get_dar( dat_01 )

# specify only PC variables
d <- get_dar( dat_01, debt = "F9_10_LIAB_TOT_EOY", assets = "F9_10_ASSET_TOT_EOY" )

# coerce one column to factor
dat_01$F9_10_LIAB_TOT_EOY <- as.factor( dat_01$F9_10_LIAB_TOT_EOY )

d <- get_dar( dat_01 )

# winsorize at 0.025 and 0.975 percentiles instead of 0.01 and 0.99
d <- get_dar( df = dat, debt = "x1", assets ="x2", winsorize=0.95 )

d <- get_dar( dat_01, winsorize = 0.95 )

# assume only one PC variable for the numerator or denominator is present in the dataset and we run with default parameters
dat_02 <- dat_01

colnames( dat_02 ) <- c( "F9_10_LIAB_TOT_EOY", "F9_01_NAFB_LIAB_TOT_EOY",
                         "x", "F9_01_NAFB_ASSET_TOT_EOY")


d <- get_dar( dat_02, winsorize = 0.95 )

colnames( dat_02 ) <- c( "F9_10_LIAB_TOT_EOY", "x",
                         "F9_10_ASSET_TOT_EOY", "F9_01_NAFB_ASSET_TOT_EOY")

d <- get_dar( dat_02, winsorize = 0.95 )


# using 990 data

data( part010810 )

d <- get_dar(df=part010810)

# now coerce one of the variables to numeric
part010810$F9_01_NAFB_ASSET_TOT_EOY <- as.character( part010810$F9_01_NAFB_ASSET_TOT_EOY )

d <- get_dar(df=part010810)

\dontrun{
## Errors ##

# numerator not specified
d <- get_dar( df = dat, debt = NULL, assets = "x2")

# denominator not specified
d <- get_dar( df = dat, debt = "x1", assets = NULL )

d <- get_dar( df = dat, debt = NULL, assets = NULL )
}
}
