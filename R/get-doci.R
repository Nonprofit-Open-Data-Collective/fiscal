###---------------------------------------------------
###   DAYS OF OPERATING CASH AND INVESTMENTS
###---------------------------------------------------

#' @title
#' Days of Operating Cash and Investments
#'
#' @description
#' Calculate the days of operating cash and investments and append it to the dataframe. 
#'
#' @param df A dataframe containing the required field fdoci computing the metric. The metric will be appended to this dataset.
#' @param equity A character string indicating the column name for unrestricted net assets, EOY (On 990: Part X, line 27B; On EZ: Not Available).
#' @param invest A character string indicating the column name for investment income (On 990: Part X, Line 8(A); On EZ: Not Available).
#' @param lbe A character string indicating the column name for lands, buildings, and equipment (On 990: Part X, Line 10(c)(B); On EZ: Not Available).
#' @param mnp A character string indicating the column name for mortgages and notes payables (On 990: Part X, Line 23(B); On EZ: Not Available).
#' @param daily.av.exp A character string indicating the column name for daily average expenses or total expenses (On 990: Part IX, Line 25(A); On EZ: Not Available).
#' @param dda Depreciation, depletion, and amortization (On 990: Part IX, line 22A; On EZ: Not Available).
#'
#' @return The original dataframe appended with the days of operating cash and investments (`doci`), 
#'  a winsorized version (`doci.w`), a standardized z-score version (`doci.z`), 
#'  and a percentile version (`doci.p`).   
#'
#' @details The days of operating cash and investment measures how long an organization would be able to 
#' continue operating its programs and investing at its current rate based on its average per-day expenses 
#' (excluding depreciation). Note: This ratio is the same as month of cash on hand but it is in days and 
#' excludes land and mortgages from the operating cash and investment numerator. The reason that land and 
#' mortgage expenses are deducted from the numerator is because those are non-liquid or necessary expenses 
#' and shouldn't truly be considered free cash for operating and investment. The higher the number of days 
#' from this metric, the more protected the system will be against revenue shocks, but the target value is 
#' subjective. Generally, a system should aim to maintain several months’ worth of cash on hand and at the 
#' very least exceed the length of the billing period (usually 30 or 60 days).
#' 
#' @examples
#' x1 <- rnorm( 1000,100,30 )
#' x2 <- rnorm( 1000,200,30 )
#' x3 <- rnorm( 1000,200,30 )
#' x4 <- rnorm( 1000,200,30 )
#' x5 <- rnorm( 1000,200,30 )
#' x6 <- rnorm( 1000,200,30 )
#'
#' dat <- data.frame( x1, x2, x3, x4, x5, x6 )
#'
#' a <- get_doci( df=dat, equity='x1', invest = 'x2',
#'               lbe='x3', mnp='x4', daily.av.exp='x5', dda='x6' )
#'
#' # #zero in the denominator
#' x5[ c(1:10) ] <- 0
#' x6[ c(1:10) ] <- 0
#' dat <- data.frame( x1, x2, x3, x4, x5, x6 )
#'
#' b <- get_doci( df=dat, equity='x1', invest = 'x2',
#'               lbe='x3', mnp='x4', daily.av.exp='x5', dda='x6' )
#'
# winsorize at 0.025 and 0.975 percentiles instead of 0.01 and 0.99
#' c <- get_doci( df=dat, equity='x1', invest = 'x2',
#'               lbe='x3', mnp='x4', daily.av.exp='x5', dda='x6', winsorize = 0.975 )
#'
#' 
#' @expdocit
get_doci <- function( df, equity, invest, lbe, mnp, daily.av.exp, dda, winsorize=0.98 )
{
  
  # checks
  if( winsorize > 1 | winsorize < 0 )
  { stop( "winsorize argument must be 0 < w < 1" ) }
  
  num <- df[[ equity ]] + df[[ invest ]] + df[[ lbe ]] + df[[ mnp ]]
  den <- ( df[[ daily.av.exp ]] + df[[ dda ]] ) / 365
  
  # can't divide by zero
  print( paste0( "Denominator cannot be zero: ", sum( den==0 ), " cases have been replaced with NA." ) )
  den[ den == 0 ] <- NA 
  
  doci <- num / den
  
  top.p    <- 1 - (1-winsorize)/2
  bottom.p <- 0 + (1-winsorize)/2
  top      <- quantile( doci, top.p, na.rm=T )
  bottom   <- quantile( doci, bottom.p, na.rm=T )
  doci.w    <- doci
  doci.w[ doci.w > top    ] <- top
  doci.w[ doci.w < bottom ] <- bottom
  
  doci.n <- scale( doci.w )
  
  doci.p <- dplyr::ntile( doci, 100 )
  
  DOCI <- data.frame( doci, doci.w, doci.n, doci.p )
  
  print( summary( DOCI ) )
  
  par( mfrow=c(2,2) )
  plot( density( doci,   na.rm=T ), main="Days of Operating Cash and Investments (DOCI)" )
  plot( density( doci.w, na.rm=T ), main="DOCI Winsorize" )
  plot( density( doci.n, na.rm=T ), main="DOCI Standardized as Z" )
  plot( density( doci.p, na.rm=T ), main="DOCI as Percentile" )
  
  df.doci <- cbind( df, DOCI )
  return( df.doci )
}
