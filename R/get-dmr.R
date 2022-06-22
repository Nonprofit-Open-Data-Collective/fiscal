###---------------------------------------------------
###   DEBT MANAGEMENT RATIO
###---------------------------------------------------

#' @title
#' Debt Management Ratio 
#'
#' @description
#' Calculate the debt management ratio and append it to the dataframe. 
#'
#' @param df A dataframe containing the required field for computing the metric. The metric will be appended to this dataset.
#' @param liabilities A character string indicating the column name for total liabilities, EOY (On 990: Part X, line 26B; On EZ: Part II, line 26B) with the default name supplied.
#' @param net.assets A character string indicating the column name for unrestricted net assets, EOY (On 990: Part X, line 27B; On EZ: Not Available) with the default name supplied.
#' @param winsorize The winsorization value (between 0 and 1), defaults to 0.98 which winsorizes at 99th and 1st percentile values.   
#' 
#' @return The original dataframe appended with the debt management ratio (`dmr`), 
#'  a winsorized version (`dmr.w`), a standardized z-score version (`dmr.z`), 
#'  and a percentile version (`dmr.p`).   
#'
#' @details This metric measures how much an organization is relying on funding from lending entities and the 
#' amount of free, unrestricted cash it has to pay back those loans. A high value in this metric could mean 
#' an organization is highly leveraged and thus that it has reduced ability to borrow more money in the future. 
#' Conversely, a low value indicates the organization may not be leveraging its assets to achieve the most 
#' growth and impact it could or that there is a dearth of credit available from lending entities.
#' Note: computation of this metric is available to both 990 and 990-EZ filers.
#' 
#' @examples
#' x1 <- rnorm( 1000,100,30 )
#' x2 <- rnorm( 1000,200,30 )
#' x2[ c(15,300,600) ] <- 0
#' 
#' dat <- data.frame( x1,x2 )
#' 
#' # specify own column names
#' d <- get_dmr( df = dat, liabilities = "x1", net.assets = "x2" )
#' 
#' head( d )
#' 
#' # run with default column names
#' dat_01 <- dat
#' 
#' colnames( dat_01 ) <- c( 'F9_10_LIAB_TOT_EOY', 'F9_10_NAFB_UNRESTRICT_EOY' )
#'
#' d <- get_dmr( dat_01 )
#' 
#' # winsorize at 0.025 and 0.975 percentiles instead of 0.01 and 0.99
#' d <- get_dmr( df = dat, liabilities = "x1", net.assets ="x2", winsorize=0.95 )
#' 
#' d <- get_dmr( dat_01, winsorize = 0.95 )
#' 
#' ## Errors ##
#' 
# numerator not specified
#' d <- get_dmr( df = dat, liabilities = NULL, net.assets = 'x2' )
#' 
#' # denominator not specified
#' d <- get_dmr( df = dat, liabilities = 'x1', net.assets = NULL )
#' 
#' # neither numerator nor denominator specified
#' d <- get_dmr( df = dat, liabilities = NULL, net.assets = NULL )
#' 
#' @export
get_dmr <- function( df, liabilities = 'F9_10_LIAB_TOT_EOY', net.assets = 'F9_10_NAFB_UNRESTRICT_EOY', winsorize=0.98 )
{
  # checks
  if( winsorize > 1 | winsorize < 0 )
  { stop( "winsorize argument must be 0 < w < 1" ) }
  
  if( is.null( liabilities )==T & is.null( net.assets )==F )
  { stop( "The numerator has been incorrectly specified. Ensure you are passing the correct data field to the correct argument." ) }
  
  if( is.null( liabilities )==F & is.null( net.assets )==T )
  { stop( "The denominator has been incorrectly specified. Ensure you are passing the correct data field to the correct argument." ) }
  
  if( is.null( liabilities )==T & is.null( net.assets )==T )
  { stop( "The argument fields are empty. Please supply column names for each argument or execute the function with default inputs." ) }
  
  l <- df[[ liabilities ]]
  n <- df[[ net.assets ]]
  
  if( winsorize > 1 | winsorize < 0 )
  { stop( "winsorize argument must be 0 < w < 1" ) }
  
  # can't divide by zero
  print( paste0( "Unrestricted net assets cannot be equal to zero: ", sum( n==0 ), " cases have been replaced with NA." ) )
  n[ n == 0 ] <- NA 
  
  dmr <- l / n
  
  top.p    <- 1 - (1-winsorize)/2
  bottom.p <- 0 + (1-winsorize)/2
  top      <- quantile( dmr, top.p, na.rm=T )
  bottom   <- quantile( dmr, bottom.p, na.rm=T )
  dmr.w    <- dmr
  dmr.w[ dmr.w > top    ] <- top
  dmr.w[ dmr.w < bottom ] <- bottom
  
  dmr.n <- scale( dmr.w )
  
  dmr.p <- dplyr::ntile( dmr, 100 )
  
  DMR <- data.frame( dmr, dmr.w, dmr.n, dmr.p )
  
  print( summary( DMR ) )
  
  par( mfrow=c(2,2) )
  plot( density(dmr,   na.rm=T), main="Debt Management Ratio (DMR)" )
  plot( density(dmr.w, na.rm=T), main="DMR Winsorized" )
  plot( density(dmr.n, na.rm=T), main="DMR Standardized as Z" )
  plot( density(dmr.p, na.rm=T), main="DMR as Percentile" )
  
  df.dmr <- cbind( df, DMR )
  return( df.dmr )
}
