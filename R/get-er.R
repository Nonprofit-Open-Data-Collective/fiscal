###---------------------------------------------------
###   EQUITY RATIO
###---------------------------------------------------

#' @title
#' Equity Ratio 
#'
#' @description
#' Calculate the equity ratio and append it to the dataframe. 
#'
#' @param df A \code{data.frame} containing the required field for computing the metric. The metric will be appended to this dataset with the default name supplied.
#' @param net.assets A character string indicating the column name for net assets, EOY (On 990: Part X, Line 33B; On EZ: Part I, Line 21) with the default name supplied.
#' @param total.assets A character string indicating the column name for total assets, EOY (On 990: Part X, Line 16B; On EZ: Part II, Line 25B) with the default name supplied.
#' @param winsorize The winsorization value (between 0 and 1), defaults to 0.98 which winsorizes at 99th and 1st percentile values.   
#' 
#' @return Object of class \code{data.frame}: the original dataframe appended with the equity ratio (`er`), 
#'  a winsorized version (`er.w`), a standardized z-score version (`er.z`), 
#'  and a percentile version (`er.p`).   
#'
#' @details This metric indicates how much of an organization’s assets are owned free and clear or how much 
#' equity it has in its total assets. Nonprofits with greater amounts of equity are more flexible in the face 
#' of financial shocks than organizations with comparatively lesser amounts of equity because they can (1) 
#' borrow money from capital markets and (2) convert those assets to cash to offset financial shocks. High 
#' values in this indicator are generally better, as they show that an organization has substantial equity in 
#' its assets. Low or negative values indicate an organization has higher liabilities and is generally more 
#' leveraged and thus more vulnerable to shocks. However, low values also indicate that an organization may 
#' be investing more of its equity for growth. Note: computation of this metric is available to both 990 and 990-EZ filers.
#' 
#' @examples
#' x1 <- rnorm( 1000,100,30 )
#' x2 <- rnorm( 1000,200,30 )
#' x2[ c(15,300,600) ] <- 0
#' 
#' dat <- data.frame( x1,x2 )
#' 
#' # specify own column names
#' d <- get_er( df = dat, net.assets = "x1", total.assets = "x2" )
#' 
#' head( d )
#' 
#' # run with default column names
#' dat_01 <- dat
#' 
#' colnames( dat_01 ) <- c( 'F9_10_NAFB_TOT_EOY', 'F9_10_ASSET_TOT_EOY' )
#' 
#' d <- get_er( dat_01 )
#' 
#' # winsorize at 0.025 and 0.975 percentiles instead of 0.01 and 0.99
#' d <- get_er( df = dat, net.assets = "x1", total.assets ="x2", winsorize=0.95 )
#' 
#' d <- get_er( dat_01, winsorize = 0.95 )
#' 
#' ## Errors ##
#' 
#' # numerator not specified
#' d <- get_er( df = dat, net.assets = NULL, total.assets = 'x2' )
#' 
#' # denominator not specified
#' d <- get_er( df = dat, net.assets = 'x1', total.assets = NULL )
#' 
#' # neither numerator nor denominator specified
#' d <- get_er( df = dat, net.assets = NULL, total.assets = NULL )
#' 
#' @export
get_er <- function( df, net.assets = 'F9_10_NAFB_TOT_EOY', total.assets = 'F9_10_ASSET_TOT_EOY', winsorize=0.98 )
{
  if( winsorize > 1 | winsorize < 0 )
  { stop( "winsorize argument must be 0 < w < 1" ) }
  
  if( is.null( net.assets )==T & is.null( total.assets )==F )
  { stop( "The numerator has been incorrectly specified. Ensure you are passing the correct data field to the correct argument." ) }
  
  if( is.null( net.assets )==F & is.null( total.assets )==T )
  { stop( "The denominator has been incorrectly specified. Ensure you are passing the correct data field to the correct argument." ) }
  
  if( is.null( net.assets )==T & is.null( total.assets )==T )
  { stop( "The argument fields are empty. Please supply column names for each argument or execute the function with default inputs." ) }
  
  a <- df[[ net.assets ]]
  t <- df[[ total.assets ]]
  
  if( winsorize > 1 | winsorize < 0 )
  { stop( "winsorize argument must be 0 < w < 1" ) }
  
  # can't divide by zero
  print( paste0( "Total assets cannot be zero: ", sum( t==0 ), " cases have been replaced with NA." ) )
  t[ t == 0 ] <- NA 
  
  er <- a / t
  
  top.p    <- 1 - (1-winsorize)/2
  bottom.p <- 0 + (1-winsorize)/2
  top      <- quantile( er, top.p, na.rm=T )
  bottom   <- quantile( er, bottom.p, na.rm=T )
  er.w    <- er
  er.w[ er.w > top    ] <- top
  er.w[ er.w < bottom ] <- bottom
  
  er.n <- scale( er.w )
  
  er.p <- dplyr::ntile( er, 100 )
  
  ER <- data.frame( er, er.w, er.n, er.p )
  
  print( summary( ER ) )
  
  par( mfrow=c(2,2) )
  plot( density(er,   na.rm=T), main="Equity Ratio (ER)" )
  plot( density(er.w, na.rm=T), main="ER Winsorized" )
  plot( density(er.n, na.rm=T), main="ER Standardized as Z" )
  plot( density(er.p, na.rm=T), main="ER as Percentile" )
  
  df.er <- data.frame( cbind( df, ER ) )
  return( df.er )
}
