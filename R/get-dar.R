###---------------------------------------------------
###   DEBT TO ASSET RATIO
###---------------------------------------------------

#' @title
#' Debt to Asset Ratio 
#'
#' @description
#' Calculate the debt to asset ratio and append it to the dataframe. 
#'
#' @param df A dataframe containing the required field for computing the metric. The metric will be appended to this dataset.
#' @param debt A character string indicating the column name for total liabilities (On 990: Part X, line 26B; On EZ: Part II, line 26B) with the default name supplied.
#' @param assets A character string indicating the column name for total assets, EOY (On 990: Part X, line 16B; On EZ: Part II, line 25B) with the default name supplied.
#' @param winsorize The winsorization value (between 0 and 1), defaults to 0.98 which winsorizes at 99th and 1st percentile values.   
#' 
#' @return The original dataframe appended with the debt to asset ratio (`dar`), 
#'  a winsorized version (`dar.w`), a standardized z-score version (`dar.z`), 
#'  and a percentile version (`dar.p`).   
#'
#' @details Total-debt-to-total-assets is a leverage ratio that defines the total 
#'  amount of debt relative to assets owned by a company. Using this metric, 
#'  analysts can compare one company's leverage with that of other companies 
#'  in the same industry. This information can reflect how financially stable 
#'  a company is. The higher the ratio, the higher the degree of leverage (DoL) and, 
#'  consequently, the higher the risk (\href{Investopedia}{https://www.investopedia.com/terms/t/totaldebttototalassets.asp}).
#'  Note: computation of this metric is available to both 990 and 990-EZ filers.
#' 
#' 
#' @examples
#' x1 <- rnorm( 1000,100,30 )
#' x2 <- rnorm( 1000,200,30 )
#' x2[ c(15,300,600) ] <- 0
#' 
#' dat <- data.frame( x1,x2 )
#' 
#' # specify own column names
#' d <- get_dar( df = dat, debt = "x1", assets = "x2" )
#' 
#' head( d )
#' 
#' # run with default column names
#' dat_01 <- dat
#' 
#' colnames( dat_01 ) <- c( 'F9_10_LIAB_TOT_EOY', 'F9_10_ASSET_TOT_EOY' )
#'
#' d <- get_dar( dat_01 )
#' 
#' # winsorize at 0.025 and 0.975 percentiles instead of 0.01 and 0.99
#' d <- get_dar( df = dat, debt = "x1", assets ="x2", winsorize=0.95 )
#' 
#' d <- get_dar( dat_01, winsorize = 0.95 )
#' 
#' ## Errors ##
#' 
#' # numerator not specified
#' d <- get_dar( df = dat, debt = NULL, assets = 'x2' )
#' 
#' # denominator not specified
#' d <- get_dar( df = dat, debt = 'x1', assets = NULL )
#' 
#' # neither numerator nor denominator specified
#' d <- get_dar( df = dat, debt = NULL, assets = NULL )
#' @export
get_dar <- function( df, debt = 'F9_10_LIAB_TOT_EOY', assets = 'F9_10_ASSET_TOT_EOY', winsorize=0.98 )
{
  # checks
  if( winsorize > 1 | winsorize < 0 )
  { stop( "winsorize argument must be 0 < w < 1" ) }
  
  if( is.null( debt )==T & is.null( assets )==F )
  { stop( "The numerator has been incorrectly specified. Ensure you are passing the correct data field to the correct argument." ) }
  
  if( is.null( debt )==F & is.null( assets )==T )
  { stop( "The denominator has been incorrectly specified. Ensure you are passing the correct data field to the correct argument." ) }
  
  if( is.null( debt )==T & is.null( assets )==T )
  { stop( "The argument fields are empty. Please supply column names for each argument or execute the function with default inputs." ) }
  
  d <- df[[ debt ]]
  a <- df[[ assets ]]

    
  # can't divide by zero
  print( paste0( "Assets cannot be equal to zero: ", sum( a==0 ), " cases have been replaced with NA." ) )
  
  a[ a == 0 ] <- NA 

  dar <- d / a

  top.p    <- 1 - (1-winsorize)/2
  bottom.p <- 0 + (1-winsorize)/2
  top      <- quantile( dar, top.p, na.rm=T )
  bottom   <- quantile( dar, bottom.p, na.rm=T )
  dar.w    <- dar
  dar.w[ dar.w > top    ] <- top
  dar.w[ dar.w < bottom ] <- bottom

  dar.z <- scale( dar.w )

  dar.p <- dplyr::ntile( dar, 100 )

  DAR <- data.frame( dar, dar.w, dar.z, dar.p )

  print( summary( DAR ) )

  par( mfrow=c(2,2) )
  plot( density(dar,   na.rm=T), main="Debt to Asset Ratio (DAR)" )
  plot( density(dar.w, na.rm=T), main="DAR Winsorized" )
  plot( density(dar.z, na.rm=T), main="DAR Standardized as Z" )
  plot( density(dar.p, na.rm=T), main="DAR as Percentile" )

  df.dar <- cbind( df, DAR )
  return( df.dar )
}



