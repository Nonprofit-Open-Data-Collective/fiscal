###---------------------------------------------------
###   DEBT TO EQUITY RATIO
###---------------------------------------------------

#' @title
#' Debt to Equity Ratio 
#'
#' @description
#' Calculate the debt to equity ratio and append it to the dataframe. 
#'
#' @param df A dataframe containing the required field for computing the metric. The metric will be appended to this dataset.
#' @param debt A character string indicating the column name for total debt, EOY (On 990: Part X, line 17B; On EZ: Not Available) with the default name supplied.
#' @param equity A character string indicating the column name for unrestricted net assets, EOY (On 990: Part X, line 27B; On EZ: Not Available) with the default name supplied.
#' @param winsorize The winsorization value (between 0 and 1), defaults to 0.98 which winsorizes at 99th and 1st percentile values.   
#' 
#' @return The original dataframe appended with the debt to equity ratio (`der`), 
#'  a winsorized version (`der.w`), a standardized z-score version (`der.z`), 
#'  and a percentile version (`der.p`).   
#'
#' @details Debt to Equity Ratio is a a leverage ratio that defines the total amount of debt relative to the 
#' unrestricted assets owned by an organization.This metric shows the big picture view of how much an organization 
#' owes relative to what it owns.Higher values mean it is more highly leveraged, meaning it has low capacity for future 
#' borrowing. As an example: if an organization has a total-debt-to-net-assets ratio of 0.4, 40% of its assets 
#' are financed by creditors, and 60% are its own unrestricted, available equity. As this percentage 
#' creeps above the 50% mark, it can call into question the organization’s ability to manage debt, 
#' which could jeopardize the delivery of programs and services. However, for developers, extremely 
#' low values may mean the organization is not capitalizing enough on its equity to expand.
#' This value can be negative if an organization has either overpaid on its debts or if it has negative 
#' unrestricted net assets.One limitation of this metric is that it does not provide any indication of 
#' asset quality or liquidity since it lumps tangible and intangible assets together. Note: computation of this metric 
#' is available to only 990 filers and not for 990-EZ filers. The default inputs use column names for variables 
#' available only to 990 filers. Note: This ratio can be interchanged with total liabilities over total net assets 
#' (which should be comparable for EZ filers and full 990 filers), but for Community Development Corporations, the 
#' more important metric is unrestricted net assets, which isn’t available for EZ filers.
#' 
#' @examples
#' x1 <- rnorm( 1000,100,30 )
#' x2 <- rnorm( 1000,200,30 )
#' x2[ c(15,300,600) ] <- 0
#' 
#' dat <- data.frame( x1,x2 )
#' 
#' # specify own column names
#' d <- get_der( df = dat, debt = "x1", equity = "x2" )
#' 
#' head( d )
#' 
#' # run with default column names
#' dat_01 <- dat
#' 
#' colnames( dat_01 ) <- c( 'F9_10_LIAB_ACC_PAYABLE_EOY', 'F9_10_NAFB_UNRESTRICT_EOY' )
#' 
#' d <- get_der( dat_01 )
#' 
#' # winsorize at 0.025 and 0.975 percentiles instead of 0.01 and 0.99
#' d <- get_der( df = dat, debt = "x1", equity ="x2", winsorize=0.95 )
#' 
#' d <- get_der( dat_01, winsorize = 0.95 )
#' 
#' ## errors ##
#' 
#' # numerator not specified
#' d <- get_der( df = dat, debt = NULL, equity = 'x2' )
#' 
#' # denominator not specified
#' d <- get_der( df = dat, debt = 'x1', equity = NULL )
#'
#' neither numerator nor denominator specified
#' d <- get_der( df = dat, debt = NULL, equity = NULL )
#' 
#' @export
get_der <- function( df, debt = 'F9_10_LIAB_ACC_PAYABLE_EOY', equity = 'F9_10_NAFB_UNRESTRICT_EOY', winsorize=0.98 )
{
  
  if( winsorize > 1 | winsorize < 0 )
  { stop( "winsorize argument must be 0 < w < 1" ) }
  
  if( is.null( debt )==T & is.null( equity )==F )
  { stop( "The numerator has been incorrectly specified. Ensure you are passing the correct data field to the correct argument." ) }
  
  if( is.null( debt )==F & is.null( equity )==T )
  { stop( "The denominator has been incorrectly specified. Ensure you are passing the correct data field to the correct argument." ) }
  
  if( is.null( debt )==T & is.null( equity )==T )
  { stop( "The argument fields are empty. Please supply column names for each argument or execute the function with default inputs." ) }
  
  d <- df[[ debt ]]
  e <- df[[ equity ]]
  

  # can't divide by zero
  print( paste0( "Equity cannot be equal to zero: ", sum( e==0 ), " cases have been replaced with NA." ) )
  e[ e == 0 ] <- NA 
  
  der <- d / e
  
  top.p    <- 1 - (1-winsorize)/2
  bottom.p <- 0 + (1-winsorize)/2
  top      <- quantile( der, top.p, na.rm=T )
  bottom   <- quantile( der, bottom.p, na.rm=T )
  der.w    <- der
  der.w[ der.w > top    ] <- top
  der.w[ der.w < bottom ] <- bottom
  
  der.n <- scale( der.w )
  
  der.p <- dplyr::ntile( der, 100 )
  
  DER <- data.frame( der, der.w, der.n, der.p )
  
  print( summary( DER ) )
  
  par( mfrow=c(2,2) )
  plot( density(der,   na.rm=T), main="Debt to Equity Ratio (DER)" )
  plot( density(der.w, na.rm=T), main="DER Winsorized" )
  plot( density(der.n, na.rm=T), main="DER Standardized as Z" )
  plot( density(der.p, na.rm=T), main="DER as Percentile" )
  
  df.der <- cbind( df, DER )
  return( df.der )
}





