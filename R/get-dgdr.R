###---------------------------------------------------
###   DONATION/GRANT DEPENDENCE RATIO
###---------------------------------------------------

#' @title
#' Donation/Grant Dependence Ratio
#'
#' @description
#' Calculate the donation/grant dependence ratio and append it to the dataframe. 
#'
#' @param df A \code{data.frame} containing the required field for computing the metric. The metric will be appended to this dataset.
#' @param total.contributions Column name for total contributions (can be quoted or unquoted) (On 990: Part VIII, Line 1h(A); On EZ: Not Available) with the default name supplied.
#' @param fund.income Column name for fundraising income (can be quoted or unquoted) (On 990: Part VIII, Line 8c(A); On EZ: Not Available) with the default name supplied.
#' @param total.revenue Column name for fundraising income (can be quoted or unquoted) (On 990: Part VIII, Line 12A; On EZ: Part I, Line 9) with the default name supplied.
#' 
#' @usage get_dgdr( df, total.contributions = "F9_08_REV_CONTR_TOT", fund.income = "F9_08_REV_OTH_FUNDR_NET_TOT", total.revenue = "F9_08_REV_TOT_TOT", winsorize = 0.98 )
#' 
#' @return Object of class \code{data.frame}: the original dataframe appended with the donation/grant dependence ratio (`dgdr`), 
#'  a winsorized version (`dgdr.w`), a standardized z-score version (`dgdr.z`), 
#'  and a percentile version (`dgdr.p`).   
#'
#' @details This metric shows how much of an organization’s total revenues come from contributions, government 
#' grants, and special event revenues. High levels in this indicator are undesirable since that means that an 
#' organization’s revenues are volatile insofar as it is dependent on contributions that are highly likely to 
#' contract during economic downturns. Low values in this indicator mean an organization is not dependent on 
#' donations. Note: computation of this metric is available to only 990 filers and not for 990-EZ filers. The default 
#' inputs use column names for variables available only to 990 filers.
#' 
#' @examples
#' x1 <- rnorm( 1000,100,30 )
#' x2 <- rnorm( 1000,200,30 )
#' xc <- rnorm( 1000,200,30 )
#' x3[ c(15,300,600) ] <- 0
#' 
#' dat <- data.frame( x1, x2, x3)
#' 
#' # specify own column names
#' # quoted
#' d <- get_dgdr( df = dat, total.contributions = "x1", fund.income = "x2", total.revenue = "x3" )
#' 
#' # unquoted
#' d <- get_dgdr( df = dat, total.contributions = x1, fund.income = x2, total.revenue = x3 )
#' 
#' head( d )
#' 
#' # run with default column names
#' dat_01 <- dat
#' 
#' colnames( dat_01 ) <- c( "F9_08_REV_CONTR_TOT", "F9_08_REV_OTH_FUNDR_NET_TOT", "F9_08_REV_TOT_TOT" )
#' 
#' d <- get_dgdr( dat_01 )
#' 
#' # winsorize at 0.025 and 0.975 percentiles instead of 0.01 and 0.99
#' d <- get_dgdr( df = dat, total.contributions = "x1", fund.income ="x2", total.revenue = "x3", winsorize=0.95 )
#' 
#' d <- get_dgdr( dat_01, winsorize = 0.95 )
#' 
#' ## errors ##
#' 
#' # numerator not specified
#' d <- get_dgdr( df = dat, total.contributions = NULL, fund.income = "x2", total.revenue = "x3" )
#' 
#' # denominator not specified
#' d <- get_dgdr( df = dat, total.contributions = "x1", fund.income = "x2", total.revenue = NULL )
#' 
#' # neither numerator nor denominator specified
#' d <- get_dgdr( df = dat, total.contributions = NULL, fund.income = NULL, total.revenue = NULL )
#' 
#' @export
get_dgdr <- function( df, total.contributions = "F9_08_REV_CONTR_TOT", 
                      fund.income = "F9_08_REV_OTH_FUNDR_NET_TOT", 
                      total.revenue = "F9_08_REV_TOT_TOT", 
                      winsorize=0.98 )
{
  
  # quoted/unquoted arguments
  total.contributions <- rlang::set_names( rlang::quo_name( rlang::enquo( total.contributions ) ) )
  fund.income <- rlang::set_names( rlang::quo_name( rlang::enquo( fund.income ) ) )
  total.revenue <- rlang::set_names( rlang::quo_name( rlang::enquo( total.revenue ) ) )
  
  if( total.contributions == "NULL" ) total.contributions <- NULL
  if( fund.income == "NULL" ) fund.income <- NULL
  if( total.revenue == "NULL" ) total.revenue <- NULL
  
  # checks
  if( winsorize > 1 | winsorize < 0 )
  { stop( "winsorize argument must be 0 < w < 1" ) }
  
  if( ( is.null( total.contributions )==T | is.null( fund.income )==T ) & is.null( total.revenue )==F )
  { stop( "The numerator has been incorrectly specified. Ensure you are passing the correct data field to the correct argument." ) }
  
  if( ( is.null( total.contributions )==F | is.null( fund.income )==F ) & is.null( total.revenue )==T )
  { stop( "The denominator has been incorrectly specified. Ensure you are passing the correct data field to the correct argument." ) }
  
  if( is.null( total.contributions )==T & is.null( fund.income )==T & is.null( total.revenue )==T )
  { stop( "The argument fields are empty. Please supply column names for each argument or execute the function with default inputs." ) }
  
  
  num <- df[[ total.contributions ]] + df[[ fund.income ]]
  r <- df[[ total.revenue ]]
  
  # can"t divide by zero
  print( paste0( "Total revenue cannot be zero: ", sum( r==0 ), " cases have been replaced with NA." ) )
  r[ r == 0 ] <- NA 
  
  dgdr <- num / r
  
  top.p    <- 1 - (1-winsorize)/2
  bottom.p <- 0 + (1-winsorize)/2
  top      <- quantile( dgdr, top.p, na.rm=T )
  bottom   <- quantile( dgdr, bottom.p, na.rm=T )
  dgdr.w    <- dgdr
  dgdr.w[ dgdr.w > top    ] <- top
  dgdr.w[ dgdr.w < bottom ] <- bottom
  
  dgdr.n <- scale( dgdr.w )
  
  dgdr.p <- dplyr::ntile( dgdr, 100 )
  
  DGDR <- data.frame( dgdr, dgdr.w, dgdr.n, dgdr.p )
  
  print( summary( DGDR ) )
  
  par( mfrow=c(2,2) )
  plot( density( dgdr,   na.rm=T ), main="Donation/Grant Dependence Ratio (DGDR)" )
  plot( density( dgdr.w, na.rm=T ), main="DGDR Winsorized" )
  plot( density( dgdr.n, na.rm=T ), main="DGDR Standardized as Z" )
  plot( density( dgdr.p, na.rm=T ), main="DGDR as Percentile" )
  
  df.dgdr <- data.frame( cbind( df, DGDR ) )
  return( df.dgdr )
}
