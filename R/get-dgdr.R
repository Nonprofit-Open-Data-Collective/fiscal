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
#' @param total.contributions A character string indicating the column name for total contributions (On 990: Part VIII, Line 1h(A); On EZ: Not Available).
#' @param fund.income A character string indicating the column name for fundraising income (On 990: Part VIII, Line 8c(A); On EZ: Not Available).
#' @param total.revenue A character string indicating the column name for total revenue (On 990: Part VIII, Line 12A; On EZ: Part I, Line 9).
#' 
#' @return Object of class \code{data.frame}: the original dataframe appended with the donation/grant dependence ratio (`dgdr`), 
#'  a winsorized version (`dgdr.w`), a standardized z-score version (`dgdr.z`), 
#'  and a percentile version (`dgdr.p`).   
#'
#' @details This metric shows how much of an organization’s total revenues come from contributions, government 
#' grants, and special event reveues. High levels in this indicator are undesirable since that means that an 
#' organization’s revenues are volatile insofar as it is dependent on contributions that are highly likely to 
#' contract during economic downturns. Low values in this indicator mean an organization is not dependent on 
#' donations.
#' 
#' @examples
#' x1 <- rnorm( 1000,100,30 )
#' x2 <- rnorm( 1000,200,30 )
#' x3 <- rnorm( 1000,200,30 )
#' x3[ c(1,10,100)] <- 0
#'
#' dat <- data.frame( x1, x2, x3 )
#'
#' a <- get_dgdr( df=dat, total.contributions='x1', fund.income='x2', total.revenue='x3', winsorize=0.98 )
#'
#' head( a )
#' 
#' @export
get_dgdr <- function( df, total.contributions, fund.income, total.revenue, winsorize=0.98 )
{
  
  # checks
  if( winsorize > 1 | winsorize < 0 )
  { stop( "winsorize argument must be 0 < w < 1" ) }
  
  num <- df[[ total.contributions ]] + df[[ fund.income ]]
  r <- df[[ total.revenue ]]
  
  # can't divide by zero
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
