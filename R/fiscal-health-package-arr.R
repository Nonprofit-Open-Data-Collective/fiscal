###---------------------------------------------------
###   ADD PIPE OPERATOR TO THE PACKAGE
###---------------------------------------------------
#' @importFrom magrittr "%>%"



###---------------------------------------------------
###   ASSETS TO REVENUES RATIO
###---------------------------------------------------

#' @title
#' Assets to Revenues Ratio
#'
#' @description
#' Calculate the assets the revenues ratio and append it to the dataframe. 
#'
#' @param df A dataframe containing the required field for computing the metric. The metric will be appended to this dataset.
#' @param total.assets A character string indicating the column name for total assets (On 990: Part X, Line 16B; On EZ: Pt II, Line 25B).
#' @param total.revenue A character string indicating the column name for total revenue (On 990: (Part VIII, Line 12A); On EZ: Part I, Line 9).
#' 
#' @return The original dataframe appended with the assets to revenues ratio (`arr`), 
#'  a winsorized version (`arr.w`), a standardized z-score version (`arr.z`), 
#'  and a percentile version (`arr.p`).   
#'
#' @details This metric measures how many dollars of assets it has per dollar of annual revenues (how many 
#' dollars of revenue have been converted into assets in the past or that year for the current year). Generally, 
#' this ratio should be higher for organizations with large inventories of developments or large endowments.
#' 
#' @examples
#' x1<-rnorm( 1000,100,30 )
#' x2<-rnorm( 1000,200,30 )
#' x2[ c(1,10,100) ] <- 0
#' 
#' dat<-data.frame( x1, x2 )
#' 
#' a<-get_arr( df=dat, total.assets='x1', total.revenue='x2' )
#' 
#' head( a )
#' 
#' @export
get_arr <- function( df, total.assets, total.revenue, winsorize=0.98 )
{
  
  # checks
  if( winsorize > 1 | winsorize < 0 )
  { stop( "winsorize argument must be 0 < w < 1" ) }
  
  t <- df[[ total.assets ]]
  r <- df[[ total.revenue ]]
  
  # can't divide by zero
  print( paste0( "Total revenue cannot be zero: ", sum( r==0 ), " cases have been replaced with NA." ) )
  r[ r == 0 ] <- NA 
  
  arr <- t / r
  
  top.p    <- 1 - (1-winsorize)/2
  bottom.p <- 0 + (1-winsorize)/2
  top      <- quantile( arr, top.p, na.rm=T )
  bottom   <- quantile( arr, bottom.p, na.rm=T )
  arr.w    <- arr
  arr.w[ arr.w > top    ] <- top
  arr.w[ arr.w < bottom ] <- bottom
  
  arr.n <- scale( arr.w )
  
  arr.p <- dplyr::ntile( arr, 100 )
  
  ARR <- data.frame( arr, arr.w, arr.n, arr.p )
  
  print( summary( ARR ) )
  
  par( mfrow=c(2,2) )
  plot( density( arr,   na.rm=T ), main="Assets to Revenues Ratio (ARR)" )
  plot( density( arr.w, na.rm=T ), main="ARR Winsorized" )
  plot( density( arr.n, na.rm=T ), main="ARR Standardized as Z" )
  plot( density( arr.p, na.rm=T ), main="ARR as Percentile" )
  
  df.arr <- cbind( df, ARR )
  return( df.arr )
}
