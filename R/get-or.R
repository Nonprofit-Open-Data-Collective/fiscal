###---------------------------------------------------
###   OPERATING MARGIN
###---------------------------------------------------

#' @title
#' Operating Margin
#'
#' @description
#' Calculate the operating margin and append it to the dataframe. 
#'
#' @param df A dataframe containing the required field for computing the metric. The metric will be appended to this dataset.
#' @param equity.eoy Unrestricted net assets, EOY (On 990: Part X, line 27B; On EZ: Not Available).
#' @param equity.boy Unrestricted net assets, BOY (On 990: Part X, line 27A; On EZ: Not Available).
#' 
#' @return The original dataframe appended with the operating margin (`or`), 
#'  a winsorized version (`or.w`), a standardized z-score version (`or.z`), 
#'  and a percentile version (`or.p`).   
#'
#' @details The operating margin measures what percent of an organizations unrestricted net assets (or free 
#' cash) from the beginning of 2019 it had at the end of 2019. 
#' 
#' @examples
#' x1<-rnorm( 1000,100,30 )
#' x2<-rnorm( 1000,200,30 )
#' x2[ c(1,10,100) ] <- 0
#'
#' dat<-data.frame( x1, x2 )
#'
#' a<-get_or( df=dat, equity.eoy='x1', equity.boy='x2', winsorize=0.98)
#'
#' head( a )
#' 
#' @export
get_or <- function( df, equity.eoy, equity.boy, winsorize=0.98 )
{
  
  # checks
  if( winsorize > 1 | winsorize < 0 )
  { stop( "winsorize argument must be 0 < w < 1" ) }
  
  num <- df[[ equity.eoy ]] - df[[ equity.boy ]]
  e <- df[[ equity.boy ]]
  
  # can't divide by zero
  print( paste0( "Equity cannot be zero: ", sum( e==0 ), " cases have been replaced with NA." ) )
  e[ e == 0 ] <- NA 
  
  or <- num / e
  
  top.p    <- 1 - (1-winsorize)/2
  bottom.p <- 0 + (1-winsorize)/2
  top      <- quantile( or, top.p, na.rm=T )
  bottom   <- quantile( or, bottom.p, na.rm=T )
  or.w    <- or
  or.w[ or.w > top    ] <- top
  or.w[ or.w < bottom ] <- bottom
  
  or.n <- scale( or.w )
  
  or.p <- dplyr::ntile( or, 100 )
  
  OR <- data.frame( or, or.w, or.n, or.p )
  
  print( summary( OR ) )
  
  par( mfrow=c(2,2) )
  plot( density( or,   na.rm=T ), main="Operating Margin (OR)" )
  plot( density( or.w, na.rm=T ), main="OR Winsorized" )
  plot( density( or.n, na.rm=T ), main="OR Standardized as Z" )
  plot( density( or.p, na.rm=T ), main="OR as Percentile" )
  
  df.or <- cbind( df, OR )
  return( df.or )
}
