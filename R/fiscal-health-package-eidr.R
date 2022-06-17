###---------------------------------------------------
###   ADD PIPE OPERATOR TO THE PACKAGE
###---------------------------------------------------
#' @importFrom magrittr "%>%"



###---------------------------------------------------
###   EARNED INCOME DEPENDENCE RATIO
###---------------------------------------------------

#' @title
#' Earned Income Dependence Ratio
#'
#' @description
#' Calculate the earned income dependence ratio and append it to the dataframe. 
#'
#' @param df A dataframe containing the required field for computing the metric. The metric will be appended to this dataset.
#' @param prog.service.rev A character string indicating the column name for program service revenue (On 990: Part VIII, Line 2g(A); On EZ: Not Available).
#' @param memb.dues A character string indicating the column name for membership dues (On 990: Part VIII, Line 1b(A); On EZ: Not Available).
#' @param royalties A character string indicating the column name for royalties (On 990: Part VIII, Line 5(A); On EZ: Not Available).
#' @param other.revenue A character string indicating the column name for all other revenue (On 990: Part VIII, Line 11d(A); On EZ: Not Available). 
#' @param total.revenue A character string indicating the column name for total revenue (On 990: (Part VIII, Line 12A); On EZ: Part I, Line 9).
#' 
#' @return The original dataframe appended with the earned income dependence ratio (`eidr`), 
#'  a winsorized version (`eidr.w`), a standardized z-score version (`eidr.z`), 
#'  and a percentile version (`eidr.p`).   
#'
#' @details This metric shows how much of an organization’s total revenues come from contributions, government 
#' grants, and special event reveues. High levels in this indicator are undesirable since that means that an 
#' organization’s revenues are volatile insofar as it is dependent on contributions that are highly likely to 
#' contract during economic downturns. Low values in this indicator mean an organization is not dependent on 
#' donations.
#' 
#' @examples
#' x1<-rnorm( 1000,100,30 )
#' x2<-rnorm( 1000,200,30 )
#' x3<-rnorm( 1000,200,30 )
#' x4<-rnorm( 1000,100,30 )
#' x5<-rnorm( 1000,200,30 )
#' x5[ c(1,10,100) ] <- 0
#'
#' dat<-data.frame( x1, x2, x3, x4, x5 )
#'
#' a<-get_eidr( df=dat, prog.service.rev='x1', memb.dues='x2', royalties='x3', other.revenue='x4', total.revenue='x5', winsorize=0.98)
#'
#' head( a )
#' 
#' @export
get_eidr <- function( df, prog.service.rev, memb.dues, royalties, other.revenue, total.revenue, winsorize=0.98 )
{
  
  # checks
  if( winsorize > 1 | winsorize < 0 )
  { stop( "winsorize argument must be 0 < w < 1" ) }
  
  num <- df[[ prog.service.rev ]] + df[[ memb.dues ]]+ df[[ royalties ]] + df[[ other.revenue ]]
  r <- df[[ total.revenue ]]
  
  # can't divide by zero
  print( paste0( "Total revenue cannot be zero: ", sum( r==0 ), " cases have been replaced with NA." ) )
  r[ r == 0 ] <- NA 
  
  eidr <- num / r
  
  top.p    <- 1 - (1-winsorize)/2
  bottom.p <- 0 + (1-winsorize)/2
  top      <- quantile( eidr, top.p, na.rm=T )
  bottom   <- quantile( eidr, bottom.p, na.rm=T )
  eidr.w    <- eidr
  eidr.w[ eidr.w > top    ] <- top
  eidr.w[ eidr.w < bottom ] <- bottom
  
  eidr.n <- scale( eidr.w )
  
  eidr.p <- dplyr::ntile( eidr, 100 )
  
  EIDR <- data.frame( eidr, eidr.w, eidr.n, eidr.p )
  
  print( summary( EIDR ) )
  
  par( mfrow=c(2,2) )
  plot( density( eidr,   na.rm=T ), main="Eared Income Dependence Ratio (EIDR)" )
  plot( density( eidr.w, na.rm=T ), main="EIDR Winsorized" )
  plot( density( eidr.n, na.rm=T ), main="EIDR Standardized as Z" )
  plot( density( eidr.p, na.rm=T ), main="EIDR as Percentile" )
  
  df.eidr <- cbind( df, EIDR )
  return( df.eidr )
}
