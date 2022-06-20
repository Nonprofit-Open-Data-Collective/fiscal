###---------------------------------------------------
###   ADD PIPE OPERATOR TO THE PACKAGE
###---------------------------------------------------
#' @importFrom magrittr "%>%"



###---------------------------------------------------
###   GOVERNMENT GRANTS RATIO
###---------------------------------------------------

#' @title
#' Government Grants Ratio
#'
#' @description
#' Calculate the government grants ratio and append it to the dataframe. 
#'
#' @param df A dataframe containing the required field for computing the metric. The metric will be appended to this dataset.
#' @param ggc A character string indicating the column name for government grant contributions (On 990: Part VIII, Line 1(E); On EZ: Not Available).
#' @param total.revenue A character string indicating the column name for total revenue (On 990: (Part VIII, Line 12A); On EZ: Part I, Line 9).
#' 
#' @return The original dataframe appended with the government grants ratio (`ggr`), 
#'  a winsorized version (`ggr.w`), a standardized z-score version (`ggr.z`), 
#'  and a percentile version (`ggr.p`).   
#'
#' @details The government grants ratio measures the percentage of an organization’s revenues that come from 
#' government grants and contributions. High values in this ratio indicate that more of an organization's 
#' revenues come from government grants while a lower number indicates that an organization's funds come from 
#' other donation sources (private foundations), earned income, or investment income.
#' 
#' @examples
#' x1<-rnorm( 1000,100,30 )
#' x2<-rnorm( 1000,200,30 )
#' x2[ c(1,10,100) ] <- 0
#'
#' dat<-data.frame( x1, x2 )
#'
#' a<-get_ggr( df=dat, ggc='x1', total.revenue='x2', winsorize=0.98)
#'
#' head( a )
#' 
#' @export
get_ggr <- function( df, ggc, total.revenue, winsorize=0.98 )
{
  
  # checks
  if( winsorize > 1 | winsorize < 0 )
  { stop( "winsorize argument must be 0 < w < 1" ) }
  
  g <- df[[ ggc ]]
  r <- df[[ total.revenue ]]
  
  # can't divide by zero
  print( paste0( "Total revenue cannot be zero: ", sum( r==0 ), " cases have been replaced with NA." ) )
  r[ r == 0 ] <- NA 
  
  ggr <- g / r
  
  top.p    <- 1 - (1-winsorize)/2
  bottom.p <- 0 + (1-winsorize)/2
  top      <- quantile( ggr, top.p, na.rm=T )
  bottom   <- quantile( ggr, bottom.p, na.rm=T )
  ggr.w    <- ggr
  ggr.w[ ggr.w > top    ] <- top
  ggr.w[ ggr.w < bottom ] <- bottom
  
  ggr.n <- scale( ggr.w )
  
  ggr.p <- dplyr::ntile( ggr, 100 )
  
  GGR <- data.frame( ggr, ggr.w, ggr.n, ggr.p )
  
  print( summary( GGR ) )
  
  par( mfrow=c(2,2) )
  plot( density( ggr,   na.rm=T ), main="Government Grants Ratio (GGR)" )
  plot( density( ggr.w, na.rm=T ), main="GGR Winsorized" )
  plot( density( ggr.n, na.rm=T ), main="GGR Standardized as Z" )
  plot( density( ggr.p, na.rm=T ), main="GGR as Percentile" )
  
  df.ggr <- cbind( df, GGR )
  return( df.ggr )
}
