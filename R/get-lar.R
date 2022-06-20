###---------------------------------------------------
###   LAND TO ASSETS RATIO
###---------------------------------------------------

#' @title
#' Self Sufficiency Ratio 
#'
#' @description
#' Calculate the self sufficiency ratio and append it to the dataframe. 
#'
#' @param land Land, buildings, and equipment (Part X, line 10b; On EZ: On EZ: Not Available).
#' @param assets Total assets (On 990: Part X, Line 16B; On EZ: Part II, line 25B).
#' @param winsorize The winsorization value (between 0 and 1), defaults to 0.98 which winsorizes at 99th and 1st percentile values.   
#' 
#' @return The original dataframe appended with the self sufficiency ratio (`lar`), 
#'  a winsorized version (`lar.w`), a standardized z-score version (`lar.z`), 
#'  and a percentile version (`lar.p`).   
#'
#' @details This metric shows the proportion of an organization’s total assets that are made up by land and 
#' buildings. It shows how diversified their asset portfolio is and somewhat how specialized their line of 
#' work is within housing or building development. Higher values mean that an organization has more of its 
#' asset portfolio in land and buildings, and lower values mean that development or land and building holdings 
#' represent a small share of what the organization’s activities and finances are tied up in.
#' 
#' @examples
#' x1 <- rnorm( 1000,100,30 )
#' x2 <- rnorm( 1000,200,30 )
#' x2[ c(15,300,600) ] <- 0
#' dat <- data.frame( x1,x2 )
#' d <- get_lar( df=dat, land='x1', assets='x2' )
#' head( d )
#'
#' # winsorize at 0.025 and 0.975 percentiles instead of 0.01 and 0.99
#' d <- get_lar( df=dat, land='x1', assets='x2', winsorize=0.95 )
#' 
#' @export
get_lar <- function( df, land, assets, winsorize=0.98 )
{
  
  l <- df[[ land ]]
  a <- df[[ assets ]]
  
  if( winsorize > 1 | winsorize < 0 )
  { stop( "winsorize argument must be 0 < w < 1" ) }
  
  # can't divide by zero
  print( paste0( "Total assets cannot be zero: ", sum( a==0 ), " cases have been replaced with NA." ) )
  a[ a == 0 ] <- NA 
  
  lar <- l / a
  
  top.p    <- 1 - (1-winsorize)/2
  bottom.p <- 0 + (1-winsorize)/2
  top      <- quantile( lar, top.p, na.rm=T )
  bottom   <- quantile( lar, bottom.p, na.rm=T )
  lar.w    <- lar
  lar.w[ lar.w > top    ] <- top
  lar.w[ lar.w < bottom ] <- bottom
  
  lar.n <- scale( lar.w )
  
  lar.p <- dplyr::ntile( lar, 100 )
  
  LAR <- data.frame( lar, lar.w, lar.n, lar.p )
  
  print( summary( lar ) )
  
  par( mfrow=c(2,2) )
  plot( density(lar,   na.rm=T), main="Self Sufficiency Ratio (LAR)" )
  plot( density(lar.w, na.rm=T), main="LAR Winsorized" )
  plot( density(lar.n, na.rm=T), main="LAR Standardized as Z" )
  plot( density(lar.p, na.rm=T), main="LAR as Percentile" )
  
  df.lar <- cbind( df, LAR )
  return( df.lar )
}
