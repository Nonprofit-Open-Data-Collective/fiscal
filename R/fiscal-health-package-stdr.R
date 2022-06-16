###---------------------------------------------------
###   ADD PIPE OPERATOR TO THE PACKAGE
###---------------------------------------------------
#' @importFrom magrittr "%>%"



###---------------------------------------------------
###   SHORT TERM DEBT RATIO
###---------------------------------------------------

#' @title
#' Short Term Debt Ratio
#'
#' @description
#' Calculate the short term debt ratio and append it to the dataframe. 
#'
#' @param ap A character string indicating the column name for accounts payable, EOY (On 990: Part X, line 17B; On EZ: Not Available).
#' @param gp A character string indicating the column name for grants payable, EOY (On 990: Part X, line 18B; On EZ: Not Available).
#' @param net.assets A character string indicating the column name for net assets, EOY (On 990: Part X, Line 33B; On EZ: Part I, Line 21).
#' 
#' @return The original dataframe appended with the short term debt ratio (`stdr`), 
#'  a winsorized version (`stdr.w`), a standardized z-score version (`stdr.z`), 
#'  and a percentile version (`stdr.p`).   
#'
#' @details This metric indicates how well an organization can cover its liabilities with its readily 
#' available cash. When an organization has a quick ratio of 1, its quick assets are equal to its current 
#' liabilities. This also indicates that the organization can pay off its current debts without selling its 
#' long-term assets. If an organization has a quick ratio higher than 1, this means that it owns more quick 
#' assets than current liabilities.
#' 
#' @examples
#' x1 <- rnorm( 1000,100,30 )
#' x2 <- rnorm( 1000,200,30 )
#' x3 <- rnorm( 1000,200,30 )
#' x3[ c(1,10,100)] <- 0
#' dat <- data.frame( x1, x2, x3 )
#'
#' a <- get_stdr( df=dat, ap='x1', gp='x2', net.assets='x3', winsorize=0.98 )
#' 
#' @export
get_stdr <- function( df, ap, gp, net.assets, winsorize=0.98 )
{
  
  # checks
  if( winsorize > 1 | winsorize < 0 )
  { stop( "winsorize argument must be 0 < w < 1" ) }
  
  num <- df[[ ap ]] + df[[ gp ]]
  a <- df[[ net.assets ]]
  
  # can't divide by zero
  print( paste0( "Net assets cannot be zero: ", sum( a==0 ), " cases have been replaced with NA." ) )
  a[ a == 0 ] <- NA 
  
  stdr <- num / a
  
  top.p    <- 1 - (1-winsorize)/2
  bottom.p <- 0 + (1-winsorize)/2
  top      <- quantile( stdr, top.p, na.rm=T )
  bottom   <- quantile( stdr, bottom.p, na.rm=T )
  stdr.w    <- stdr
  stdr.w[ stdr.w > top    ] <- top
  stdr.w[ stdr.w < bottom ] <- bottom
  
  stdr.n <- scale( stdr.w )
  
  stdr.p <- dplyr::ntile( stdr, 100 )
  
  STDR <- data.frame( stdr, stdr.w, stdr.n, stdr.p )
  
  print( summary( STDR ) )
  
  par( mfrow=c(2,2) )
  plot( density( stdr,   na.rm=T ), main="Short Term Debt Ratio (STDR)" )
  plot( density( stdr.w, na.rm=T ), main="STDR Winsorized" )
  plot( density( stdr.n, na.rm=T ), main="STDR Standardized as Z" )
  plot( density( stdr.p, na.rm=T ), main="STDR as Percentile" )
  
  df.stdr <- cbind( df, STDR )
  return( df.stdr )
}
