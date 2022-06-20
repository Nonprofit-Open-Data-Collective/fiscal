###---------------------------------------------------
###   QUICK RATIO
###---------------------------------------------------

#' @title
#' Quick Ratio
#'
#' @description
#' Calculate the quick ratio and append it to the dataframe. 
#'
#' @param cash A character string indicating the column name for cash, EOY (On 990: Part X, line 1B; On EZ: Not Available).
#' @param si A character string indicating the column name for short-term investments, EOY (On 990: Part X, line 2B;On EZ: Not Available).
#' @param pr A character string indicating the column name for pledges and grant receivables, EOY On 990: Part X, line 3B; On EZ: Not Available).
#' @param ar A character string indicating the column name for accounts receivables, EOY (On 990: Part X, line 4B; On EZ: Not Available).
#' @param ap A character string indicating the column name for accounts payable, EOY (On 990: Part X, line 17B; On EZ: Not Available).
#' @param gp A character string indicating the column name for grants payable, EOY (On 990: Part X, line 18B; On EZ: Not Available).
#' 
#' #' @return The original dataframe appended with the quick ratio (`qr`), 
#'  a winsorized version (`qr.w`), a standardized z-score version (`qr.z`), 
#'  and a percentile version (`qr.p`).   
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
#' x4 <- rnorm( 1000,100,30 )
#' x5 <- rnorm( 1000,200,30 )
#' x6 <- rnorm( 1000,200,30 )
#' dat <- data.frame( x1, x2, x3, x4, x5, x6 )
#'
#' a <- get_qr( df=dat, cash='x1', si='x2', pr='x3', ar='x4', ap='x5', gp='x6', winsorize=0.98 )
#' 
#' @export
get_qr <- function( df, cash, si, pr, ar, ap, gp, winsorize=0.98 )
{
  
  # checks
  if( winsorize > 1 | winsorize < 0 )
  { stop( "winsorize argument must be 0 < w < 1" ) }
  
  num <- df[[ cash ]] + df[[ si ]] + df[[ pr ]] + df[[ ar ]]
  den <- df[[ ap ]] + df[[ gp ]]
  
  # can't divide by zero
  print( paste0( "Payables cannot be zero: ", sum( den==0 ), " cases have been replaced with NA." ) )
  den[ den == 0 ] <- NA 
  
  qr <- num / den
  
  top.p    <- 1 - (1-winsorize)/2
  bottom.p <- 0 + (1-winsorize)/2
  top      <- quantile( qr, top.p, na.rm=T )
  bottom   <- quantile( qr, bottom.p, na.rm=T )
  qr.w    <- qr
  qr.w[ qr.w > top    ] <- top
  qr.w[ qr.w < bottom ] <- bottom
  
  qr.n <- scale( qr.w )
  
  qr.p <- dplyr::ntile( qr, 100 )
  
  QR <- data.frame( qr, qr.w, qr.n, qr.p )
  
  print( summary( QR ) )
  
  par( mfrow=c(2,2) )
  plot( density( qr,   na.rm=T ), main="Quick Ratio (QR)" )
  plot( density( qr.w, na.rm=T ), main="QR Winsorized" )
  plot( density( qr.n, na.rm=T ), main="QR Standardized as Z" )
  plot( density( qr.p, na.rm=T ), main="QR as Percentile" )
  
  df.qr <- cbind( df, QR )
  return( df.qr )
}
