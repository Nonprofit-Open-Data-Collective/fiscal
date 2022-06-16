###---------------------------------------------------
###   ADD PIPE OPERATOR TO THE PACKAGE
###---------------------------------------------------
#' @importFrom magrittr "%>%"



###---------------------------------------------------
###   PRE-DEPRECIATION PROFITABILITY MARGIN
###---------------------------------------------------

#' @title
#' Post-Depreciation Profitability Margin
#'
#' @description
#' Calculate the post-depreciation profitability margin and append it to the dataframe. 
#'
#' @param expenses A character string indicating the column name for total functional expenses (On 990: Part IX, line 25A; On EZ: Not Available).
#' @param revenue A character string indicating the column name for total revenue, (On 990: Part VIII, line 12A; On EZ: Part I, line 9).
#' 
#' @return The original dataframe appended with the post-depreciation profitability margin (`podpm`), 
#'  a winsorized version (`podpm.w`), a standardized z-score version (`podpm.z`), 
#'  and a percentile version (`podpm.p`).   
#'
#' @details Post-depreciation profit is an income measure used to determine profit after incorporating 
#' non-cash expenses on a balance sheet. Post-depreciation profit is calculated because it provides a 
#' picture of an organization’s true available profits net of depreciation expenses. Non-expense items lower 
#' an organization’s reported earnings, so a post-depreciation profit would show a lower profit in comparison 
#' to profits calculated prior to depreciation expenses.High values in this metric are generally desirable 
#' since they indicate that an organization is not losing a lot of its revenues to expenses. Values close to 
#' zero are normal, and negative numbers indicate the organization is functioning at a deficit.
#' 
#' @examples
#' x1 <- rnorm( 1000,100,30 )
#' x2 <- rnorm( 1000,200,30 )
#' x3 <- rnorm( 1000,200,30 )
#' dat <- data.frame( x1, x2, x3 )
#' a <- get_podpm( df=dat, revenue='x1', depreciation='x2' )
#' 
#' @export
get_podpm <- function( df, expenses, revenue, winsorize=0.98 )
{
  
  # checks
  if( winsorize > 1 | winsorize < 0 )
  { stop( "winsorize argument must be 0 < w < 1" ) }
  
  num <- df[[ revenue ]] - df[[ expenses ]]
  den <- df[[ revenue ]]  
  
  # can't divide by zero
  print( paste0( "Revenue cannot be zero: ", sum( den==0 ), " cases have been replaced with NA." ) )
  den[ den == 0 ] <- NA 
  
  podpm <- num / den
  
  top.p    <- 1 - (1-winsorize)/2
  bottom.p <- 0 + (1-winsorize)/2
  top      <- quantile( podpm, top.p, na.rm=T )
  bottom   <- quantile( podpm, bottom.p, na.rm=T )
  podpm.w    <- podpm
  podpm.w[ podpm.w > top    ] <- top
  podpm.w[ podpm.w < bottom ] <- bottom
  
  podpm.n <- scale( podpm.w )
  
  podpm.p <- dplyr::ntile( podpm, 100 )
  
  PODPM <- data.frame( podpm, podpm.w, podpm.n, podpm.p )
  
  print( summary( PODPM ) )
  
  par( mfrow=c(2,2) )
  plot( density( podpm,   na.rm=T ), main="Post-Depreciation Profitability Margin (PODPM)" )
  plot( density( podpm.w, na.rm=T ), main="PODPM Winsorized" )
  plot( density( podpm.n, na.rm=T ), main="PODPM Standardized as Z" )
  plot( density( podpm.p, na.rm=T ), main="PODPM as Percentile" )
  
  df.podpm <- cbind( df, PODPM )
  return( df.podpm )
}
