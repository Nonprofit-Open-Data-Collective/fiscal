###---------------------------------------------------
###   PRE-DEPRECIATION PROFITABILITY MARGIN
###---------------------------------------------------

#' @title
#' Pre-Depreciation Profitability Margin
#'
#' @description
#' Calculate the pre-depreciation profitability margin and append it to the dataframe. 
#'
#' @param expenses A character string indicating the column name for total functional expenses (On 990: Part IX, line 25A; On EZ: Not Available).
#' @param depreciation A character string indicating the column name for depreciation expenses (On 990: Part X, line 2B; On EZ:Not available).
#' @param revenue A character string indicating the column name for total revenue (On 990: Part VIII, line 12A; On EZ: Part I, line 9).
#' 
#' @return The original dataframe appended with the pre-depreciation profitability margin (`predpm`), 
#'  a winsorized version (`predpm.w`), a standardized z-score version (`predpm.z`), 
#'  and a percentile version (`predpm.p`).   
#'
#' @details Pre-depreciation profit is an income measure used to determine profit before incorporating 
#' non-cash expenses on a balance sheet. Pre-depreciation profit is calculated because it provides a cleaner 
#' number that can help determine a organization’s ability to service debt. Much like free cash flow, 
#' pre-depreciation profit is a measure of a organization’s actual cash flow. Non-expense items lower an 
#' organization’s reported earnings, so a pre-depreciation profit would show a higher profit in comparison to 
#' profits calculated after depreciation. High values in this metric are generally desirable since they 
#' indicate that an organization is not losing a lot of its revenues to expenses, though the amount of 
#' expense exempted from this metric due to depreciation (which for community development corporations can 
#' represent a large portion of their expenses), makes it less of an indicator of true profitability and more 
#' an indicator of an organization’s cash flow. Values close to zero are normal, and negative numbers 
#' indicate the organization is functioning at a deficit.
#' 
#' @examples
#' x1 <- rnorm( 1000,100,30 )
#' x2 <- rnorm( 1000,200,30 )
#' x3 <- rnorm( 1000,200,30 )
#' dat <- data.frame( x1, x2, x3 )
#' a <- get_predpm( df=dat, revenue='x1', depreciation='x2',
#'              expenses='x3' )
#' 
#' @export
get_predpm <- function( df, expenses, depreciation, revenue, winsorize=0.98 )
{
  
  # checks
  if( winsorize > 1 | winsorize < 0 )
  { stop( "winsorize argument must be 0 < w < 1" ) }
  
     num <- df[[ revenue ]] - ( df[[ expenses ]] + df[[ depreciation ]] )
     den <- df[[ revenue ]]  

  # can't divide by zero
  print( paste0( "Revenue cannot be zero: ", sum( den==0 ), " cases have been replaced with NA." ) )
  den[ den == 0 ] <- NA 
  
  predpm <- num / den
  
  top.p    <- 1 - (1-winsorize)/2
  bottom.p <- 0 + (1-winsorize)/2
  top      <- quantile( predpm, top.p, na.rm=T )
  bottom   <- quantile( predpm, bottom.p, na.rm=T )
  predpm.w    <- predpm
  predpm.w[ predpm.w > top    ] <- top
  predpm.w[ predpm.w < bottom ] <- bottom
  
  predpm.n <- scale( predpm.w )
  
  predpm.p <- dplyr::ntile( predpm, 100 )
  
  PREDPM <- data.frame( predpm, predpm.w, predpm.n, predpm.p )
  
  print( summary( PREDPM ) )
  
  par( mfrow=c(2,2) )
  plot( density( predpm,   na.rm=T ), main="Pre-Depreciation Profitability Margin (PREDPM)" )
  plot( density( predpm.w, na.rm=T ), main="PREDPM Winsorized" )
  plot( density( predpm.n, na.rm=T ), main="PREDPM Standardized as Z" )
  plot( density( predpm.p, na.rm=T ), main="PREDPM as Percentile" )
  
  df.predpm <- cbind( df, PREDPM )
  return( df.predpm )
}
