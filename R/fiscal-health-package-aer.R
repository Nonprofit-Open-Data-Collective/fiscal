###---------------------------------------------------
###   ADD PIPE OAERATOR TO THE PACKAGE
###---------------------------------------------------
#' @importFrom magrittr "%>%"



###---------------------------------------------------
###   ADMINISTRATION EXPENSE RATIO
###---------------------------------------------------

#' @title
#' Administration Expense Ratio
#'
#' @description
#' Calculate the administration expense ratio and append it to the dataframe. 
#'
#' @param df A dataframe containing the required field for computing the metric. The metric will be appended to this dataset.
#' @param mgmt.ge A character string indicating the column name for management and general expenses (On 990: Part 9, Line 25(C); On EZ: Not Available.
#' @param total.expense A character string indicating the column name for total expenses (On 990: Part 9, Line 25(A); On EZ: Part 1, Line 17).
#' 
#' @return The original dataframe appended with the administration expense ratio (`aer`), 
#'  a winsorized version (`aer.w`), a standardized z-score version (`aer.z`), 
#'  and a percentile version (`aer.p`).   
#'
#' @details The administrative expense ratio measures the percentage of an organization’s expenses that are 
#' being allocated to administrative costs. High values in this ratio indicate that more of an organization's 
#' expenses are going towards its management and general expenses (or overhead) while a lower number indicates 
#' that an organization's funds are going towards program service or fundraising expenses. Charity Navigator 
#' generally gives its highest rankings to organizations that spend less than 15% of expenses on overhead. The 
#' Better Business Bureau’s Wise Giving Alliance recommends a ratio of less than 35%.
#' 
#' @examples
#' x1<-rnorm( 1000,100,30 )
#' x2<-rnorm( 1000,200,30 )
#' x2[ c(1,10,100) ] <- 0
#' 
#' dat<-data.frame( x1, x2 )
#' 
#' a<-get_aer( df=dat, mgmt.ge='x1', total.expense='x2' )
#' 
#' head( a )
#' 
#' @export
get_aer <- function( df, mgmt.ge, total.expense, winsorize=0.98 )
{
  
  # checks
  if( winsorize > 1 | winsorize < 0 )
  { stop( "winsorize argument must be 0 < w < 1" ) }
  
  m <- df[[ mgmt.ge ]]
  e <- df[[ total.expense ]]
  
  # can't divide by zero
  print( paste0( "Total expenses cannot be zero: ", sum( e==0 ), " cases have been replaced with NA." ) )
  e[ e == 0 ] <- NA 
  
  aer <- m / e
  
  top.p    <- 1 - (1-winsorize)/2
  bottom.p <- 0 + (1-winsorize)/2
  top      <- quantile( aer, top.p, na.rm=T )
  bottom   <- quantile( aer, bottom.p, na.rm=T )
  aer.w    <- aer
  aer.w[ aer.w > top    ] <- top
  aer.w[ aer.w < bottom ] <- bottom
  
  aer.n <- scale( aer.w )
  
  aer.p <- dplyr::ntile( aer, 100 )
  
  AER <- data.frame( aer, aer.w, aer.n, aer.p )
  
  print( summary( AER ) )
  
  par( mfrow=c(2,2) )
  plot( density( aer,   na.rm=T ), main="Administration Expense Ratio (AER)" )
  plot( density( aer.w, na.rm=T ), main="AER Winsorized" )
  plot( density( aer.n, na.rm=T ), main="AER Standardized as Z" )
  plot( density( aer.p, na.rm=T ), main="AER as Percentile" )
  
  df.aer <- cbind( df, AER )
  return( df.aer )
}
