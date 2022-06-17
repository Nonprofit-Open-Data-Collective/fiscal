###---------------------------------------------------
###   ADD PIPE OPERATOR TO THE PACKAGE
###---------------------------------------------------
#' @importFrom magrittr "%>%"



###---------------------------------------------------
###   PROGRAM EFFICIENCY RATIO
###---------------------------------------------------

#' @title
#' Program Efficiency Ratio
#'
#' @description
#' Calculate the assets the program efficiency ratio and append it to the dataframe. 
#'
#' @param df A dataframe containing the required field for computing the metric. The metric will be appended to this dataset.
#' @param pse A character string indicating the column name for program service expenses (On 990: Part 9, Line 25B; On EZ: Pt II, Line 25B).
#' @param total.expense A character string indicating the column name for total expenses (On 990: Part 1, Line 18(B); On EZ: Part 1, Line 17).
#' 
#' @return The original dataframe appended with the program efficiency ratio (`per`), 
#'  a winsorized version (`per.w`), a standardized z-score version (`per.z`), 
#'  and a percentile version (`per.p`).   
#'
#' @details The program efficiency ratio measures the percentage of expenses that a nonprofit organization is 
#' spending on its core mission. High values in this ratio indicate that more of an organization's expenses 
#' are going towards its core mission or program services while a lower number indicates that an organization's 
#' expenses are comprised of other things like management or fundraising. Charity Navigator generally gives 
#' the highest rankings to those organizations whose ratio of program expenses is 85% or higher of their total 
#' expenses. Other agencies, such as the Better Business Bureau’s Wise Giving Alliance, recommend a ratio of 
#' 65% or higher.
#' 
#' @examples
#' x1<-rnorm( 1000,100,30 )
#' x2<-rnorm( 1000,200,30 )
#' x2[ c(1,10,100) ] <- 0
#' 
#' dat<-data.frame( x1, x2 )
#'
#' a<-get_per( df=dat, pse='x1', total.expense='x2' )
#' 
#' head( a )
#' 
#' @export
get_per <- function( df, pse, total.expense, winsorize=0.98 )
{
  
  # checks
  if( winsorize > 1 | winsorize < 0 )
  { stop( "winsorize argument must be 0 < w < 1" ) }
  
  t <- df[[ pse ]]
  e <- df[[ total.expense ]]
  
  # can't divide by zero
  print( paste0( "Total expenses cannot be zero: ", sum( e==0 ), " cases have been replaced with NA." ) )
  e[ e == 0 ] <- NA 
  
  per <- t / e
  
  top.p    <- 1 - (1-winsorize)/2
  bottom.p <- 0 + (1-winsorize)/2
  top      <- quantile( per, top.p, na.rm=T )
  bottom   <- quantile( per, bottom.p, na.rm=T )
  per.w    <- per
  per.w[ per.w > top    ] <- top
  per.w[ per.w < bottom ] <- bottom
  
  per.n <- scale( per.w )
  
  per.p <- dplyr::ntile( per, 100 )
  
  PER <- data.frame( per, per.w, per.n, per.p )
  
  print( summary( PER ) )
  
  par( mfrow=c(2,2) )
  plot( density( per,   na.rm=T ), main="Program Efficiency Ratio (PER)" )
  plot( density( per.w, na.rm=T ), main="PER Winsorized" )
  plot( density( per.n, na.rm=T ), main="PER Standardized as Z" )
  plot( density( per.p, na.rm=T ), main="PER as Percentile" )
  
  df.per <- cbind( df, PER )
  return( df.per )
}
