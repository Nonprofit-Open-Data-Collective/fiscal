###---------------------------------------------------
###   ADD PIPE OPERATOR TO THE PACKAGE
###---------------------------------------------------
#' @importFrom magrittr "%>%"



###---------------------------------------------------
###   INVESTMENT INCOME DEPENDENCE RATIO
###---------------------------------------------------

#' @title
#' Investment Income Dependence Ratio
#'
#' @description
#' Calculate the investment income dependence ratio and append it to the dataframe. 
#'
#' @param df A dataframe containing the required field for computing the metric. The metric will be appended to this dataset.
#' @param invest.income A character string indicating the column name for investment income (On 990: Part VIII, Line 3(A); On EZ: Not Available).
#' @param bond.proceeds A character string indicating the column name for investment income from tax-exempt bond proceeds (On 990: Part VIII, Line 4(A); On EZ: Not Available).
#' @param rent.income A character string indicating the column name for gross rent income (On 990: Part VIII, Line 6(A); On EZ: Not Available).
#' @param other.income A character string indicating the column name for gross income from sales of assets other than inventory  (On 990: Part VIII, Line 7(A); On EZ: Not Available). 
#' @param total.revenue A character string indicating the column name for total revenue (On 990: (Part VIII, Line 12A); On EZ: Part I, Line 9).
#' 
#' @return The original dataframe appended with the investment income dependence ratio (`iidr`), 
#'  a winsorized version (`iidr.w`), a standardized z-score version (`iidr.z`), 
#'  and a percentile version (`iidr.p`).   
#'
#' @details This metric shows how much of an organization’s total revenues come from investment income 
#' (interest, dividents, gains/losses on sales of securities or other assets). High levels in this metric 
#' indicate an organization is more depenent on investment income (and thus vulnerable to market downturns), 
#' and low values indicate their income comes more from donations or earned income.
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
#' a<-get_iidr( df=dat, invest.income='x1', bond.proceeds='x2', rent.income='x3', other.income='x4', total.revenue='x5', winsorize=0.98)
#'
#' head( a )
#' 
#' @export
get_iidr <- function( df, invest.income, bond.proceeds, rent.income, other.income, total.revenue, winsorize=0.98 )
{
  
  # checks
  if( winsorize > 1 | winsorize < 0 )
  { stop( "winsorize argument must be 0 < w < 1" ) }
  
  num <- df[[ invest.income ]] + df[[ bond.proceeds ]]+ df[[ rent.income ]] + df[[ other.income ]]
  r <- df[[ total.revenue ]]
  
  # can't divide by zero
  print( paste0( "Total revenue cannot be zero: ", sum( r==0 ), " cases have been replaced with NA." ) )
  r[ r == 0 ] <- NA 
  
  iidr <- num / r
  
  top.p    <- 1 - (1-winsorize)/2
  bottom.p <- 0 + (1-winsorize)/2
  top      <- quantile( iidr, top.p, na.rm=T )
  bottom   <- quantile( iidr, bottom.p, na.rm=T )
  iidr.w    <- iidr
  iidr.w[ iidr.w > top    ] <- top
  iidr.w[ iidr.w < bottom ] <- bottom
  
  iidr.n <- scale( iidr.w )
  
  iidr.p <- dplyr::ntile( iidr, 100 )
  
  IIDR <- data.frame( iidr, iidr.w, iidr.n, iidr.p )
  
  print( summary( IIDR ) )
  
  par( mfrow=c(2,2) )
  plot( density( iidr,   na.rm=T ), main="Investment Income Dependence Ratio (IIDR)" )
  plot( density( iidr.w, na.rm=T ), main="IIDR Winsorized" )
  plot( density( iidr.n, na.rm=T ), main="IIDR Standardized as Z" )
  plot( density( iidr.p, na.rm=T ), main="IIDR as Percentile" )
  
  df.iidr <- cbind( df, IIDR )
  return( df.iidr )
}
