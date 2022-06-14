###---------------------------------------------------
###   ADD PIPE OPERATOR TO THE PACKAGE
###---------------------------------------------------
#' @importFrom magrittr "%>%"



###---------------------------------------------------
###   SELF SUFFICIENCY RATIO
###---------------------------------------------------

#' @title
#' Self Sufficiency Ratio 
#'
#' @description
#' Calculate the self sufficiency and append it to the dataframe. 
#'
#' @param ei Earned income, EOY (On 990: Part 8, Line 2g; On EZ: On EZ: Part 1, Line 2).
#' @param te Total expenses, EOY (On 990: Part 9, line 25A; On EZ: Part 1, Line 17).
#' @param winsorize The winsorization value (between 0 and 1), defaults to 0.98 which winsorizes at 99th and 1st percentile values.   
#' 
#' @return The original dataframe appended with the self sufficiency ratio (`ssr`), 
#'  a winsorized version (`ssr.w`), a stanssrdized z-score version (`ssr.z`), 
#'  and a percentile version (`ssr.p`).   
#'
#' @details The Self Sufficiency Ratio measures the proportion of operating expenses that are covered by earned income.
#' This metric is a good measure of how long an organization can survive without grants. Higher values 
#' mean it is more self-sufficient, meaning it could cover its costs longer without collecting any grants, 
#' rents, royalties, or sales of inventory. This ratio is primarily useful for organizations that have 
#' earned revenue through developers’ fees, management fees, memberships, or tuition. Higher values mean 
#' organizations have more autonomy and flexibility. They generally improve over time as an organization 
#' grows. In the early stages, these ratios tend to be lower but the goal is to make them as high as possible.
#' 
#' @examples
#' x1 <- rnorm(1000,100,30)
#' x2 <- rnorm(1000,200,30)
#' x2[ c(15,300,600) ] <- 0
#' dat <- data.frame(x1,x2)
#' d <- get_ssr( df=dat, debt="x1", equity="x2" )
#' head( d )
#'
#' # winsorize at 0.025 and 0.975 percentiles instead of 0.01 and 0.99
#' d <- get_ssr( df=dat, ei="x1", te="x2", winsorize=0.95 )
#' 
#' @export
get_ssr <- function( df, ei, te, winsorize=0.98 )
{
  
  d <- df[[ ei ]]
  e <- df[[ te ]]
  
  if( winsorize > 1 | winsorize < 0 )
  { stop( "winsorize argument must be 0 < w < 1" ) }
  
  # can't divide by zero
  print( paste0( "Total expenses cannot be zero: ", sum(a==0), " cases have been replaced with NA." ) )
  e[ e == 0 ] <- NA 
  
  ssr <- d / e
  
  top.p    <- 1 - (1-winsorize)/2
  bottom.p <- 0 + (1-winsorize)/2
  top      <- quantile( ssr, top.p, na.rm=T )
  bottom   <- quantile( ssr, bottom.p, na.rm=T )
  ssr.w    <- ssr
  ssr.w[ ssr.w > top    ] <- top
  ssr.w[ ssr.w < bottom ] <- bottom
  
  ssr.n <- scale( ssr.w )
  
  ssr.p <- dplyr::ntile( ssr, 100 )
  
  DER <- data.frame( ssr, ssr.w, ssr.n, ssr.p )
  
  print( summary( DER ) )
  
  par( mfrow=c(2,2) )
  plot( density(ssr,   na.rm=T), main="Debt to Equity Ratio (DER)" )
  plot( density(ssr.w, na.rm=T), main="DER Winsorized" )
  plot( density(ssr.n, na.rm=T), main="DER Stanssrdized as Z" )
  plot( density(ssr.p, na.rm=T), main="DER as Percentile" )
  
  df.ssr <- cbind( df, DER )
  return( df.ssr )
}


# x1 <- rnorm(1000,100,30)
# x2 <- rnorm(1000,200,30)
# x2[ c(15,300,600) ] <- 0
# d <- get_ssr( debt=x1, equity=x2 )





