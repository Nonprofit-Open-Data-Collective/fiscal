###---------------------------------------------------
###   ADD PIPE OPERATOR TO THE PACKAGE
###---------------------------------------------------
#' @importFrom magrittr "%>%"



###---------------------------------------------------
###   DEBT TO EQUITY RATIO
###---------------------------------------------------

#' @title
#' Debt to Equity Ratio 
#'
#' @description
#' Calculate the debt to equity ratio and append it to the dataframe. 
#'
#' @param debt Total debt, EOY (On 990: Part X, line 17B; On EZ: Not Available).
#' @param equity Unrestricted net assets, EOY (On 990: Part X, line 27B; On EZ: Not Available).
#' @param winsorize The winsorization value (between 0 and 1), defaults to 0.98 which winsorizes at 99th and 1st percentile values.   
#' 
#' @return The original dataframe appended with the debt to asset ratio (`der`), 
#'  a winsorized version (`der.w`), a standerdized z-score version (`der.z`), 
#'  and a percentile version (`der.p`).   
#'
#' @details Debt to Equity Ratio is a a leverage ratio that defines the total amount of debt relative to the unrestricted assets owned by an organization.
#' This metric shows the big picture view of how much an organization owes relative to what it owns. 
#' Higher values mean it is more highly leveraged, meaning it has low capacity for future borrowing. 
#' As an example: if an organization has a total-debt-to-net-assets ratio of 0.4, 40% of its assets 
#' are financed by creditors, and 60% are its own unrestricted, available equity. As this percentage 
#' creeps above the 50% mark, it can call into question the organization’s ability to manage debt, 
#' which could jeopardize the delivery of programs and services. However, for developers, extremely 
#' low values may mean the organization is not capitalizing enough on its equity to expand.
#' This value can be negative if an organization has either overpaid on its debts or if it has negative 
#' unrestricted net assets.One limitation of this metric is that it does not provide any indication of 
#' asset quality or liquidity since it lumps tangible and intangible assets together.
#' 
#' @examples
#' x1 <- rnorm(1000,100,30)
#' x2 <- rnorm(1000,200,30)
#' x2[ c(15,300,600) ] <- 0
#' dat <- data.frame(x1,x2)
#' d <- get_der( df=dat, debt="x1", equity="x2" )
#' head( d )
#'
#' # winsorize at 0.025 and 0.975 percentiles instead of 0.01 and 0.99
#' d <- get_der( df=dat, debt="x1", assets="x2", winsorize=0.95 )
#' 
#' @export
get_der <- function( df, debt, equity, winsorize=0.98 )
{
  
  d <- df[[ debt ]]
  e <- df[[ equity ]]
  
  if( winsorize > 1 | winsorize < 0 )
  { stop( "winsorize argument must be 0 < w < 1" ) }
  
  # can't divide by zero
  print( paste0( "Assets cannot be zero: ", sum(a==0), " cases have been replaced with NA." ) )
  e[ e == 0 ] <- NA 
  
  der <- d / e
  
  top.p    <- 1 - (1-winsorize)/2
  bottom.p <- 0 + (1-winsorize)/2
  top      <- quantile( der, top.p, na.rm=T )
  bottom   <- quantile( der, bottom.p, na.rm=T )
  der.w    <- der
  der.w[ der.w > top    ] <- top
  der.w[ der.w < bottom ] <- bottom
  
  der.n <- scale( der.w )
  
  der.p <- dplyr::ntile( der, 100 )
  
  DER <- data.frame( der, der.w, der.n, der.p )
  
  print( summary( DER ) )
  
  par( mfrow=c(2,2) )
  plot( density(der,   na.rm=T), main="Debt to Equity Ratio (DER)" )
  plot( density(der.w, na.rm=T), main="DER Winsorized" )
  plot( density(der.n, na.rm=T), main="DER Standerdized as Z" )
  plot( density(der.p, na.rm=T), main="DER as Percentile" )
  
  df.der <- cbind( df, DER )
  return( df.der )
}


# x1 <- rnorm(1000,100,30)
# x2 <- rnorm(1000,200,30)
# x2[ c(15,300,600) ] <- 0
# d <- get_der( debt=x1, equity=x2 )





