###---------------------------------------------------
###   ADD PIPE OPERATOR TO THE PACKAGE
###---------------------------------------------------
#' @importFrom magrittr "%>%"

#' @title
#' Current Ratio
#' 
#' @description 
#' Calculate the current ratio.
#' 
#' @param assets Current Assets (On 990: (Part X, line 1B) + (Part X, line 2B) + (Part X, line 3B) + (Part X, line 4B) + (Part X, line 8B) + (Part X, line 9B); On EZ: Part I, line 22 (cash and short-term investments only).
#' @param liabilities Current Liabilities (On 990: (Part X, line 17B) + (Part X, line 18B); On EZ: Not available).
#' @param winsorize The winsorization value (between 0 and 1), defaults to 0.98 which winsorizes at 99th and 1st percentile values.   
#' 
#' @return The original dataframe with the current ratio (`cr`),
#'  a winsorized version (`cr.w`), a stancrdized z-score version (`cr.z`), 
#'  and a percentile version (`cr.p`).  
#'  
#'  @details The current ratio is used to measure the overall liquidity of a nonprofit organization.
#'  In its simplest form, it shows how many dollars of current assets an organization has to cover its 
#'  current obligations. The higher the ratio, the more liquid the organization.
#'  As a rule of thumb, organizations should strive for a current ratio of 1.0 or higher. An organization
#'  with a ratio of 1.0 would have one dollar of assets to pay for every dollar of current liabilities.
#'   
#'   @examples 
#'   
#'   @export
get_cr<-function( df, assets, liabilities, winsorize=0.98 )
{
  a <- df[[ assets ]]
  l <- df[[ liabilities ]]
  
  if( winsorize > 1 | winsorize < 0 ){
    stop( 'winsorize argument must be 0 < w < 1' )
  }

  print(paste0('Liabilities cannot be equal zero:',sum(l==0),'cases have been replaced with NA'))
  
  l[[ l==0 ]] <- NA
  
  cr <- a/l
  
  top.p    <- 1 - (1-winsorize)/2
  bottom.p <- 0 + (1-winsorize)/2
  top      <- quantile( cr, top.p, na.rm=T )
  bottom   <- quantile( cr, bottom.p, na.rm=T )
  cr.w    <- cr
  cr.w[ cr.w > top    ] <- top
  cr.w[ cr.w < bottom ] <- bottom
  
  cr.n <- scale( cr.w )
  
  cr.p <- dplyr::ntile( cr, 100 )
  
  CR <- data.frame( cr, cr.w, cr.n, cr.p )
  
  print( summary( CR ) )
  
  par( mfrow=c(2,2) )
  plot( density(cr,   na.rm=T), main="Debt to Asset Ratio (CR)" )
  plot( density(cr.w, na.rm=T), main="CR Winsorized" )
  plot( density(cr.n, na.rm=T), main="CR Stancrdized as Z" )
  plot( density(cr.p, na.rm=T), main="CR as Percentile" )
  
  df.cr <- cbind( df, CR )
  return( df.cr )
}


  
