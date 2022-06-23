###---------------------------------------------------
###   SELF SUFFICIENCY RATIO
###---------------------------------------------------

#' @title
#' Self Sufficiency Ratio 
#'
#' @description
#' Calculate the self sufficiency ratio and append it to the dataframe. 
#'
#' @param df A \code{data.frame} containing the required field for computing the metric. The metric will be appended to this dataset.
#' @param prog.serv.rev Program service revenue, EOY (Part 8, Line 2g(A); On EZ: Part 1, Line 2).
#' @param total.expense total expenses, EOY (On 990: Part IX, line 25A; On EZ: Part 1, Line 17).
#' @param winsorize The winsorization value (between 0 and 1), defaults to 0.98 which winsorizes at 99th and 1st percentile values.   
#' 
#' @return Object of class \code{data.frame}: the original dataframe appended with the self sufficiency ratio (`ssr`), 
#'  a winsorized version (`ssr.w`), a standardized z-score version (`ssr.z`), 
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
#' x1 <- rnorm( 1000,100,30 )
#' x2 <- rnorm( 1000,200,30 )
#' x2[ c(15,300,600) ] <- 0
#' 
#' dat <- data.frame( x1,x2 )
#' 
#' # specify own column names
#' d <- get_ssr( df = dat, prog.serv.rev = "x1", total.expense = "x2" )
#' 
#' head( d )
#' 
#' # run with default column names
#' dat_01 <- dat
#' 
#' colnames( dat_01 ) <- c( 'F9_08_REV_PROG_TOT_TOT', 'F9_09_EXP_TOT_TOT' )
#' 
#' d <- get_ssr( dat_01 )
#' 
#' # winsorize at 0.025 and 0.975 percentiles instead of 0.01 and 0.99
#' d <- get_ssr( df = dat, prog.serv.rev = "x1", total.expense ="x2", winsorize=0.95 )
#' 
#' d <- get_ssr( dat_01, winsorize = 0.95 )
#' 
#' ## errors ##
#' 
#' # numerator not specified
#' d <- get_ssr( df = dat, prog.serv.rev = NULL, total.expense = 'x2' )
#' 
#' # denominator not specified
#' d <- get_ssr( df = dat, prog.serv.rev = 'x1', total.expense = NULL )
#' 
#' # neither numerator nor denominator specified
#' d <- get_ssr( df = dat, prog.serv.rev = NULL, total.expense = NULL )
#' 
#' @export
get_ssr <- function( df, prog.serv.rev = 'F9_08_REV_PROG_TOT_TOT', total.expense = 'F9_09_EXP_TOT_TOT', winsorize=0.98 )
{
  if( winsorize > 1 | winsorize < 0 )
  { stop( "winsorize argument must be 0 < w < 1" ) }
  
  if( is.null( prog.serv.rev )==T & is.null( total.expense )==F )
  { stop( "The numerator has been incorrectly specified. Ensure you are passing the correct data field to the correct argument." ) }
  
  if( is.null( prog.serv.rev )==F & is.null( total.expense )==T )
  { stop( "The denominator has been incorrectly specified. Ensure you are passing the correct data field to the correct argument." ) }
  
  if( is.null( prog.serv.rev )==T & is.null( total.expense )==T )
  { stop( "The argument fields are empty. Please supply column names for each argument or execute the function with default inputs." ) }
  
  p <- df[[ prog.serv.rev ]]
  e <- df[[ total.expense ]]
  

  # can't divide by zero
  print( paste0( "Total expenses cannot be equal to zero: ", sum( e==0 ), " cases have been replaced with NA." ) )
  e[ e == 0 ] <- NA 
  
  ssr <- p / e
  
  top.p    <- 1 - (1-winsorize)/2
  bottom.p <- 0 + (1-winsorize)/2
  top      <- quantile( ssr, top.p, na.rm=T )
  bottom   <- quantile( ssr, bottom.p, na.rm=T )
  ssr.w    <- ssr
  ssr.w[ ssr.w > top    ] <- top
  ssr.w[ ssr.w < bottom ] <- bottom
  
  ssr.n <- scale( ssr.w )
  
  ssr.p <- dplyr::ntile( ssr, 100 )
  
  SSR <- data.frame( ssr, ssr.w, ssr.n, ssr.p )
  
  print( summary( SSR ) )
  
  par( mfrow=c(2,2) )
  plot( density(ssr,   na.rm=T), main="Self Sufficiency Ratio (SSR)" )
  plot( density(ssr.w, na.rm=T), main="SSR Winsorized" )
  plot( density(ssr.n, na.rm=T), main="SSR Standardized as Z" )
  plot( density(ssr.p, na.rm=T), main="SSR as Percentile" )
  
  df.ssr <- data.frame( cbind( df, SSR ) )
  return( df.ssr )
}

  




