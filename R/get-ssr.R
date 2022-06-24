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
# x1 <- rnorm( 1000,100,30 )
# x2 <- rnorm( 1000,200,30 )
# x2[ c(15,300,600) ] <- 0
# 
# dat <- data.frame( x1, x2 )
# 
# # specify own column names
# d <- get_ssr( df = dat, prog.serv.rev = "x1", total.expense = "x4" )
# 
# head( d )
# 
# # run with default column names
# x3 <- rnorm( 1000,100,30 )
# x4 <- rnorm( 1000,200,30 )
# x3[ seq( from = 1, to = 1000, 50 ) ] <- NA
# x4[ seq( from = 1, to = 1000, 71 ) ] <- NA
# 
# dat_01 <- data.frame( x1, x2, x3, x4 )
# 
# colnames( dat_01 ) <- c( 'F9_08_REV_PROG_TOT_TOT', 'F9_09_EXP_TOT_TOT',
#                          'F9_01_REV_PROG_TOT_CY', 'F9_01_EXP_TOT_CY')
# 
# # run only with 990 variable names
# d <- get_ssr( dat_01, prog.serv.rev = "F9_08_REV_PROG_TOT_TOT", total.expense = "F9_09_EXP_TOT_TOT" )
# 
# #run only with 990-EZ variable names
# d <- get_ssr( dat_01, prog.serv.rev = "F9_01_REV_PROG_TOT_CY", total.expense = "F9_01_EXP_TOT_CY" )
# 
# 
# # winsorize at 0.025 and 0.975 percentiles instead of 0.01 and 0.99
# d <- get_ssr( df = dat, prog.serv.rev = "x1", total.expense ="x2", winsorize=0.95 )
# 
# d <- get_ssr( dat_01, winsorize = 0.95 )
# 
# ## errors ##
# 
# # numerator not specified
# d <- get_ssr( df = dat, prog.serv.rev = NULL, total.expense = 'x2' )
# 
# # denominator not specified
# d <- get_ssr( df = dat, prog.serv.rev = 'x1', total.expense = NULL )
# 
# # neither numerator nor denominator specified
# d <- get_ssr( df = dat, prog.serv.rev = NULL, total.expense = NULL )
# 
# # column names vector not of correct length
# d <- get_ssr( df = dat, prog.serv.rev = c('a','b','c'), total.expense = 'a' )
# 
# # column names vector not of correct length
# d <- get_ssr( df = dat, prog.serv.rev = 'a', total.expense = c( 'a', 'b', 'c' ) )
#' 
#' @export
get_ssr <- function( df, prog.serv.rev = c( 'F9_08_REV_PROG_TOT_TOT', 'F9_01_REV_PROG_TOT_CY' ), 
                     total.expense = c( 'F9_09_EXP_TOT_TOT', 'F9_01_EXP_TOT_CY' ), 
                     winsorize=0.98 )
{
  # quoted/unquoted arguments
  if( !is.null( substitute( prog.serv.rev ) ) )   prog.serv.rev   <- rm_quote( deparse( substitute( prog.serv.rev ) ) )
  if( !is.null( substitute( total.expense ) ) )   total.expense   <- rm_quote( deparse( substitute( total.expense ) ) )
  
  if( winsorize > 1 | winsorize < 0 )
  { stop( "winsorize argument must be 0 < w < 1" ) }
  
  if( is.null( prog.serv.rev )==T & is.null( total.expense )==F )
  { stop( "The numerator has been incorrectly specified. Ensure you are passing the correct data field to the correct argument." ) }
  
  if( is.null( prog.serv.rev )==F & is.null( total.expense )==T )
  { stop( "The denominator has been incorrectly specified. Ensure you are passing the correct data field to the correct argument." ) }
  
  if( is.null( prog.serv.rev )==T & is.null( total.expense )==T )
  { stop( "The argument fields are empty. Please supply column names for each argument or execute the function with default inputs." ) }
  
  if( length( prog.serv.rev ) > 2 | length( prog.serv.rev ) < 1 )
  { stop( "`prog.serv.rev` must be a single quoted or unquoted string or a vector with a minimum length of one and maximum length of two." ) }
  
  if( length( total.expense ) > 2 | length( total.expense ) < 1 )
  { stop( "`total.expense` must be a single quoted or unquoted string or a vector with a minimum length of one and maximum length of two." ) }
  
  
  # copy data
  dat <- df
  
  
  if ( length( prog.serv.rev )==2 & length( total.expense )==2 ) {
    
    # create a column that concatenates two numerator variables into single column
    dat[ is.na( dat[ prog.serv.rev[2] ] )==F, 'p' ] <- dat[ which( is.na( dat[ prog.serv.rev[2] ] )==F ), prog.serv.rev[2] ]
    dat[ is.na( dat[ prog.serv.rev[1] ] )==F, 'p' ] <- dat[ which( is.na( dat[ prog.serv.rev[1] ] )==F ), prog.serv.rev[1] ]
    
    # create a column that concatenates two denominator variables into single column
    dat[ is.na( dat[ total.expense[2] ] )==F, 'e' ] <- dat[ which( is.na( dat[ total.expense[2] ] )==F ), total.expense[2] ]
    dat[ is.na( dat[ total.expense[1] ] )==F, 'e' ] <- dat[ which( is.na( dat[ total.expense[1] ] )==F ), total.expense[1] ]
    
    
    p <- dat[[ 'p' ]]
    e <- dat[[ 'e' ]]
  }
  
  else if ( length( prog.serv.rev )==2 & length( total.expense )==1 ) {
    
    # create a column that concatenates two denominator variables into single column
    dat[ is.na( dat[ prog.serv.rev[2] ] )==F, 'p' ] <- dat[ which( is.na( dat[ prog.serv.rev[2] ] )==F ), prog.serv.rev[2] ]
    dat[ is.na( dat[ prog.serv.rev[1] ] )==F, 'p' ] <- dat[ which( is.na( dat[ prog.serv.rev[1] ] )==F ), prog.serv.rev[1] ]
    
    
    p <- dat[[ 'p' ]]
    e <- dat[[ total.expense ]]
  }
  
  else if ( length( prog.serv.rev )==1 & length( total.expense )==2 ) {
    
    # create a column that concatenates two numerator variables into single column
    dat[ is.na( dat[ total.expense[2] ] )==F, 'e' ] <- dat[ which( is.na( dat[ total.expense[2] ] )==F ), total.expense[2] ]
    dat[ is.na( dat[ total.expense[1] ] )==F, 'e' ] <- dat[ which( is.na( dat[ total.expense[1] ] )==F ), total.expense[1] ]
    
    
    p <- dat[[ prog.serv.rev ]]
    e <- dat[[ 'e' ]]
  }
  
  else if ( length( prog.serv.rev )==1 & length( total.expense )==1 ) {
    
    p <- dat[[ prog.serv.rev ]]
    e <- dat[[ total.expense ]]
  }
  
  
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


d <-get_ssr( part010810, prog.serv.rev = 'F9_01_REV_PROG_TOT_CY', 
             total.expense = 'F9_01_EXP_TOT_CY')


  




