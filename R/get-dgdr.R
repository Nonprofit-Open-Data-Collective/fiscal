###---------------------------------------------------
###   DONATION/GRANT DEPENDENCE RATIO
###---------------------------------------------------

#' @title
#' Donation/Grant Dependence Ratio
#'
#' @description
#' Calculate the donation/grant dependence ratio and append it to the dataframe. 
#'
#' @param df A \code{data.frame} containing the required field for computing the metric. The metric will be appended to this dataset.
#' @param total.contributions Column name for total contributions (can be quoted or unquoted) (On 990: Part VIII, Line 1h(A); On EZ: Not Available) with the default name supplied.
#' @param fund.income Column name for fundraising income (can be quoted or unquoted) (On 990: Part VIII, Line 8c(A); On EZ: Not Available) with the default name supplied.
#' @param total.revenue Column name for fundraising income (can be quoted or unquoted) (On 990: Part VIII, Line 12A; On EZ: Part I, Line 9) with the default name supplied.
#' @param winsorize The winsorization value (between 0 and 1), defaults to 0.98 which winsorizes at 99th and 1st percentile values.   
#' @param numerator A character string indicating the user-supplied column name for a pre-aggregated variable for the numerator (CHANGE). Do not combine with numerator column component arguments (`total.contributions`, `fund.income`). 
#' @param denominator A character string indicating the user-supplied column name for a pre-aggregated variable for the denominator (CHANGE). Do not combine with denominator column component arguments (`total.revenue`). 

#' @usage get_dgdr( df, total.contributions = "F9_08_REV_CONTR_TOT", fund.income = "F9_08_REV_OTH_FUNDR_NET_TOT", total.revenue = "F9_08_REV_TOT_TOT", numerator = NULL, denominator = NULL, winsorize = 0.98 )
#' 
#' @return Object of class \code{data.frame}: the original dataframe appended with the donation/grant dependence ratio (`dgdr`), 
#'  a winsorized version (`dgdr.w`), a standardized z-score version (`dgdr.z`), 
#'  and a percentile version (`dgdr.p`).   
#'
#' @details This metric shows how much of an organization’s total revenues come from contributions, government 
#' grants, and special event revenues. High levels in this indicator are undesirable since that means that an 
#' organization’s revenues are volatile insofar as it is dependent on contributions that are highly likely to 
#' contract during economic downturns. Low values in this indicator mean an organization is not dependent on 
#' donations. Note: computation of this metric is available to only 990 filers and not for 990-EZ filers. The default 
#' inputs use column names for variables available only to 990 filers.
#' 
#' @import dplyr
#' @import stringr
#' @import magrittr
#' 
#' @examples
#' library( fiscal )
#' x1 <- rnorm( 1000, 100, 30 )
#' x2 <- rnorm( 1000, 200, 30 )
#' x3 <- rnorm( 1000, 200, 30 )
#' x3[ c( 15, 300, 600 ) ] <- 0
#' 
#' dat <- data.frame( x1, x2, x3 )
#' 
#' # specify own column names
#' d <- get_dgdr( df = dat, total.contributions = "x1", fund.income = "x2", total.revenue = "x3" )
#' 
#' 
#' head( d ) 
#' 
#' # run with default column names
#' x1[ seq( from = 1, to = 1000, 50 ) ] <- NA
#' x2[ seq( from = 1, to = 1000, 71 ) ] <- NA
#' 
#' dat_01 <- data.frame( x1, x2, x3 )
#' 
#' colnames( dat_01 ) <- c( "F9_08_REV_CONTR_TOT", "F9_08_REV_OTH_FUNDR_NET_TOT", "F9_08_REV_TOT_TOT" )
#' 
#' d <- get_dgdr( dat_01 )
#' 
#' # coerce one column to factor
#' dat_01$F9_08_REV_OTH_FUNDR_NET_TOT <- as.factor( dat_01$F9_08_REV_OTH_FUNDR_NET_TOT )
#' 
#' d <- get_dgdr( dat_01 )
#' 
#' # winsorize at 0.025 and 0.975 percentiles instead of 0.01 and 0.99
#' d <- get_dgdr( df = dat, total.contributions = "x1", fund.income = "x2", total.revenue = "x3", winsorize = 0.95 )
#' 
#' d <- get_dgdr( dat_01, winsorize = 0.95 )
#' 
#' 
#' # using 990 data
#' data( part010810 )
#' d <- get_dgdr( df = part010810 )
#' 
#' # now coerce one of the variables to numeric
#' part010810$F9_09_EXP_TOT_TOT <- as.character( part010810$F9_09_EXP_TOT_TOT )
#' 
#' d <- get_dgdr( df = part010810 )
#' 
#' \dontrun{
#' ## Errors ##
#' 
#' # numerator not specified
#' d <- get_dgdr( df = dat, total.contributions = NULL, fund.income = NULL, total.revenue = "x3" )
#' 
#' # denominator not specified
#' d <- get_dgdr( df = dat, total.contributions = "x1", fund.income = "x2", total.revenue = NULL )
#' 
#' # neither numerator nor denominator specified
#' d <- get_dgdr( df = dat, total.contributions = NULL, fund.income = NULL, total.revenue = NULL )
#' 
#' # column names vector not of correct length
#' d <- get_dgdr( df = dat, total.contributions = c("e","b","c"), fund.income = "x2", total.revenue = "e")
#' 
#' # column names vector not of correct length
#' d <- get_dgdr( df = dat, total.contributions = "e", total.revenue = c( "e", "b", "c"), fund.income = "x2" )
#' }
#' 
#' @export
get_dgdr <- function( df, 
                      total.contributions = "F9_08_REV_CONTR_TOT", 
                      fund.income = "F9_08_REV_OTH_FUNDR_NET_TOT", 
                      total.revenue = "F9_08_REV_TOT_TOT", 
                      numerator = NULL,
                      denominator = NULL,
                      winsorize=0.98 )
{
  
  # checks
  if( winsorize > 1 | winsorize < 0 )
  { stop( "winsorize argument must be 0 < w < 1" ) }
  
  if ( is.null( total.contributions )==F & is.null( fund.income )==F & is.null( total.revenue )==F ) {
    
    if ( total.contributions == "F9_08_REV_CONTR_TOT" & fund.income == "F9_08_REV_OTH_FUNDR_NET_TOT" &
         total.revenue == "F9_08_REV_TOT_TOT" & is.null( numerator )==F & is.null( denominator )==F ){
      
      warning( "Default argument inputs overridden with specified numerator and denominator arguments" ) 
      
      total.contributions <- NULL
      fund.income <- NULL
      total.revenue <- NULL
      
    }
    
  } 
  
  if ( ( ( length( c( total.contributions, fund.income ) ) < 2 )==F | is.null( numerator )==F ) &
       ( ( is.null( total.revenue )==T ) &
         is.null( denominator )==T ) )
  { stop( "The denominator has been incorrectly specified. Ensure you are passing the correct data field to the correct argument." ) }
  
  if ( ( ( length( c( total.revenue ) ) == 0 )==F | is.null( denominator )==F ) &
       ( ( is.null( total.contributions )==T | is.null( fund.income )==T ) &
         is.null( numerator )==T ) )
  { stop( "The numerator has been incorrectly specified. Ensure you are passing the correct data field to the correct argument." ) }
  
  
  if ( ( length( c( total.contributions, fund.income ) ) <= 2 ) &
       ( length( c( total.contributions, fund.income ) ) >= 1 ) & 
       ( is.null( numerator )==F ) )
  { stop( "The numerator has been incorrectly specified with conflicting arguments. Ensure you are passing the correct data field to the correct argument." ) }
  
  if ( ( length( c( total.revenue ) ) >= 1 ) & 
       ( is.null( denominator )==F ) )
  { stop( "The denominator has been incorrectly specified with conflicting arguments. Ensure you are passing the correct data field to the correct argument." ) }
  
  if( ( length( total.contributions ) > 1 | length( total.contributions ) < 1 ) & is.null( numerator ) == T ) 
  { stop( "`total.contributions` must be a single quoted string or a vector with a maximum length of one." ) }
  
  if( ( length( fund.income ) > 1 | length( fund.income ) < 1 ) & is.null( numerator ) == T ) 
  { stop( "`fund.income` must be a single quoted string or a vector with a maximum length of one." ) }
  
  if( ( length( total.revenue ) > 1 | length( total.revenue ) < 1 ) & is.null( denominator ) == T ) 
  { stop( "`total.revenue` must be a single quoted string or a vector with a maximum length of one." ) }
  
  if( ( length( numerator ) > 1 & length( c( total.contributions, fund.income ) ) == 0 ) )
  { stop( "`numerator` must be a single quoted string or a vector with a maximum length of one." ) }
  
  if( ( length( denominator ) > 1 & length( c( total.revenue ) ) == 0 ) )
  { stop( "`denominator` must be a single quoted string or a vector with a maximum length of one." ) }
  
  
  
  # copy data
  dat <- df
  
  ## ensure variable classes are numeric ##
  
  # run coerce_numeric and loop through all variables required and that matched by the two input arguments
  v <- c( colnames( dat )[which( colnames( dat ) %in% total.contributions ) ], colnames( dat )[which( colnames( dat ) %in% fund.income )],
          colnames( dat )[which( colnames( dat ) %in% total.revenue )],colnames( dat )[which( colnames( dat ) %in% numerator )],
          colnames( dat )[which( colnames( dat ) %in% denominator )] )
  
  dat <- coerce_numeric( d = dat, vars = v )
  
  if( is.null( numerator) == T & is.null( denominator ) == T ){
    
    num <- dat[[ total.contributions ]] + dat[[ fund.income ]]
    den <- dat[[ total.revenue ]]
    
  }
  
  if( is.null( numerator) == F & is.null( denominator ) == T ){
    
    num <- dat[[ numerator ]]
    den <- dat[[ total.revenue ]]
    
  }
  
  if( is.null( numerator) == T & is.null( denominator ) == F ){
    
    num <- dat[[ total.contributions ]] + dat[[ fund.income ]]
    den <- dat[[ denominator ]]
    
  }
  
  if( is.null( numerator) == F & is.null( denominator ) == F ){
    
    num <- dat[[ numerator ]]
    den <- dat[[ denominator ]]
    
  }
  
  # can't divide by zero
  print( paste0( "Net assets cannot be zero: ", sum( den==0, na.rm = T ), " cases have been replaced with NA." ) )
  den[ den == 0 ] <- NA 
  
  dgdr <- num / den
  
  top.p    <- 1 - (1-winsorize)/2
  bottom.p <- 0 + (1-winsorize)/2
  top      <- quantile( dgdr, top.p, na.rm=T )
  bottom   <- quantile( dgdr, bottom.p, na.rm=T )
  dgdr.w    <- dgdr
  dgdr.w[ dgdr.w > top    ] <- top
  dgdr.w[ dgdr.w < bottom ] <- bottom
  
  dgdr.n <- scale( dgdr.w )
  
  dgdr.p <- dplyr::ntile( dgdr, 100 )
  
  DGDR <- data.frame( dgdr, dgdr.w, dgdr.n, dgdr.p )
  
  print( summary( DGDR ) )
  
  par( mfrow=c(2,2) )
  plot( density( dgdr,   na.rm=T ), main="Donation/Grant Dependence Ratio (DGDR)" )
  plot( density( dgdr.w, na.rm=T ), main="DGDR Winsorized" )
  plot( density( dgdr.n, na.rm=T ), main="DGDR Standardized as Z" )
  plot( density( dgdr.p, na.rm=T ), main="DGDR as Percentile" )
  
  df.dgdr <- data.frame( cbind( df, DGDR ) )
  return( df.dgdr )
}
