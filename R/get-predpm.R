###---------------------------------------------------
###   PRE-DEPRECIATION PROFITABILITY MARGIN
###---------------------------------------------------

#' @title
#' Pre-Depreciation Profitability Margin
#'
#' @description
#' Calculate the pre-depreciation profitability margin and append it to the dataframe. 
#'
#' @param df A \code{data.frame} containing the required field for computing the metric. The metric will be appended to this dataset.
#' @param expenses A character string indicating the column name for total functional expenses (On 990: Part IX, line 25A; On EZ: Not Available).
#' @param depreciation A character string indicating the column name for depreciation expenses (On 990: Part IX, line 22A; On EZ:Not available).
#' @param revenue A character string indicating the column name for total revenue (On 990: Part VIII, line 12A; On EZ: Part I, line 9).
#' @param numerator A character string indicating the user-supplied column name for a pre-aggregated variable for the numerator. Do not combine with numerator column component arguments (`revenue`, `expenses`,`depreciation`).
#' @param denominator A character string indicating the user-supplied column name for a pre-aggregated variable for the denominator. Do not combine with denominator column component arguments (`revenue`). 
#' @param winsorize The winsorization value (between 0 and 1), defaults to 0.98 which winsorizes at 99th and 1st percentile values.   
#' 
#' @usage get_predpm( df, 
#' expenses = "F9_09_EXP_TOT_TOT",
#' revenue = "F9_08_REV_TOT_TOT", 
#' depreciation = "F9_09_EXP_DEPREC_TOT", 
#' numerator = NULL,
#' denominator = NULL,
#' winsorize = 0.98 )
#' 
#' @return Object of class \code{data.frame}: the original dataframe appended with the pre-depreciation profitability margin (`predpm`), 
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
#' d <- get_predpm( df = dat, expenses = "x1", depreciation = "x2", revenue = "x3" )
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
#' colnames( dat_01 ) <- c( "F9_09_EXP_TOT_TOT", "F9_09_EXP_DEPREC_TOT", "F9_08_REV_TOT_TOT" )
#' 
#' d <- get_predpm( dat_01 )
#' 
#' # coerce one column to factor
#' dat_01$F9_09_EXP_TOT_TOT <- as.factor( dat_01$F9_09_EXP_TOT_TOT )
#' 
#' d <- get_predpm( dat_01 )
#' 
#' # winsorize at 0.025 and 0.975 percentiles instead of 0.01 and 0.99
#' d <- get_predpm( df = dat, expenses = "x1", depreciation = "x2", revenue = "x3", winsorize = 0.95 )
#' 
#' d <- get_predpm( dat_01, winsorize = 0.95 )
#' 
#' 
#' # using 990 data
#' data( part010810 )
#' d <- get_predpm( df = part010810 )
#' 
#' # now coerce one of the variables to numeric
#' part010810$F9_09_EXP_TOT_TOT <- as.character( part010810$F9_09_EXP_TOT_TOT )
#' 
#' d <- get_predpm( df = part010810 )
#' 
#' \dontrun{
#' ## Errors ##
#' 
#' # numerator not specified
#' d <- get_predpm( df = dat, expenses = NULL, depreciation = NULL, revenue = "x3" )
#' 
#' # denominator not specified
#' d <- get_predpm( df = dat, expenses = "x1", depreciation = "x2", revenue = NULL )
#' 
#' # neither numerator nor denominator specified
#' d <- get_predpm( df = dat, expenses = NULL, depreciation = NULL, revenue = NULL )
#' 
#' # column names vector not of correct length
#' d <- get_predpm( df = dat, expenses = c("e","b","c"), depreciation = "x2", revenue = "e")
#' 
#' # column names vector not of correct length
#' d <- get_predpm( df = dat, expenses = "e", revenue = c( "e", "b", "c"), depreciation = "x2" )
#' }
#' @export
get_predpm <- function( df, 
                        expenses = "F9_09_EXP_TOT_TOT",
                        revenue = "F9_08_REV_TOT_TOT", 
                        depreciation = "F9_09_EXP_DEPREC_TOT", 
                        numerator = NULL,
                        denominator = NULL,
                        winsorize = 0.98 )
{
  
  # checks
  if( winsorize > 1 | winsorize < 0 )
  { stop( "winsorize argument must be 0 < w < 1" ) }
  
  if ( is.null( expenses )==F & is.null( depreciation )==F & is.null( revenue )==F ) {
    
    if ( expenses == "F9_09_EXP_TOT_TOT" & depreciation == "F9_09_EXP_DEPREC_TOT" &
         revenue == "F9_08_REV_TOT_TOT" & is.null( numerator )==F & is.null( denominator )==F ){
      
      warning( "Default argument inputs overridden with specified numerator and denominator arguments" ) 
      
      expenses <- NULL
      depreciation <- NULL
      revenue <- NULL
      
    }
    
  } 
  
  if ( ( ( length( c( expenses, depreciation ) ) < 2 )==F | is.null( numerator )==F ) &
       ( ( is.null( revenue )==T ) &
         is.null( denominator )==T ) )
  { stop( "The denominator has been incorrectly specified. Ensure you are passing the correct data field to the correct argument." ) }
  
  if ( ( ( length( c( revenue ) ) == 0 )==F | is.null( denominator )==F ) &
       ( ( is.null( expenses )==T | is.null( depreciation )==T ) &
         is.null( numerator )==T ) )
  { stop( "The numerator has been incorrectly specified. Ensure you are passing the correct data field to the correct argument." ) }
  
  
  if ( ( length( c( expenses, depreciation ) ) <= 2 ) &
       ( length( c( expenses, depreciation ) ) >= 1 ) & 
       ( is.null( numerator )==F ) )
  { stop( "The numerator has been incorrectly specified with conflicting arguments. Ensure you are passing the correct data field to the correct argument." ) }
  
  if ( ( length( c( revenue ) ) >= 1 ) & 
       ( is.null( denominator )==F ) )
  { stop( "The denominator has been incorrectly specified with conflicting arguments. Ensure you are passing the correct data field to the correct argument." ) }
  
  if( ( length( expenses ) > 1 | length( expenses ) < 1 ) & is.null( numerator ) == T ) 
  { stop( "`expenses` must be a single quoted string or a vector with a maximum length of one." ) }
  
  if( ( length( depreciation ) > 1 | length( depreciation ) < 1 ) & is.null( numerator ) == T ) 
  { stop( "`depreciation` must be a single quoted string or a vector with a maximum length of one." ) }
  
  if( ( length( revenue ) > 1 | length( revenue ) < 1 ) & is.null( denominator ) == T ) 
  { stop( "`revenue` must be a single quoted string or a vector with a maximum length of one." ) }
  
  if( ( length( numerator ) > 1 & length( c( expenses, depreciation ) ) == 0 ) )
  { stop( "`numerator` must be a single quoted string or a vector with a maximum length of one." ) }
  
  if( ( length( denominator ) > 1 & length( c( revenue ) ) == 0 ) )
  { stop( "`denominator` must be a single quoted string or a vector with a maximum length of one." ) }
  
  
  
  # copy data
  dat <- df
  
  ## ensure variable classes are numeric ##
  
  # run coerce_numeric and loop through all variables required and that matched by the two input arguments
  v <- c( colnames( dat )[which( colnames( dat ) %in% expenses ) ], colnames( dat )[which( colnames( dat ) %in% depreciation )],
          colnames( dat )[which( colnames( dat ) %in% revenue )],colnames( dat )[which( colnames( dat ) %in% numerator )],
          colnames( dat )[which( colnames( dat ) %in% denominator )] )
  
  dat <- coerce_numeric( d = dat, vars = v )
  
  if( is.null( numerator) == T & is.null( denominator ) == T ){
    
    num <- dat[[ revenue ]] - ( dat[[ expenses ]] - dat[[ depreciation ]] )
    den <- dat[[ revenue ]]
    
  }
  
  if( is.null( numerator) == F & is.null( denominator ) == T ){
    
    num <- dat[[ numerator ]]
    den <- dat[[ revenue ]]
    
  }
  
  if( is.null( numerator) == T & is.null( denominator ) == F ){
    
    num <- dat[[ revenue ]] - ( dat[[ expenses ]] - dat[[ depreciation ]] )
    den <- dat[[ denominator ]]
    
  }
  
  if( is.null( numerator) == F & is.null( denominator ) == F ){
    
    num <- dat[[ numerator ]]
    den <- dat[[ denominator ]]
    
  }
  
  # can't divide by zero
  print( paste0( "Revenue cannot be zero: ", sum( den==0, na.rm = T ), " cases have been replaced with NA." ) )
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
  plot( density( predpm,   na.rm=T ), main="Pre-depreciation Profitability Margin (PREDPM)" )
  plot( density( predpm.w, na.rm=T ), main="PREDPM Winsorized" )
  plot( density( predpm.n, na.rm=T ), main="PREDPM Standardized as Z" )
  plot( density( predpm.p, na.rm=T ), main="PREDPM as Percentile" )
  
  df.predpm <- data.frame( cbind( df, PREDPM ) )
  return( df.predpm )
}
  