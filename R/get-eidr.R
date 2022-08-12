###---------------------------------------------------
###   EARNED INCOME DEPENDENCE RATIO
###---------------------------------------------------

#' @title
#' Earned Income Dependence Ratio
#'
#' @description
#' Calculate the earned income dependence ratio and append it to the dataframe. 
#'
#' @param df A \code{data.frame} containing the required field for computing the metric. The metric will be appended to this dataset.
#' @param prog.service.rev A character string indicating the column name for program service revenue (On 990: Part VIII, Line 2g(A); On EZ: Not Available).
#' @param memb.dues A character string indicating the column name for membership dues (On 990: Part VIII, Line 1b(A); On EZ: Not Available).
#' @param royalties A character string indicating the column name for royalties (On 990: Part VIII, Line 5(A); On EZ: Not Available).
#' @param other.revenue A character string indicating the column name for all other revenue (On 990: Part VIII, Line 11d(A); On EZ: Not Available). 
#' @param total.revenue A character string indicating the column name for total revenue (On 990: (Part VIII, Line 12A); On EZ: Part I, Line 9).
#' @param numerator A character string indicating the user-supplied column name for a pre-aggregated variable for the numerator. Do not combine with numerator column component arguments (`prog.service.rev`, `memb.dues`,`royalties`, `other.revenue`).
#' @param denominator A character string indicating the user-supplied column name for a pre-aggregated variable for the denominator. Do not combine with denominator column component arguments (`total.revenue`). 
#' @param winsorize The winsorization value (between 0 and 1), defaults to 0.98 which winsorizes at 99th and 1st percentile values.   
#' 
#' @usage get_eidr( df, 
#' prog.service.rev = "F9_08_REV_PROG_TOT_TOT", 
#' memb.dues = "F9_08_REV_CONTR_MEMBSHIP_DUE", 
#' royalties = "F9_08_REV_OTH_ROY_TOT", 
#' other.revenue = "F9_08_REV_MISC_OTH_TOT", 
#' total.revenue = "F9_08_REV_TOT_TOT", 
#' numerator = NULL,
#' denominator = NULL,
#' winsorize = 0.98 )
#' 
#' @return Object of class \code{data.frame}: the original dataframe appended with the earned income dependence ratio (`eidr`), 
#'  a winsorized vermemb.dueson (`eidr.w`), a standardized z-score vermemb.dueson (`eidr.z`), 
#'  and a percentile vermemb.dueson (`eidr.p`).   
#'
#' @details This metric shows how much of an organization’s total revenues come from earned income (program 
#' service revenues, dues, assessments, profits from sales). High levels in this indicator are more dememb.duesrable 
#' memb.duesnce that means that an organization is fairly self-sustaining with its own activities. Low values in this 
#' indicator mean an organization is likely dependent on donations or investments for their revenues and thus more 
#' vulnerable to the sentiments of donors or market forces. Note: computation of this metric is available to only 
#' 990 filers and not for 990-EZ filers. The default inputs use column names for variables available only to 990 filers.
#' 
#' @import dplyr
#' @import stringr
#' @import magrittr
#' 
#' @examples
#' library( fiscal )
#' x1 <- rnorm( 1000,100,30 )
#' x2 <- rnorm( 1000,200,30 )
#' x3 <- rnorm( 1000,200,30 )
#' x4 <- rnorm( 1000,100,30 )
#' x5 <- rnorm( 1000,200,30 )
#' dat <- data.frame( x1, x2, x3, x4, x5 )
#' 
#' # specify own column names
#' d <- get_eidr( df=dat, prog.service.rev='x1', memb.dues='x2', royalties='x3', other.revenue='x4', total.revenue='x5', winsorize=0.98 )
#' 
#' head( d )
#' 
#' # run with default column names
#' dat_01 <- dat
#' 
#' colnames( dat_01 ) <- c( "F9_08_REV_PROG_TOT_TOT", "F9_08_REV_CONTR_MEMBSHIP_DUE", "F9_08_REV_OTH_ROY_TOT",
#'                          "F9_08_REV_MISC_OTH_TOT", "F9_08_REV_TOT_TOT" )
#' 
#' 
#' d <- get_eidr( dat_01 )
#' 
#' # coerce one column to factor
#' dat_01$F9_08_REV_MISC_OTH_TOT <- as.factor( dat_01$F9_08_REV_MISC_OTH_TOT )
#' 
#' d <- get_eidr( dat_01 )
#' 
#' # winsorize at 0.025 and 0.975 percentiles instead of 0.01 and 0.99
#' d <- get_eidr( df=dat, prog.service.rev='x1', memb.dues='x2', royalties='x3', other.revenue='x4', total.revenue='x5', winsorize = 0.95 )
#' 
#' d <- get_eidr( dat_01, winsorize = 0.95 )
#' 
#' # aggregate variables into single numerator and denominator
#' x.den <- x5 
#' x.num <- x1 + x2 + x3 + x4
#' 
#' dat_02 <- cbind( dat, x.den, x.num)
#' 
#' d <- get_eidr( dat_02, numerator = "x.num", denominator = "x.den" )
#' 
#' # using 990 data
#' data( part010810 )
#' d <- get_eidr( df = part010810 )
#' 
#' # now coerce one of the variables to numeric
#' part010810$F9_10_LIAB_ACC_PAYABLE_EOY <- as.character( part010810$F9_10_LIAB_ACC_PAYABLE_EOY )
#' 
#' d <- get_eidr( df = part010810 )
#' 
#' \dontrun{
#' ## Errors ##
#' # incorrectly specify denominator
#' get_eidr( df = dat_02, prog.service.rev = 'x1', memb.dues = 'x2', royalties = 'x3', other.revenue = 'x4', 
#'           total.revenue = NULL, numerator = NULL, denominator = NULL, winsorize=0.98 )
#' 
#' # incorrectly specify numerator with conflicting arguments
#' get_eidr( df = dat_02, prog.service.rev = 'x1', memb.dues = 'x2', royalties = 'x3', other.revenue = 'x4', 
#'           total.revenue = NULL, numerator = 'x.num', denominator = 'x.den' , winsorize=0.98 )
#' 
#' 
#' # incorrectly specify denominator with conflicting arguments
#' get_eidr( df = dat_02, prog.service.rev = 'x1', memb.dues = 'x2', royalties = 'x3', other.revenue = 'x4', 
#'           total.revenue = 'x5', numerator = NULL, denominator = 'x.den' , winsorize=0.98 )  
#' 
#' # supplying no arguments for the numerator
#' get_eidr( df = dat_02, prog.service.rev = NULL, memb.dues = NULL, royalties = NULL, other.revenue = NULL, 
#'           total.revenue = 'x5', numerator = NULL, denominator = NULL , winsorize=0.98 )  
#' 
#' # supplying no arguments for the denominator
#' get_eidr( df = dat_02, prog.service.rev = 'x1', memb.dues = 'x2', royalties = 'x3', other.revenue = 'x4', 
#'           total.revenue = NULL, numerator = NULL, denominator = NULL , winsorize=0.98 )  
#' 
#' # supplying argument for one of the parameters in the numerator that is greater than length 1
#' get_eidr( df = dat_02, prog.service.rev = 'x1', memb.dues = 'x2', royalties = 'x3', other.revenue = 'x4', 
#'           total.revenue = c( 'x5', 'x6' ), numerator = NULL, denominator = NULL, winsorize=0.98 )
#' 
#' get_eidr( df = dat_02, prog.service.rev = NULL, memb.dues = NULL, royalties = NULL, other.revenue = NULL, 
#'           total.revenue = NULL, numerator = c( 'x5', 'x6' ), denominator = 'x.den' , winsorize=0.98 )  
#' }
#' 
#' @export
get_eidr <- function( df, 
                      prog.service.rev = "F9_08_REV_PROG_TOT_TOT", 
                      memb.dues = "F9_08_REV_CONTR_MEMBSHIP_DUE", 
                      royalties = "F9_08_REV_OTH_ROY_TOT", 
                      other.revenue = "F9_08_REV_MISC_OTH_TOT", 
                      total.revenue = "F9_08_REV_TOT_TOT", 
                      numerator = NULL,
                      denominator = NULL,
                      winsorize = 0.98 )
{
  
  # checks
  if( winsorize > 1 | winsorize < 0 )
  { stop( "winsorize argument must be 0 < w < 1" ) }
  
  if( is.null( prog.service.rev )==F & is.null( memb.dues )==F & is.null( royalties )==F & is.null( other.revenue )==F &
      is.null( total.revenue )==F ) {
    
  if( prog.service.rev == "F9_08_REV_PROG_TOT_TOT" & memb.dues == "F9_08_REV_CONTR_MEMBSHIP_DUE" &
      royalties == "F9_08_REV_OTH_ROY_TOT" & other.revenue == "F9_08_REV_MISC_OTH_TOT" & total.revenue == "F9_08_REV_TOT_TOT" &
      is.null( numerator )==F & is.null( denominator )==F ){
    warning( "Default argument inputs overridden with specified numerator and denominator arguments" ) 
    
    prog.service.rev <- NULL
    memb.dues <- NULL
    royalties <- NULL
    other.revenue <- NULL
    total.revenue <- NULL
    
  }
  
  } 
  
  if ( ( ( length( c( prog.service.rev, memb.dues, royalties, other.revenue ) ) < 4 )==F | is.null( numerator )==F ) &
       ( ( is.null( total.revenue )==T ) &
         is.null( denominator )==T ) )
  { stop( "The denominator has been incorrectly specified. Ensure you are passing the correct data field to the correct argument." ) }
  
  if ( ( ( length( c( total.revenue ) ) < 1 )==F | is.null( denominator )==F ) &
       ( ( is.null( prog.service.rev )==T | is.null( memb.dues )==T | 
           is.null( royalties )==T | is.null( other.revenue )==T ) &
         is.null( numerator )==T ) )
  { stop( "The numerator has been incorrectly specified. Ensure you are passing the correct data field to the correct argument." ) }
  
  
  if ( ( length( c( prog.service.rev, memb.dues, royalties, other.revenue ) ) <= 4 ) &
       ( length( c( prog.service.rev, memb.dues, royalties, other.revenue ) ) >= 1 ) & 
       ( is.null( numerator )==F ) )
  { stop( "The numerator has been incorrectly specified with conflicting arguments. Ensure you are passing the correct data field to the correct argument." ) }
  
  if ( ( length( c( total.revenue ) ) >= 1 ) & 
       ( is.null( denominator )==F ) )
  { stop( "The denominator has been incorrectly specified with conflicting arguments. Ensure you are passing the correct data field to the correct argument." ) }
  
  if( ( length( prog.service.rev ) > 1 | length( prog.service.rev ) < 1 ) & is.null( numerator ) == T ) 
  { stop( "`prog.service.rev` must be a single quoted string or a vector with a maximum length of one." ) }
  
  if( ( length( memb.dues ) > 1 | length( memb.dues ) < 1 ) & is.null( numerator ) == T ) 
  { stop( "`memb.dues` must be a single quoted string or a vector with a maximum length of one." ) }
  
  if( ( length( royalties ) > 1 | length( royalties ) < 1 ) & is.null( numerator ) == T ) 
  { stop( "`royalties` must be a single quoted string or a vector with a maximum length of one." ) }
  
  if( ( length( other.revenue ) > 1 | length( other.revenue ) < 1 ) & is.null( numerator ) == T ) 
  { stop( "`other.revenue` must be a single quoted string or a vector with a maximum length of one." ) }
  
  if( ( length( total.revenue ) > 1 | length( total.revenue ) < 1 ) & is.null( denominator ) == T ) 
  { stop( "`total.revenue` must be a single quoted string or a vector with a maximum length of one." ) }
  
  if( ( length( numerator ) > 1 & length( c( prog.service.rev, memb.dues, royalties, other.revenue ) ) == 0 ) )
  { stop( "`numerator` must be a single quoted string or a vector with a maximum length of one." ) }
  
  if( ( length( denominator ) > 1 & length( c( total.revenue ) ) == 0 ) )
  { stop( "`denominator` must be a single quoted string or a vector with a maximum length of one." ) }
  
  
  
  # copy data
  dat <- df
  
  ## ensure variable classes are numeric ##
  
  # run coerce_numeric and loop through all variables required and that matched by the two input arguments
  v <- c( colnames( dat )[which( colnames( dat ) %in% prog.service.rev ) ], colnames( dat )[which( colnames( dat ) %in% memb.dues )],
          colnames( dat )[which( colnames( dat ) %in% other.revenue )], colnames( dat )[which( colnames( dat ) %in% royalties )], 
          colnames( dat )[which( colnames( dat ) %in% total.revenue )], colnames( dat )[which( colnames( dat ) %in% numerator )],
          colnames( dat )[which( colnames( dat ) %in% denominator )] )
  
  dat <- coerce_numeric( d = dat, vars = v )
  
  if( is.null( numerator) == T & is.null( denominator ) == T ){
    
  num <- dat[[ prog.service.rev ]] + dat[[ memb.dues ]] + dat[[ royalties ]] + dat[[ other.revenue ]]
  den <- dat[[ total.revenue ]] 
  
  }
  
  if( is.null( numerator) == F & is.null( denominator ) == T ){
    
    num <- dat[[ numerator ]]
    den <- dat[[ total.revenue ]] 
    
  }
  
  if( is.null( numerator) == T & is.null( denominator ) == F ){
    
    num <- dat[[ prog.service.rev ]] + dat[[ memb.dues ]] + dat[[ royalties ]] + dat[[ other.revenue ]]
    den <- dat[[ denominator ]] 
    
  }
  
  if( is.null( numerator) == F & is.null( denominator ) == F ){
    
    num <- dat[[ numerator ]]
    den <- dat[[ denominator ]] 
    
  }
  
  # can't divide by zero
  print( paste0( "Total revenue cannot be zero: ", sum( den==0, na.rm = T ), " cases have been replaced with NA." ) )
  den[ den == 0 ] <- NA 
  
  eidr <- num / den
  
  top.p    <- 1 - (1-winsorize)/2
  bottom.p <- 0 + (1-winsorize)/2
  top      <- quantile( eidr, top.p, na.rm=T )
  bottom   <- quantile( eidr, bottom.p, na.rm=T )
  eidr.w    <- eidr
  eidr.w[ eidr.w > top    ] <- top
  eidr.w[ eidr.w < bottom ] <- bottom
  
  eidr.n <- scale( eidr.w )
  
  eidr.p <- dplyr::ntile( eidr, 100 )
  
  EIDR <- data.frame( eidr, eidr.w, eidr.n, eidr.p )
  
  print( summary( EIDR ) )
  
  par( mfrow=c(2,2) )
  plot( density( eidr,   na.rm=T ), main="Earned Income Dependence Ratio (EIDR)" )
  plot( density( eidr.w, na.rm=T ), main="EIDR Winsorized" )
  plot( density( eidr.n, na.rm=T ), main="EIDR Standardized as Z" )
  plot( density( eidr.p, na.rm=T ), main="EIDR as Percentile" )
  
  df.eidr <- data.frame( cbind( df, EIDR ) )
  return( df.eidr )
}
