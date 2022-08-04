###---------------------------------------------------
###   SHORT TERM DEBT RATIO
###---------------------------------------------------

#' @title
#' Short Term Debt Ratio
#'
#' @description
#' Calculate the short term debt ratio and append it to the dataframe. 
#'
#' @param df A \code{data.frame} containing the required field for computing the metric. The metric will be appended to this dataset.
#' @param accounts.payable A character string indicating the column name for accounts payable, EOY (On 990: Part X, line 17B; On EZ: Not Available).
#' @param grants.payable A character string indicating the column name for grants payable, EOY (On 990: Part X, line 18B; On EZ: Not Available).
#' @param net.assets A character string indicating the column name for net assets, EOY (On 990: Part X, Line 33B; On EZ: Part I, Line 21).
#' 
#' @usage get_stdr( df, accounts.payable = "F9_10_LIAB_ACC_PAYABLE_EOY", grants.payable = "F9_10_LIAB_GRANT_PAYABLE_EOY", net.assets = "F9_10_NAFB_TOT_EOY", numerator = NULL, denominator = NULL, winsorize = 0.98 )
#' 
#' @return Object of class \code{data.frame}: the original dataframe appended with the short term debt ratio (`stdr`), 
#'  a winsorized version (`stdr.w`), a standardized z-score version (`stdr.z`), 
#'  and a percentile version (`stdr.p`).   
#'
#' @details This metric indicates how well an organization can cover its immediate liabilities with its equity. 
#' Low values are better as they indicate that an organization has lower levels of leverage and more flexibility 
#' to dispense its assets as needed or as opportunities arise. When an organization has a Short Term Debt Ratio 
#' of 1, its liabilities are fully equal to the assets it owns. If an organization has a ratio higher than 1, it 
#' indicates that it is over leveraged. A value close to zero indicates low levels of liability, and a negative 
#' value indicates that an organization has overpaid on their liabilities. Note: computation of this metric is available 
#' to only 990 filers and not for 990-EZ filers. The default inputs use column names for variables available only to 990 
#' filers.
#' 
#' @examples
#' x1 <- rnorm( 1000, 100, 30 )
#' x2 <- rnorm( 1000, 200, 30 )
#' x3 <- rnorm( 1000, 200, 30 )
#' x3[ c( 15, 300, 600 ) ] <- 0
#' 
#' dat <- data.frame( x1, x2, x3 )
#' 
#' # specify own column names
#' d <- get_stdr( df = dat, accounts.payable = "x1", grants.payable = "x2", net.assets = "x3" )
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
#' colnames( dat_01 ) <- c( "F9_10_LIAB_ACC_PAYABLE_EOY", "F9_10_LIAB_GRANT_PAYABLE_EOY", "F9_10_NAFB_TOT_EOY" )
#' 
#' d <- get_stdr( dat_01 )
#' 
#' # coerce one column to factor
#' dat_01$F9_10_LIAB_GRANT_PAYABLE_EOY <- as.factor( dat_01$F9_10_LIAB_GRANT_PAYABLE_EOY )
#' 
#' d <- get_stdr( dat_01 )
#' 
#' # winsorize at 0.025 and 0.975 percentiles instead of 0.01 and 0.99
#' d <- get_stdr( df = dat, accounts.payable = "x1", grants.payable = "x2", net.assets = "x3", winsorize = 0.95 )
#' 
#' d <- get_stdr( dat_01, winsorize = 0.95 )
#' 
#' 
#' # using 990 data
#' load( '/Volumes/My Passport for Mac/Urban Institute/Summer Projects/Fiscal/fiscal/R/sysdata.rda' )
#' d <- get_stdr( df = part010810 )
#' 
#' # now coerce one of the variables to numeric
#' part010810$F9_09_EXP_TOT_TOT <- as.character( part010810$F9_09_EXP_TOT_TOT )
#' 
#' d <- get_stdr( df = part010810 )
#' 
#' 
#' ## Errors ##
#' 
#' # numerator not specified
#' d <- get_stdr( df = dat, accounts.payable = NULL, grants.payable = NULL, net.assets = "x3" )
#' 
#' # denominator not specified
#' d <- get_stdr( df = dat, accounts.payable = "x1", grants.payable = "x2", net.assets = NULL )
#' 
#' # neither numerator nor denominator specified
#' d <- get_stdr( df = dat, accounts.payable = NULL, grants.payable = NULL, net.assets = NULL )
#' 
#' # column names vector not of correct length
#' d <- get_stdr( df = dat, accounts.payable = c("e","b","c"), grants.payable = "x2", net.assets = "e")
#' 
#' # column names vector not of correct length
#' d <- get_stdr( df = dat, accounts.payable = "e", net.assets = c( "e", "b", "c"), grants.payable = "x2" )
#' @export
get_stdr <- function( df,
                      accounts.payable = "F9_10_LIAB_ACC_PAYABLE_EOY",
                      grants.payable = "F9_10_LIAB_GRANT_PAYABLE_EOY",
                      net.assets = "F9_10_NAFB_TOT_EOY",
                      numerator = NULL,
                      denominator = NULL,
                      winsorize = 0.98 )
{
  
  
  
  # checks
  if( winsorize > 1 | winsorize < 0 )
  { stop( "winsorize argument must be 0 < w < 1" ) }
  
  if ( is.null( accounts.payable )==F & is.null( grants.payable )==F & is.null( net.assets )==F ) {
    
    if ( accounts.payable == "F9_10_LIAB_ACC_PAYABLE_EOY" & grants.payable == "F9_10_LIAB_GRANT_PAYABLE_EOY" &
         net.assets == "F9_10_NAFB_TOT_EOY" & is.null( numerator )==F & is.null( denominator )==F ){
      
      warning( "Default argument inputs overridden with specified numerator and denominator arguments" ) 
      
      accounts.payable <- NULL
      grants.payable <- NULL
      net.assets <- NULL
      
    }
    
  } 
  
  if ( ( ( length( c( accounts.payable, grants.payable ) ) < 2 )==F | is.null( numerator )==F ) &
       ( ( is.null( net.assets )==T ) &
         is.null( denominator )==T ) )
  { stop( "The denominator has been incorrectly specified. Ensure you are passing the correct data field to the correct argument." ) }
  
  if ( ( ( length( c( net.assets ) ) == 0 )==F | is.null( denominator )==F ) &
       ( ( is.null( accounts.payable )==T | is.null( grants.payable )==T ) &
         is.null( numerator )==T ) )
  { stop( "The numerator has been incorrectly specified. Ensure you are passing the correct data field to the correct argument." ) }
  
  
  if ( ( length( c( accounts.payable, grants.payable ) ) <= 2 ) &
       ( length( c( accounts.payable, grants.payable ) ) >= 1 ) & 
       ( is.null( numerator )==F ) )
  { stop( "The numerator has been incorrectly specified with conflicting arguments. Ensure you are passing the correct data field to the correct argument." ) }
  
  if ( ( length( c( net.assets ) ) >= 1 ) & 
       ( is.null( denominator )==F ) )
  { stop( "The denominator has been incorrectly specified with conflicting arguments. Ensure you are passing the correct data field to the correct argument." ) }
  
  if( ( length( accounts.payable ) > 1 | length( accounts.payable ) < 1 ) & is.null( numerator ) == T ) 
  { stop( "`accounts.payable` must be a single quoted string or a vector with a maximum length of one." ) }
  
  if( ( length( grants.payable ) > 1 | length( grants.payable ) < 1 ) & is.null( numerator ) == T ) 
  { stop( "`grants.payable` must be a single quoted string or a vector with a maximum length of one." ) }
  
  if( ( length( net.assets ) > 1 | length( net.assets ) < 1 ) & is.null( denominator ) == T ) 
  { stop( "`net.assets` must be a single quoted string or a vector with a maximum length of one." ) }
  
  if( ( length( numerator ) > 1 & length( c( accounts.payable, grants.payable ) ) == 0 ) )
  { stop( "`numerator` must be a single quoted string or a vector with a maximum length of one." ) }
  
  if( ( length( denominator ) > 1 & length( c( net.assets ) ) == 0 ) )
  { stop( "`denominator` must be a single quoted string or a vector with a maximum length of one." ) }
  
  
  
  # copy data
  dat <- df
  
  ## ensure variable classes are numeric ##
  
  # run coerce_numeric and loop through all variables required and that matched by the two input arguments
  v <- c( colnames( dat )[which( colnames( dat ) %in% accounts.payable ) ], colnames( dat )[which( colnames( dat ) %in% grants.payable )],
          colnames( dat )[which( colnames( dat ) %in% net.assets )],colnames( dat )[which( colnames( dat ) %in% numerator )],
          colnames( dat )[which( colnames( dat ) %in% denominator )] )
  
  dat <- coerce_numeric( d = dat, vars = v )
  
  if( is.null( numerator) == T & is.null( denominator ) == T ){
    
    num <- dat[[ accounts.payable ]] + dat[[ grants.payable ]]
    den <- dat[[ net.assets ]]
    
  }
  
  if( is.null( numerator) == F & is.null( denominator ) == T ){
    
    num <- dat[[ numerator ]]
    den <- dat[[ net.assets ]]
    
  }
  
  if( is.null( numerator) == T & is.null( denominator ) == F ){
    
    num <- dat[[ accounts.payable ]] + dat[[ grants.payable ]]
    den <- dat[[ denominator ]]
    
  }
  
  if( is.null( numerator) == F & is.null( denominator ) == F ){
    
    num <- dat[[ numerator ]]
    den <- dat[[ denominator ]]
    
  }
  
  # can't divide by zero
  print( paste0( "Net assets cannot be zero: ", sum( den==0, na.rm = T ), " cases have been replaced with NA." ) )
  den[ den == 0 ] <- NA 
  
  stdr <- num / den
  
  top.p    <- 1 - (1-winsorize)/2
  bottom.p <- 0 + (1-winsorize)/2
  top      <- quantile( stdr, top.p, na.rm=T )
  bottom   <- quantile( stdr, bottom.p, na.rm=T )
  stdr.w    <- stdr
  stdr.w[ stdr.w > top    ] <- top
  stdr.w[ stdr.w < bottom ] <- bottom
  
  stdr.n <- scale( stdr.w )
  
  stdr.p <- dplyr::ntile( stdr, 100 )
  
  STDR <- data.frame( stdr, stdr.w, stdr.n, stdr.p )
  
  print( summary( STDR ) )
  
  par( mfrow=c(2,2) )
  plot( density( stdr,   na.rm=T ), main="Short-Term Debt Ratio (STDR)" )
  plot( density( stdr.w, na.rm=T ), main="STDR Winsorized" )
  plot( density( stdr.n, na.rm=T ), main="STDR Standardized as Z" )
  plot( density( stdr.p, na.rm=T ), main="STDR as Percentile" )
  
  df.stdr <- data.frame( cbind( df, STDR ) )
  return( df.stdr )
}
