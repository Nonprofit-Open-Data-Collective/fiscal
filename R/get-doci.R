###---------------------------------------------------
###   DAYS OF OPERATING CASH AND INVESTMENTS
###---------------------------------------------------

#' @title
#' Days of Operating Cash and Investments
#'
#' @description
#' Calculate the days of operating cash and investments and append it to the dataframe. 
#'
#' @param df A \code{data.frame} containing the required field for computing the metric. The metric will be appended to this dataset.
#' @param net.assets A character string indicating the column name for unrestricted net assets, EOY (On 990: Part X, line 27B; On EZ: Not Available).
#' @param invest A character string indicating the column name for investment income (On 990: Part X, Line 8(B); On EZ: Not Available).
#' @param lbe A character string indicating the column name for lands, buildings, and equipment (On 990: Part X, Line 10(c)(B); On EZ: Not Available).
#' @param mnp A character string indicating the column name for mortgages and notes payables (On 990: Part X, Line 23(B); On EZ: Not Available).
#' @param tot.func.exp A character string indicating the column name for daily average expenses or total expenses (On 990: Part IX, Line 25(A); On EZ: Not Available).
#' @param dda Depreciation, depletion, and amortization (On 990: Part IX, line 22A; On EZ: Not Available).
#'
#' @usage get_doci( df, 
#' net.assets = "F9_10_NAFB_UNRESTRICT_EOY", 
#' invest = "F9_10_ASSET_INV_SALE_EOY", 
#' lbe = "F9_10_ASSET_LAND_BLDG_NET_EOY", 
#' mnp = "F9_10_LIAB_MTG_NOTE_EOY", 
#' tot.func.exp = "F9_09_EXP_TOT_TOT", 
#' dda = "F9_09_EXP_DEPREC_TOT", numerator = NULL, denominator = NULL, winsorize = 0.98 )
#' 
#' @return Object of class \code{data.frame}: the original dataframe appended with the days of operating cash and investments (`doci`), 
#'  a winsorized version (`doci.w`), a standardized z-score version (`doci.z`), 
#'  and a percentile version (`doci.p`).   
#'
#' @details The days of operating cash and investment measures how long an organization would be able to 
#' continue operating its programs and investing at its current rate based on its average per-day expenses 
#' (excluding depreciation). Note: This ratio is the same as month of cash on hand but it is in days and 
#' excludes land and mortgages from the operating cash and investment numerator. The reason that land and 
#' mortgage expenses are deducted from the numerator is because those are non-liquid or necessary expenses 
#' and shouldn't truly be considered free cash for operating and investment. The higher the number of days 
#' from this metric, the more protected the system will be against revenue shocks, but the target value is 
#' subjective. Generally, a system should aim to maintain several months’ worth of cash on hand and at the 
#' very least exceed the length of the billing period (usually 30 or 60 days). Note: computation of this metric 
#' is available to only 990 filers and not for 990-EZ filers. The default inputs use column names for variables 
#' available only to 990 filers.
#' 
#' @examples
#' x1 <- rnorm( 1000,100,30 )
#' x2 <- rnorm( 1000,200,30 )
#' x3 <- rnorm( 1000,200,30 )
#' x4 <- rnorm( 1000,100,30 )
#' x5 <- rnorm( 1000,200,30 )
#' x6 <- rnorm( 1000,200,30 )
#' dat <- data.frame( x1, x2, x3, x4, x5, x6 )
#' 
#' # specify own column names
#' d <- get_doci( df=dat, net.assets='x1', invest='x2', lbe='x3', mnp='x4', tot.func.exp='x5', dda = 'x6', winsorize=0.98 )
#' 
#' head( d )
#' 
#' # run with default column names
#' dat_01 <- dat
#' 
#' colnames( dat_01 ) <- c( "F9_10_NAFB_UNRESTRICT_EOY", "F9_10_ASSET_INV_SALE_EOY", "F9_10_ASSET_LAND_BLDG_NET_EOY",
#'                          "F9_10_LIAB_MTG_NOTE_EOY", "F9_09_EXP_TOT_TOT", "F9_09_EXP_DEPREC_TOT" )
#' 
#' 
#' d <- get_doci( dat_01 )
#' 
#' # coerce one column to factor
#' dat_01$F9_10_NAFB_UNRESTRICT_EOY <- as.factor( dat_01$F9_10_NAFB_UNRESTRICT_EOY )
#' 
#' d <- get_doci( dat_01 )
#' 
#' # winsorize at 0.025 and 0.975 percentiles instead of 0.01 and 0.99
#' d <- get_doci( df=dat, net.assets='x1', invest='x2', lbe='x3', mnp='x4', tot.func.exp='x5', dda = 'x6', winsorize = 0.95 )
#' 
#' d <- get_doci( dat_01, winsorize = 0.95 )
#' 
#' # aggregate variables into single numerator and denominator
#' x.den <- x5 + x6
#' x.num <- x1 + x2 + x3 + x4
#' 
#' dat_02 <- cbind( dat, x.den, x.num)
#' 
#' d <- get_doci( dat_02, numerator = "x.num", denominator = "x.den" )
#' 
#' # using 990 data
#' load( '/Volumes/My Passport for Mac/Urban Institute/Summer Projects/Fiscal/fiscal/R/sysdata.rda' )
#' d <- get_doci( df = part010810 )
#' 
#' # now coerce one of the variables to numeric
#' part010810$F9_10_NAFB_UNRESTRICT_EOY <- as.character( part010810$F9_10_NAFB_UNRESTRICT_EOY )
#' 
#' d <- get_doci( df = part010810 )
#' 
#' #errors 
#' 
#' # incorrectly specify denominator
#' get_doci( df = dat_02, net.assets = 'x1', invest = 'x2', lbe = 'x3', mnp = 'x4', 
#'           tot.func.exp = NULL, numerator = NULL, denominator = NULL, winsorize=0.98 )
#' 
#' # incorrectly specify numerator with conflicting arguments
#' get_doci( df = dat_02, net.assets = 'x1', invest = 'x2', lbe = 'x3', mnp = 'x4', 
#'           tot.func.exp = NULL, dda = NULL, numerator = 'x.num', denominator = 'x.den' , winsorize=0.98 )
#' 
#' 
#' # incorrectly specify denominator with conflicting arguments
#' get_doci( df = dat_02, net.assets = 'x1', invest = 'x2', lbe = 'x3', mnp = 'x4', 
#'           tot.func.exp = 'x5', dda = 'x6', numerator = NULL, denominator = 'x.den' , winsorize=0.98 )  
#' 
#' # supplying no arguments for the numerator
#' get_doci( df = dat_02, net.assets = NULL, invest = NULL, lbe = NULL, mnp = NULL, 
#'           tot.func.exp = 'x5', dda = 'x6', numerator = NULL, denominator = NULL , winsorize=0.98 )  
#' 
#' # supplying no arguments for the denominator
#' get_doci( df = dat_02, net.assets = 'x1', invest = 'x2', lbe = 'x3', mnp = 'x4', 
#'           tot.func.exp = NULL, dda = NULL, numerator = NULL, denominator = NULL , winsorize=0.98 )  
#' 
#' # supplying argument for one of the parameters in the numerator that is greater than length 1
#' get_doci( df = dat_02, net.assets = 'x1', invest = 'x2', lbe = 'x3', mnp = 'x4', 
#'           tot.func.exp = c( 'x5', 'x6' ), dda = 'x6', numerator = NULL, denominator = NULL, winsorize=0.98 )
#' 
#' get_doci( df = dat_02, net.assets = NULL, invest = NULL, lbe = NULL, mnp = NULL, 
#' @export
get_doci <- function( df, 
                      net.assets = "F9_10_NAFB_UNRESTRICT_EOY", 
                      invest = "F9_10_ASSET_INV_SALE_EOY", 
                      lbe = "F9_10_ASSET_LAND_BLDG_NET_EOY", 
                      mnp = "F9_10_LIAB_MTG_NOTE_EOY", 
                      tot.func.exp = "F9_09_EXP_TOT_TOT", 
                      dda = "F9_09_EXP_DEPREC_TOT", numerator = NULL, denominator = NULL, winsorize = 0.98 )
{
  
  
  
  # checks
  if( winsorize > 1 | winsorize < 0 )
  { stop( "winsorize argument must be 0 < w < 1" ) }
  
  if( is.null( net.assets )==F & is.null( invest )==F & is.null( lbe )==F & is.null( mnp )==F &
      is.null( tot.func.exp )==F & is.null( dda )==F ) {
    
    if( net.assets == "F9_10_NAFB_UNRESTRICT_EOY" & invest == "F9_10_ASSET_INV_SALE_EOY" &
        lbe == "F9_10_ASSET_LAND_BLDG_NET_EOY" & mnp == "F9_10_LIAB_MTG_NOTE_EOY" & tot.func.exp == "F9_09_EXP_TOT_TOT" &
        dda == "F9_09_EXP_DEPREC_TOT" & is.null( numerator )==F & is.null( denominator )==F ){
      
      warning( "Default argument inputs overridden with specified numerator and denominator arguments" ) 
      
      net.assets <- NULL
      invest <- NULL
      lbe <- NULL
      mnp <- NULL
      tot.func.exp <- NULL
      dda <- NULL
      
    }
    
  } 
  
  if ( ( ( length( c( net.assets, invest, lbe, mnp ) ) < 4 )==F | is.null( numerator )==F ) &
       ( ( is.null( tot.func.exp )==T & is.null( dda )==T ) &
         is.null( denominator )==T ) )
  { stop( "The denominator has been incorrectly specified. Ensure you are passing the correct data field to the correct argument." ) }
  
  if ( ( ( length( c( tot.func.exp, dda ) ) < 2 )==F | is.null( denominator )==F ) &
       ( ( is.null( net.assets )==T | is.null( invest )==T | 
           is.null( lbe )==T | is.null( mnp )==T ) &
         is.null( numerator )==T ) )
  { stop( "The numerator has been incorrectly specified. Ensure you are passing the correct data field to the correct argument." ) }
  
  
  if ( ( length( c( net.assets, invest, lbe, mnp ) ) <= 4 ) &
       ( length( c( net.assets, invest, lbe, mnp ) ) >= 1 ) & 
       ( is.null( numerator )==F ) )
  { stop( "The numerator has been incorrectly specified with conflicting arguments. Ensure you are passing the correct data field to the correct argument." ) }
  
  if ( ( length( c( tot.func.exp, dda ) ) >= 2 ) & 
       ( is.null( denominator )==F ) )
  { stop( "The denominator has been incorrectly specified with conflicting arguments. Ensure you are passing the correct data field to the correct argument." ) }
  
  if( ( length( net.assets ) > 1 | length( net.assets ) < 1 ) & is.null( numerator ) == T ) 
  { stop( "`net.assets` must be a single quoted string or a vector with a maximum length of one." ) }
  
  if( ( length( invest ) > 1 | length( invest ) < 1 ) & is.null( numerator ) == T ) 
  { stop( "`invest` must be a single quoted string or a vector with a maximum length of one." ) }
  
  if( ( length( lbe ) > 1 | length( lbe ) < 1 ) & is.null( numerator ) == T ) 
  { stop( "`lbe` must be a single quoted string or a vector with a maximum length of one." ) }
  
  if( ( length( mnp ) > 1 | length( mnp ) < 1 ) & is.null( numerator ) == T ) 
  { stop( "`mnp` must be a single quoted string or a vector with a maximum length of one." ) }
  
  if( ( length( tot.func.exp ) > 1 | length( tot.func.exp ) < 1 ) & is.null( denominator ) == T ) 
  { stop( "`tot.func.exp` must be a single quoted string or a vector with a maximum length of one." ) }
  
  if( ( length( dda ) > 1 | length( dda ) < 1 ) & is.null( denominator ) == T ) 
  { stop( "`dda` must be a single quoted string or a vector with a maximum length of one." ) }
  
  if( ( length( numerator ) > 1 & length( c( net.assets, invest, lbe, mnp ) ) == 0 ) )
  { stop( "`numerator` must be a single quoted string or a vector with a maximum length of one." ) }
  
  if( ( length( denominator ) > 1 & length( c( tot.func.exp, dda ) ) == 0 ) )
  { stop( "`denominator` must be a single quoted string or a vector with a maximum length of one." ) }
  
  
  
  # copy data
  dat <- df
  
  ## ensure variable classes are numeric ##
  
  # run coerce_numeric and loop through all variables required and that matched by the two input arguments
  v <- c( colnames( dat )[which( colnames( dat ) %in% net.assets ) ], colnames( dat )[which( colnames( dat ) %in% invest )],
          colnames( dat )[which( colnames( dat ) %in% mnp )], colnames( dat )[which( colnames( dat ) %in% lbe )], 
          colnames( dat )[which( colnames( dat ) %in% tot.func.exp )], colnames( dat )[which( colnames( dat ) %in% dda )],
          colnames( dat )[which( colnames( dat ) %in% numerator )], colnames( dat )[which( colnames( dat ) %in% denominator )] )
  
  dat <- coerce_numeric( d = dat, vars = v )
  
  if( is.null( numerator) == T & is.null( denominator ) == T ){
    
    num <- dat[[ net.assets ]] + dat[[ invest ]] - ( dat[[ lbe ]] + dat[[ mnp ]] )
    den <- ( dat[[ tot.func.exp ]] - dat[[ dda ]] ) / 365
    
  }
  
  if( is.null( numerator) == F & is.null( denominator ) == T ){
    
    num <- dat[[ numerator ]]
    den <- ( dat[[ tot.func.exp ]] - dat[[ dda ]] ) / 365
    
  }
  
  if( is.null( numerator) == T & is.null( denominator ) == F ){
    
    num <- dat[[ net.assets ]] + dat[[ invest ]] - ( dat[[ lbe ]] + dat[[ mnp ]] )
    den <- ( dat[[ denominator ]] ) / 365
    
  }
  
  if( is.null( numerator) == F & is.null( denominator ) == F ){
    
    num <- dat[[ numerator ]]
    den <- ( dat[[ denominator ]] ) / 365
    
  }
  
  # can't divide by zero
  print( paste0( "Denominator cannot be zero: ", sum( den==0, na.rm = T ), " cases have been replaced with NA." ) )
  den[ den == 0 ] <- NA 
  
  doci <- num / den
  
  top.p    <- 1 - (1-winsorize)/2
  bottom.p <- 0 + (1-winsorize)/2
  top      <- quantile( doci, top.p, na.rm=T )
  bottom   <- quantile( doci, bottom.p, na.rm=T )
  doci.w    <- doci
  doci.w[ doci.w > top    ] <- top
  doci.w[ doci.w < bottom ] <- bottom
  
  doci.n <- scale( doci.w )
  
  doci.p <- dplyr::ntile( doci, 100 )
  
  DOCI <- data.frame( doci, doci.w, doci.n, doci.p )
  
  print( summary( DOCI ) )
  
  par( mfrow=c(2,2) )
  plot( density( doci,   na.rm=T ), main="Days of Operating Cash and Investments (DOCI)" )
  plot( density( doci.w, na.rm=T ), main="DOCI Winsorized" )
  plot( density( doci.n, na.rm=T ), main="DOCI Standardized as Z" )
  plot( density( doci.p, na.rm=T ), main="DOCI as Percentile" )
  
  df.doci <- data.frame( cbind( df, DOCI ) )
  return( df.doci )
}
