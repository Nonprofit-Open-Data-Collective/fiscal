###---------------------------------------------------
###   MONTHS OF OPERATING CASH ON HAND
###---------------------------------------------------

#' @title
#' Months of Operating Cash on Hand 
#'
#' @description
#' Calculate the months of operating cash on hand and append it to the dataframe. 
#'
#' @param df A \code{data.frame} containing the required field for computing the metric. The metric will be appended to this dataset.
#' @param cash A character string indicating the column name for cash, EOY (On 990: Part X, line 1B; On EZ: Part II, line 22B (cash and short-term investments only)) with the default name supplied.
#' @param short.invest A character string indicating the column name for short-term investments, EOY (On 990: Part X, line 2B; On EZ: Part II, line 22B (cash and short-term investments only)) with the default name supplied.
#' @param pledges.receive A character string indicating the column name for pledges and grant receivables, EOY On 990: Part X, line 3B; On EZ: Not Available) with the default name supplied.
#' @param accounts.receive A character string indicating the column name for accounts receivables, EOY (On 990: Part X, line 4B; On EZ: Not Available) with the default name supplied.
#' @param tot.func.exp A character string indicating the column name for total functional expenses (On 990: Part IX, line 25A; On EZ: Not Available) with the default name supplied.
#' @param dda A character string indicating the column name for depreciation, depletion, and amortization (On 990: Part IX, line 22A; On EZ: Not Available) with the default name supplied.
#' @param numerator A character string indicating the user-supplied column name for a pre-aggregated variable for the numerator (CHANGE). Do not combine with numerator column component arguments (`cash`, `short.invest`,`pledges.receive`, `accounts.receive`). Users may also use this argument to supply the column variable for EZ-filers: cash, savings, and investment, EOY On EZ: Part II, line 22B (cash and short-term investments only)).
#' @param denominator A character string indicating the user-supplied column name for a pre-aggregated variable for the denominator (CHANGE). Do not combine with denominator column component arguments (`tot.func.exp`, `dda`). Users may also use this argument to supply the column variable for EZ-filers: Total operating expenses, EOY (On EZ: Part I, line 17 (operating expenses only)).
#' @param winsorize The winsorization value (between 0 and 1), defaults to 0.98 which winsorizes at 99th and 1st percentile values.   
#' 
#' @usage get_moch( df, cash = 'F9_10_ASSET_CASH_EOY', 
#' short.invest = 'F9_10_ASSET_SAVING_EOY', 
#' pledges.receive = 'F9_10_ASSET_PLEDGE_NET_EOY', 
#' accounts.receive = 'F9_10_ASSET_ACC_NET_EOY', 
#' tot.func.exp = 'F9_09_EXP_TOT_TOT', 
#' dda = 'F9_09_EXP_DEPREC_TOT', numerator = NULL, denominator = NULL, winsorize = 0.98 )
#' 
#' @return Object of class \code{data.frame}: the original dataframe with the months of operating cash on hand (`moch`), 
#'  a winsorized version (`moch.w`), a standardized z-score version (`moch.z`), 
#'  and a percentile version (`moch.p`).   
#'
#' @details Months of cash on hand is the number of months that an organization can continue to pay its 
#' operating expenses, given the amount of cash available. This metric is good to review at startup of 
#' organization, during periods of low revenues, and prior to undertaking new major activity.The metric is 
#' limited though in the following ways. First, it is based on an average monthly cash outflow, which is not 
#' really the case. Instead, cash tends to be spent in a lumpy manner, such as when rent or payroll are paid. 
#' Also, management tends to take drastic action to reduce expenses as cash reserves decline, so that the 
#' actual months of operation tend to be longer than indicated by this ratio. Thus, it is better to use a 
#' detailed cash flow analysis to determine the precise duration of the available cash, with regular updates.
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
#' d <- get_moch( df=dat, cash='x1', short.invest='x2', pledges.receive='x3', accounts.receive='x4', tot.func.exp='x5', dda = 'x6', winsorize=0.98 )
#' 
#' head( d )
#' 
#' # run with default column names
#' dat_01 <- dat
#' 
#' colnames( dat_01 ) <- c( "F9_10_ASSET_CASH_EOY", "F9_10_ASSET_SAVING_EOY", "F9_10_ASSET_PLEDGE_NET_EOY",
#'                          "F9_10_ASSET_ACC_NET_EOY", "F9_09_EXP_TOT_TOT", "F9_09_EXP_DEPREC_TOT" )
#' 
#' 
#' d <- get_moch( dat_01 )
#' 
#' # coerce one column to factor
#' dat_01$F9_10_ASSET_ACC_NET_EOY <- as.factor( dat_01$F9_10_ASSET_ACC_NET_EOY )
#' 
#' d <- get_moch( dat_01 )
#' 
#' # winsorize at 0.025 and 0.975 percentiles instead of 0.01 and 0.99
#' d <- get_moch( df=dat, cash='x1', short.invest='x2', pledges.receive='x3', accounts.receive='x4', tot.func.exp='x5', dda = 'x6', winsorize = 0.95 )
#' 
#' d <- get_moch( dat_01, winsorize = 0.95 )
#' 
#' # aggregate variables into single numerator and denominator
#' x.den <- x5 + x6
#' x.num <- x1 + x2 + x3 + x4
#' 
#' dat_02 <- cbind( dat, x.den, x.num)
#' 
#' d <- get_moch( dat_02, numerator = "x.num", denominator = "x.den" )
#' 
#' # using 990 data
#' load( '/Volumes/My Passport for Mac/Urban Institute/Summer Projects/Fiscal/fiscal/R/sysdata.rda' )
#' d <- get_moch( df = part010810 )
#' 
#' # now coerce one of the variables to numeric
#' part010810$F9_09_EXP_TOT_TOT <- as.character( part010810$F9_09_EXP_TOT_TOT )
#' 
#' d <- get_moch( df = part010810 )
#' 
#' # incorrectly specify denominator
#' get_moch( df = dat_02, cash = 'x1', short.invest = 'x2', pledges.receive = 'x3', accounts.receive = 'x4', 
#'           tot.func.exp = NULL, numerator = NULL, denominator = NULL, winsorize=0.98 )
#' 
#' # incorrectly specify numerator with conflicting arguments
#' get_moch( df = dat_02, cash = 'x1', short.invest = 'x2', pledges.receive = 'x3', accounts.receive = 'x4', 
#'           tot.func.exp = NULL, dda = NULL, numerator = 'x.num', denominator = 'x.den' , winsorize=0.98 )
#' 
#' 
#' # incorrectly specify denominator with conflicting arguments
#' get_moch( df = dat_02, cash = 'x1', short.invest = 'x2', pledges.receive = 'x3', accounts.receive = 'x4', 
#'           tot.func.exp = 'x5', dda = 'x6', numerator = NULL, denominator = 'x.den' , winsorize=0.98 )  
#' 
#' # supplying no arguments for the numerator
#' get_moch( df = dat_02, cash = NULL, short.invest = NULL, pledges.receive = NULL, accounts.receive = NULL, 
#'           tot.func.exp = 'x5', dda = 'x6', numerator = NULL, denominator = NULL , winsorize=0.98 )  
#' 
#' # supplying no arguments for the denominator
#' get_moch( df = dat_02, cash = 'x1', short.invest = 'x2', pledges.receive = 'x3', accounts.receive = 'x4', 
#'           tot.func.exp = NULL, dda = NULL, numerator = NULL, denominator = NULL , winsorize=0.98 )  
#' 
#' # supplying argument for one of the parameters in the numerator that is greater than length 1
#' get_moch( df = dat_02, cash = 'x1', short.invest = 'x2', pledges.receive = 'x3', accounts.receive = 'x4', 
#'           tot.func.exp = c( 'x5', 'x6' ), dda = 'x6', numerator = NULL, denominator = NULL, winsorize=0.98 )
#' 
#' get_moch( df = dat_02, cash = NULL, short.invest = NULL, pledges.receive = NULL, accounts.receive = NULL, 
#'           tot.func.exp = NULL, dda = NULL, numerator = c( 'x5', 'x6' ), denominator = 'x.den' , winsorize=0.98 )  
#' @export
get_moch <- function( df, cash = 'F9_10_ASSET_CASH_EOY', 
                      short.invest = 'F9_10_ASSET_SAVING_EOY', 
                      pledges.receive = 'F9_10_ASSET_PLEDGE_NET_EOY', 
                      accounts.receive = 'F9_10_ASSET_ACC_NET_EOY', 
                      tot.func.exp = 'F9_09_EXP_TOT_TOT', 
                      dda = 'F9_09_EXP_DEPREC_TOT', numerator = NULL, denominator = NULL, winsorize = 0.98 )
{
  
  # checks
  if( winsorize > 1 | winsorize < 0 )
  { stop( "winsorize argument must be 0 < w < 1" ) }
  
  if( is.null( cash )==F & is.null( short.invest )==F & is.null( pledges.receive )==F & is.null( accounts.receive )==F &
      is.null( tot.func.exp )==F & is.null( dda )==F ) {
    
    if( cash == "F9_10_ASSET_CASH_EOY" & short.invest == "F9_10_ASSET_SAVING_EOY" &
        pledges.receive == "F9_10_ASSET_PLEDGE_NET_EOY" & accounts.receive == "F9_10_ASSET_ACC_NET_EOY" & tot.func.exp == "F9_09_EXP_TOT_TOT" &
        dda == "F9_09_EXP_DEPREC_TOT" & is.null( numerator )==F & is.null( denominator )==F ){
      
      warning( "Default argument inputs overridden with specified numerator and denominator arguments" ) 
      
      cash <- NULL
      short.invest <- NULL
      pledges.receive <- NULL
      accounts.receive <- NULL
      tot.func.exp <- NULL
      dda <- NULL
      
    }
    
  } 
  
  if ( ( ( length( c( cash, short.invest, pledges.receive, accounts.receive ) ) < 4 )==F | is.null( numerator )==F ) &
       ( ( is.null( tot.func.exp )==T & is.null( dda )==T ) &
         is.null( denominator )==T ) )
  { stop( "The denominator has been incorrectly specified. Ensure you are passing the correct data field to the correct argument." ) }
  
  if ( ( ( length( c( tot.func.exp, dda ) ) < 2 )==F | is.null( denominator )==F ) &
       ( ( is.null( cash )==T | is.null( short.invest )==T | 
           is.null( pledges.receive )==T | is.null( accounts.receive )==T ) &
         is.null( numerator )==T ) )
  { stop( "The numerator has been incorrectly specified. Ensure you are passing the correct data field to the correct argument." ) }
  
  
  if ( ( length( c( cash, short.invest, pledges.receive, accounts.receive ) ) <= 4 ) &
       ( length( c( cash, short.invest, pledges.receive, accounts.receive ) ) >= 1 ) & 
       ( is.null( numerator )==F ) )
  { stop( "The numerator has been incorrectly specified with conflicting arguments. Ensure you are passing the correct data field to the correct argument." ) }
  
  if ( ( length( c( tot.func.exp, dda ) ) >= 2 ) & 
       ( is.null( denominator )==F ) )
  { stop( "The denominator has been incorrectly specified with conflicting arguments. Ensure you are passing the correct data field to the correct argument." ) }
  
  if( ( length( cash ) > 1 | length( cash ) < 1 ) & is.null( numerator ) == T ) 
  { stop( "`cash` must be a single quoted string or a vector with a maximum length of one." ) }
  
  if( ( length( short.invest ) > 1 | length( short.invest ) < 1 ) & is.null( numerator ) == T ) 
  { stop( "`short.invest` must be a single quoted string or a vector with a maximum length of one." ) }
  
  if( ( length( pledges.receive ) > 1 | length( pledges.receive ) < 1 ) & is.null( numerator ) == T ) 
  { stop( "`pledges.receive` must be a single quoted string or a vector with a maximum length of one." ) }
  
  if( ( length( accounts.receive ) > 1 | length( accounts.receive ) < 1 ) & is.null( numerator ) == T ) 
  { stop( "`accounts.receive` must be a single quoted string or a vector with a maximum length of one." ) }
  
  if( ( length( tot.func.exp ) > 1 | length( tot.func.exp ) < 1 ) & is.null( denominator ) == T ) 
  { stop( "`tot.func.exp` must be a single quoted string or a vector with a maximum length of one." ) }
  
  if( ( length( dda ) > 1 | length( dda ) < 1 ) & is.null( denominator ) == T ) 
  { stop( "`dda` must be a single quoted string or a vector with a maximum length of one." ) }
  
  if( ( length( numerator ) > 1 & length( c( cash, short.invest, pledges.receive, accounts.receive ) ) == 0 ) )
  { stop( "`numerator` must be a single quoted string or a vector with a maximum length of one." ) }
  
  if( ( length( denominator ) > 1 & length( c( tot.func.exp, dda ) ) == 0 ) )
  { stop( "`denominator` must be a single quoted string or a vector with a maximum length of one." ) }
  
  
  
  # copy data
  dat <- df
  
  ## ensure variable classes are numeric ##
  
  # run coerce_numeric and loop through all variables required and that matched by the two input arguments
  v <- c( colnames( dat )[which( colnames( dat ) %in% cash ) ], colnames( dat )[which( colnames( dat ) %in% short.invest )],
          colnames( dat )[which( colnames( dat ) %in% accounts.receive )], colnames( dat )[which( colnames( dat ) %in% pledges.receive )], 
          colnames( dat )[which( colnames( dat ) %in% tot.func.exp )], colnames( dat )[which( colnames( dat ) %in% dda )],
          colnames( dat )[which( colnames( dat ) %in% numerator )], colnames( dat )[which( colnames( dat ) %in% denominator )] )
  
  dat <- coerce_numeric( d = dat, vars = v )
  
  if( is.null( numerator) == T & is.null( denominator ) == T ){
    
    num <- dat[[ cash ]] + dat[[ short.invest ]] + dat[[ pledges.receive ]] + dat[[ accounts.receive ]]
    den <- ( dat[[ tot.func.exp ]] - dat[[ dda ]] ) / 12
    
  }
  
  if( is.null( numerator) == F & is.null( denominator ) == T ){
    
    num <- dat[[ numerator ]]
    den <- ( dat[[ tot.func.exp ]] - dat[[ dda ]] ) / 12
    
  }
  
  if( is.null( numerator) == T & is.null( denominator ) == F ){
    
    num <- dat[[ cash ]] + dat[[ short.invest ]] + dat[[ pledges.receive ]] + dat[[ accounts.receive ]]
    den <- ( dat[[ denominator ]] ) / 12
    
  }
  
  if( is.null( numerator) == F & is.null( denominator ) == F ){
    
    num <- dat[[ numerator ]]
    den <- ( dat[[ denominator ]] ) / 12
    
  }
  
  # can't divide by zero
  print( paste0( "Denominator cannot be zero: ", sum( den==0, na.rm = T ), " cases have been replaced with NA." ) )
  den[ den == 0 ] <- NA 
  
  moch <- num / den
  
  top.p    <- 1 - (1-winsorize)/2
  bottom.p <- 0 + (1-winsorize)/2
  top      <- quantile( moch, top.p, na.rm=T )
  bottom   <- quantile( moch, bottom.p, na.rm=T )
  moch.w    <- moch
  moch.w[ moch.w > top    ] <- top
  moch.w[ moch.w < bottom ] <- bottom
  
  moch.n <- scale( moch.w )
  
  moch.p <- dplyr::ntile( moch, 100 )
  
  MOCH <- data.frame( moch, moch.w, moch.n, moch.p )
  
  print( summary( MOCH ) )
  
  par( mfrow=c(2,2) )
  plot( density( moch,   na.rm=T ), main="Days of Cash on Hand (MOCH)" )
  plot( density( moch.w, na.rm=T ), main="MOCH Winsorized" )
  plot( density( moch.n, na.rm=T ), main="MOCH Standardized as Z" )
  plot( density( moch.p, na.rm=T ), main="MOCH as Percentile" )
  
  df.moch <- data.frame( cbind( df, MOCH ) )
  return( df.moch )
}
