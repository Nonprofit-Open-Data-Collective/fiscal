###---------------------------------------------------
###   MONTHS OF OPERATING CASH ON HAND
###---------------------------------------------------

#' @title
#' Months of Operating Cash on Hand 
#'
#' @description
#' Calculate the months of operating cash on hand and append it to the dataframe. 
#'
#' @param df A dataframe containing the required field for computing the metric. The metric will be appended to this dataset.
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
#' @return The original dataframe appended with the months of operating cash on hand (`moch`), 
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
#' x3 <- rnorm( 1000,100,30 )
#' x4 <- rnorm( 1000,200,30 )
#' x5 <- rnorm( 1000,100,30 )
#' x6 <- rnorm( 1000,200,30 )
#' 
#' x5[ c(15,300,600) ] <- 0
#' x6[ c(15,300,600) ] <- 0
#' 
#' dat <- data.frame( x1,x2, x3, x4, x5 , x6)
#' 
#' # specify own column names
#' d <- get_moch( df = dat, cash = 'x1', short.invest = 'x2', pledges.receive = 'x3', accounts.receive = 'x4',
#'                tot.func.exp = 'x5', dda = 'x6', numerator = NULL, denominator = NULL )
#' 
#' head( d )
#' 
#' # run with default column names
#' dat_01 <- dat
#' 
#' colnames( dat_01 ) <- c( 'ASSET_CASH_EOY', 'ASSET_SAVING_EOY', 'ASSET_PLEDGE_NET_BOY', 
#'                         'ASSET_ACC_NET_EOY', 'EXP_TOT_TOT','EXP_DEPREC_TOT' )
#'
#' 
#' d <- get_moch( dat_01 )
#' 
#' head( d )
#' 
#' # specify column names for aggregated variables only
#' x.den <- x5 + x6
#' x.num <- x1 + x2 + x3 + x4
#' 
#' dat_02 <- cbind( dat, x.den, x.num )
#' 
#' d <- get_moch( df = dat_02, cash = NULL, short.invest = NULL, pledges.receive = NULL, accounts.receive = NULL,
#'                tot.func.exp = NULL, dda = NULL, numerator = 'x.num', denominator = 'x.den' )
#' 
#' head ( d )
#' 
#' # specify column names for mixture of aggregated (denominator) and individual variables (numerator)
#' # and winsorize at 0.025 and 0.975 percentiles instead of 0.01 and 0.99
#' d <- get_moch( df = dat_02, cash = 'x1', short.invest = 'x2', pledges.receive = 'x3', accounts.receive = 'x4',
#'                tot.func.exp = NULL, dda = NULL, numerator = NULL, denominator = 'x.den', winsorize=0.95 )
#' 
#' head ( d )
#' 
#' # specify column names for mixture of aggregated (numerator) and individual variables (denominator)
#' # and winsorize at 0.025 and 0.975 percentiles instead of 0.01 and 0.99
#' 
#' d <- get_moch( df = dat_02, cash = NULL, short.invest = NULL, pledges.receive = NULL, accounts.receive = NULL,
#'                tot.func.exp = 'x5', dda = 'x6', numerator = 'x.num', denominator = NULL, winsorize=0.95 )
#' head ( d )
#' 
#' 
#' ## Errors ##
#' 
#' # incorrectly specify denominator
#' get_moch( df = dat_02, cash = 'x1', short.invest = 'x2', pledges.receive = 'x3', accounts.receive = 'x4',
#'           tot.func.exp = NULL, dda = 'x6', numerator = NULL, denominator = NULL, winsorize=0.98 )
#' 
#' # incorrectly specify numerator with conflicting arguments
#' get_moch( df = dat_02, cash = 'x1', short.invest = 'x2', pledges.receive = 'x3', accounts.receive = 'x4',
#'           tot.func.exp = NULL, dda = NULL, numerator = 'x.num', denominator = 'x.den' )
#' 
#' # incorrectly specify numerator with conflicting arguments
#' get_moch( df = dat_02, cash = 'x1', short.invest = 'x2', pledges.receive = 'x3', accounts.receive = 'x4',
#'           tot.func.exp = 'x5', dda = 'x6', numerator = NULL, denominator = 'x.den' )
#' 
#' # supplying no arguments for the numerator
#' get_moch( df = dat_02, cash = NULL, short.invest = NULL, pledges.receive = NULL, accounts.receive = NULL,
#'           tot.func.exp = 'x5', dda = 'x6', numerator = NULL, denominator = NULL )
#' 
#' get_moch( df = dat_03, cash = NULL, short.invest = NULL, pledges.receive = NULL, accounts.receive = NULL,
#'           tot.func.exp = NULL, dda = NULL, numerator = NULL, denominator = 'x.den' )
#' 
#' # supplying no arguments for the denominator
#' get_moch( df = dat_02, cash = 'x1', short.invest = 'x2', pledges.receive = 'x3', accounts.receive = 'x4',
#'           tot.func.exp = NULL, dda = NULL, numerator = NULL, denominator = NULL )
#' 
#' get_moch( df = dat_02, cash = NULL, short.invest = NULL, pledges.receive = NULL, accounts.receive = NULL,
#'           tot.func.exp = NULL, dda = NULL, numerator = 'x.num', denominator = NULL )
#' 
#' # supplying no arguments at all
#' get_moch( df = dat_02, cash = NULL, short.invest = NULL, pledges.receive = NULL, accounts.receive = NULL,
#'           tot.func.exp = NULL, dda = NULL, numerator = NULL, denominator = NULL )
#' 
#' @export
get_moch <- function( df, cash = 'ASSET_CASH_EOY', short.invest = 'ASSET_SAVING_EOY', pledges.receive = 'ASSET_PLEDGE_NET_BOY', accounts.receive = 'ASSET_ACC_NET_EOY', tot.func.exp = 'EXP_TOT_TOT', dda = 'EXP_DEPREC_TOT', numerator = NULL, denominator = NULL, winsorize = 0.98 )
{
  
  # checks
  if ( winsorize > 1 | winsorize < 0 )
  { stop( "winsorize argument must be 0 < w < 1" ) }
  
  
  if ( ( ( length( c( cash, short.invest, pledges.receive, accounts.receive ) ) < 4 )==F | is.null( numerator )==F ) &
       ( ( is.null( tot.func.exp )==T | is.null( dda )==T) &
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
  
  if ( ( length( c( tot.func.exp, dda ) ) <= 2 ) &
       ( length( c( tot.func.exp, dda ) ) >= 1 ) & 
       ( is.null( denominator )==F ) )
  { stop( "The denominator has been incorrectly specified with conflicting arguments. Ensure you are passing the correct data field to the correct argument." ) }
  
  if ( ( length( c( tot.func.exp, dda ) ) == 0 ) &
       ( length( c( tot.func.exp, dda ) ) == 0 ) & 
       ( is.null( denominator )==T & is.null ( numerator )==T ) )
  { stop( "The argument fields are empty. Please supply column names for each argument or execute the function with default inputs." ) }
  
  
  
  if ( ( length( c( cash, short.invest, pledges.receive, accounts.receive ) ) < 4 ) ==F & ( is.null( tot.func.exp )==F & is.null( dda )==F ) ){
    num <- df[[ cash ]] + df[[ short.invest ]] + df[[ pledges.receive ]] + df[[ accounts.receive ]]
    den <- ( df[[ tot.func.exp ]] + df[[ dda ]] ) / 12
  }
  
  else if ( (length( c( cash, short.invest, pledges.receive, accounts.receive ) ) < 4 ) ==F & ( is.null( tot.func.exp )==T & is.null( dda )==T & is.null( denominator )==F ) ){
    num <- df[[ cash ]] + df[[ short.invest ]] + df[[ pledges.receive ]] + df[[ accounts.receive ]]
    den <- df[[ denominator ]] / 12
  }
  
  else if ( (length( c( cash, short.invest, pledges.receive, accounts.receive ) ) == 0 ) ==T & ( is.null( tot.func.exp )==T & is.null( dda )==T & is.null( denominator )==F & is.null( numerator )==F ) ){
    num <- df[[ numerator ]]
    den <- df[[ denominator ]] / 12
  }
  
  else if ( (length( c( cash, short.invest, pledges.receive, accounts.receive ) ) == 0 ) ==T & ( is.null( tot.func.exp )==F & is.null( dda )==F & is.null( denominator )==T & is.null( numerator )==F ) ){
    num <- df[[ numerator ]]
    den <- ( df[[ tot.func.exp ]]+ df[[ dda ]] ) / 12
  }
  
  
  if( winsorize > 1 | winsorize < 0 ){
    stop( 'winsorize argument must be 0 < w < 1' )
  }
  
  print( paste0( 'Denominator cannot be equal to zero: ',sum( den==0 ),' cases have been replaced with NA' ))
  
  den[ den==0 ] <- NA
  
  moch <- num/den
  
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
  plot( density(moch,   na.rm=T), main="Months of Operating Cash on Hand (MOCH)" )
  plot( density(moch.w, na.rm=T), main="MOCH Winsorized" )
  plot( density(moch.n, na.rm=T), main="MOCH Standardized as Z" )
  plot( density(moch.p, na.rm=T), main="MOCH as Percentile" )
  
  df.moch <- cbind( df, MOCH )
  return( df.moch )
  
}
