###---------------------------------------------------
###   DAYS OF OPERATING CASH ON HAND
###---------------------------------------------------

#' @title
#' Days of Operating Cash on Hand 
#'
#' @description
#' Calculate the days of operating cash on hand and append it to the dataframe. 
#'
#' @param df A dataframe containing the required field for computing the metric. The metric will be appended to this dataset.
#' @param cash A character string indicating the column name for cash, EOY (On 990: Part X, line 1B; On EZ: Part II, line 22B (cash and short-term investments only)) with the default name supplied.
#' @param short.invest A character string indicating the column name for short-term investments, EOY (On 990: Part X, line 2B; On EZ: Part II, line 22B (cash and short-term investments only)) with the default name supplied.
#' @param pledges.receive A character string indicating the column name for pledges and grant receivables, EOY On 990: Part X, line 3B; On EZ: Not Available) with the default name supplied.
#' @param accounts.receive A character string indicating the column name for accounts receivables, EOY (On 990: Part X, line 4B; On EZ: Not Available) with the default name supplied.
#' @param tot.func.exp A character string indicating the column name for total functional expenses (On 990: Part IX, line 25A; On EZ: Not Available) with the default name supplied.
#' @param dda A character string indicating the column name for depreciation, depletion, and amortization (On 990: Part IX, line 22A; On EZ: Not Available) with the default name supplied.
#' @param ez.csi A character string indicating the user-supplied column name for a pre-aggregated variable for the numerator (CHANGE). Do not combine with numerator column component arguments (`cash`, `short.invest`,`pledges.receive`, `accounts.receive`). Users may also use this argument to supply the column variable for EZ-filers: cash, savings, and investment, EOY On EZ: Part II, line 22B (cash and short-term investments only)).
#' @param ez.toe A character string indicating the user-supplied column name for a pre-aggregated variable for the denominator (CHANGE). Do not combine with denominator column component arguments (`tot.func.exp`, `dda`). Users may also use this argument to supply the column variable for EZ-filers: Total operating expenses, EOY (On EZ: Part I, line 17 (operating expenses only)).
#' @param winsorize The winsorization value (between 0 and 1), defaults to 0.98 which winsorizes at 99th and 1st percentile values.   
#' 
#' @return The original dataframe appended with the days of operating cash on hand (`doch`), 
#'  a winsorized version (`doch.w`), a standardized z-score version (`doch.z`), 
#'  and a percentile version (`doch.p`).   
#'
#' @details Days cash on hand is the number of days that an organization can continue to pay 
#' its operating expenses, given the amount of cash available. This metric is good to review at startup 
#' of organization, during periods of low revenues, and prior to undertaking new major activity. The metric 
#' is limited though in the following ways. First, it is based on an average daily cash outflow, which is 
#' not really the case. Instead, cash tends to be spent in a lumpy manner, such as when rent or payroll 
#' are paid. Also, management tends to take drastic action to reduce expenses as cash reserves decline, 
#' so that the actual days of operation tend to be longer than indicated by this ratio. Thus, it is better 
#' to use a detailed cash flow analysis to determine the precise duration of the available cash, with regular 
#' updates. Note: computation of this metric is available to both 990 and 990-EZ filers. The default inputs use 
#' column names for variables available only to 990 filers. 
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
#' d <- get_doch( df = dat, cash = 'x1', short.invest = 'x2', pledges.receive = 'x3', accounts.receive = 'x4',
#'                tot.func.exp = 'x5', dda = 'x6', ez.csi = NULL, ez.toe = NULL )
#' 
#' head( d )
#' 
#' # run with default column names
#' dat_01 <- dat
#' 
#' colnames( dat_01 ) <- c( 'name1', 'name2', 'name3', 
#'                          'name4', 'name5', 'name6' )
#' 
#' d <- get_doch( dat_01 )
#' 
#' head( d )
#' 
#' # specify column names for aggregated variables only
#' x.den <- x5 + x6
#' x.num <- x1 + x2 + x3 + x4
#' 
#' dat_02 <- cbind( dat, x.den, x.num )
#' 
#' d <- get_doch( df = dat_02, cash = NULL, short.invest = NULL, pledges.receive = NULL, accounts.receive = NULL,
#'                tot.func.exp = NULL, dda = NULL, ez.csi = 'x.num', ez.toe = 'x.den' )
#' 
#' head ( d )
#' 
#' # specify column names for mixture of aggregated (denominator) and individual variables (numerator)
#' # and winsorize at 0.025 and 0.975 percentiles instead of 0.01 and 0.99
#' d <- get_doch( df = dat_02, cash = 'x1', short.invest = 'x2', pledges.receive = 'x3', accounts.receive = 'x4',
#'                tot.func.exp = NULL, dda = NULL, ez.csi = NULL, ez.toe = 'x.den', winsorize=0.95 )
#' 
#' head ( d )
#' 
#' # specify column names for mixture of aggregated (numerator) and individual variables (denominator)
#' # and winsorize at 0.025 and 0.975 percentiles instead of 0.01 and 0.99
#' 
#' d <- get_doch( df = dat_02, cash = NULL, short.invest = NULL, pledges.receive = NULL, accounts.receive = NULL,
#'                tot.func.exp = 'x5', dda = 'x6', ez.csi = 'x.num', ez.toe = NULL, winsorize=0.95 )
#' head ( d )
#' 
#' 
#' ## Errors ##
#' 
#' # incorrectly specify denominator
#' get_doch( df = dat_02, cash = 'x1', short.invest = 'x2', pledges.receive = 'x3', accounts.receive = 'x4',
#'           tot.func.exp = NULL, dda = 'x6', ez.csi = NULL, ez.toe = NULL, winsorize=0.98 )
#' 
#' # incorrectly specify numerator with conflicting arguments
#' get_doch( df = dat_02, cash = 'x1', short.invest = 'x2', pledges.receive = 'x3', accounts.receive = 'x4',
#'           tot.func.exp = NULL, dda = NULL, ez.csi = 'x.num', ez.toe = 'x.den' )
#' 
#' # incorrectly specify numerator with conflicting arguments
#' get_doch( df = dat_02, cash = 'x1', short.invest = 'x2', pledges.receive = 'x3', accounts.receive = 'x4',
#'           tot.func.exp = 'x5', dda = 'x6', ez.csi = NULL, ez.toe = 'x.den' )
#' 
#' # supplying no arguments for the numerator
#' get_doch( df = dat_02, cash = NULL, short.invest = NULL, pledges.receive = NULL, accounts.receive = NULL,
#'           tot.func.exp = 'x5', dda = 'x6', ez.csi = NULL, ez.toe = NULL )
#' 
#' get_doch( df = dat_03, cash = NULL, short.invest = NULL, pledges.receive = NULL, accounts.receive = NULL,
#'           tot.func.exp = NULL, dda = NULL, ez.csi = NULL, ez.toe = 'x.den' )
#' 
#' # supplying no arguments for the denominator
#' get_doch( df = dat_02, cash = 'x1', short.invest = 'x2', pledges.receive = 'x3', accounts.receive = 'x4',
#'           tot.func.exp = NULL, dda = NULL, ez.csi = NULL, ez.toe = NULL )
#' 
#' get_doch( df = dat_02, cash = NULL, short.invest = NULL, pledges.receive = NULL, accounts.receive = NULL,
#'           tot.func.exp = NULL, dda = NULL, ez.csi = 'x.num', ez.toe = NULL )
#' 
#' # supplying no arguments at all
#' get_doch( df = dat_02, cash = NULL, short.invest = NULL, pledges.receive = NULL, accounts.receive = NULL,
#'           tot.func.exp = NULL, dda = NULL, ez.csi = NULL, ez.toe = NULL )
#' 
#' @export
get_doch <- function( df, cash = 'name1', short.invest = 'name2', pledges.receive = 'name3', accounts.receive = 'name4', tot.func.exp = 'name5', dda = 'name6', ez.csi = NULL, ez.toe = NULL,winsorize = 0.98 )
{
  
  # checks
  if ( winsorize > 1 | winsorize < 0 )
  { stop( "winsorize argument must be 0 < w < 1" ) }
  
  
  if ( ( ( length( c( cash, short.invest, pledges.receive, accounts.receive ) ) < 4 )==F | is.null( ez.csi )==F ) &
       ( ( is.null( tot.func.exp )==T | is.null( dda )==T) &
         is.null( ez.toe )==T ) )
  { stop( "The denominator has been incorrectly specified. Ensure you are passing the correct data field to the correct argument." ) }
  
  if ( ( ( length( c( tot.func.exp, dda ) ) < 2 )==F | is.null( ez.toe )==F ) &
       ( ( is.null( cash )==T | is.null( short.invest )==T | 
           is.null( pledges.receive )==T | is.null( accounts.receive )==T ) &
         is.null( ez.csi )==T ) )
  { stop( "The numerator has been incorrectly specified. Ensure you are passing the correct data field to the correct argument." ) }
  
  if ( ( length( c( cash, short.invest, pledges.receive, accounts.receive ) ) <= 4 ) &
       ( length( c( cash, short.invest, pledges.receive, accounts.receive ) ) >= 1 ) & 
       ( is.null( ez.csi )==F ) )
  { stop( "The numerator has been incorrectly specified with conflicting arguments. Ensure you are passing the correct data field to the correct argument." ) }
  
  if ( ( length( c( tot.func.exp, dda ) ) <= 2 ) &
       ( length( c( tot.func.exp, dda ) ) >= 1 ) & 
       ( is.null( ez.toe )==F ) )
  { stop( "The denominator has been incorrectly specified with conflicting arguments. Ensure you are passing the correct data field to the correct argument." ) }

  if ( ( length( c( tot.func.exp, dda ) ) == 0 ) &
       ( length( c( tot.func.exp, dda ) ) == 0 ) & 
       ( is.null( ez.toe )==T & is.null ( ez.csi )==T ) )
  { stop( "The argument fields are empty. Please supply column names for each argument or execute the function with default inputs." ) }
  
  
  
  if ( ( length( c( cash, short.invest, pledges.receive, accounts.receive ) ) < 4 ) ==F & ( is.null( tot.func.exp )==F & is.null( dda )==F ) ){
    num <- df[[ cash ]] + df[[ short.invest ]] + df[[ pledges.receive ]] + df[[ accounts.receive ]]
    den <- ( df[[ tot.func.exp ]] + df[[ dda ]] ) / 365
  }
  
  else if ( (length( c( cash, short.invest, pledges.receive, accounts.receive ) ) < 4 ) ==F & ( is.null( tot.func.exp )==T & is.null( dda )==T & is.null( ez.toe )==F ) ){
    num <- df[[ cash ]] + df[[ short.invest ]] + df[[ pledges.receive ]] + df[[ accounts.receive ]]
    den <- df[[ ez.toe ]] / 365
  }
  
  else if ( (length( c( cash, short.invest, pledges.receive, accounts.receive ) ) == 0 ) ==T & ( is.null( tot.func.exp )==T & is.null( dda )==T & is.null( ez.toe )==F & is.null( ez.csi )==F ) ){
    num <- df[[ ez.csi ]]
    den <- df[[ ez.toe ]] / 365
  }
  
  else if ( (length( c( cash, short.invest, pledges.receive, accounts.receive ) ) == 0 ) ==T & ( is.null( tot.func.exp )==F & is.null( dda )==F & is.null( ez.toe )==T & is.null( ez.csi )==F ) ){
    num <- df[[ ez.csi ]]
    den <- ( df[[ tot.func.exp ]]+ df[[ dda ]] ) / 365
  }
  
  
  if( winsorize > 1 | winsorize < 0 ){
    stop( 'winsorize argument must be 0 < w < 1' )
  }
  
  print( paste0('Denominator cannot be equal to zero: ',sum( den==0 ),' cases have been replaced with NA' ))
  
  den[ den==0 ] <- NA
  
  doch <- num/den
  
  top.p    <- 1 - (1-winsorize)/2
  bottom.p <- 0 + (1-winsorize)/2
  top      <- quantile( doch, top.p, na.rm=T )
  bottom   <- quantile( doch, bottom.p, na.rm=T )
  doch.w    <- doch
  doch.w[ doch.w > top    ] <- top
  doch.w[ doch.w < bottom ] <- bottom
  
  doch.n <- scale( doch.w )
  
  doch.p <- dplyr::ntile( doch, 100 )
  
  DOCH <- data.frame( doch, doch.w, doch.n, doch.p )
  
  print( summary( DOCH ) )
  
  par( mfrow=c(2,2) )
  plot( density(doch,   na.rm=T), main="Days of Operating Cash on Hand (DOCH)" )
  plot( density(doch.w, na.rm=T), main="DOCH Winsorized" )
  plot( density(doch.n, na.rm=T), main="DOCH Standardized as Z" )
  plot( density(doch.p, na.rm=T), main="DOCH as Percentile" )
  
  df.doch <- cbind( df, DOCH )
  return( df.doch )
  
}
