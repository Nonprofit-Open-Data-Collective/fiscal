###---------------------------------------------------
###   ADD PIPE OPERATOR TO THE PACKAGE
###---------------------------------------------------
#' @importFrom magrittr "%>%"

#' @title
#' Current Ratio
#' 
#' @description 
#' Calculate the current ratio and append it to the dataframe.
#' 
#' @param df A dataframe containing the required field for computing the metric. The metric will be appended to this dataset.
#' @param cash A character string indicating the column name for cash, EOY (On 990: Part X, line 1B; On EZ:Part I, line 22 (cash and short-term investments only)).
#' @param short.invest A character string indicating the column name for short-term investments, EOY (On 990: Part X, line 2B; On EZ:Part I, line 22 (cash and short-term investments only)).
#' @param pledges.recieve A character string indicating the column name for pledges and grant receivables, EOY On 990: Part X, line 3B; On EZ: Not Available).
#' @param accounts.recieve A character string indicating the column name for accounts receivables, EOY (On 990: Part X, line 4B; On EZ: Not Available).
#' @param inventories.sale A character string indicating the column name for inventories for sale or use, EOY (On 990: Part X, line 8B; On EZ: Not Available).
#' @param prepaid.expense A character string indicating the column name for prepaid expenses and deferred charges, EOY (On 990: Part X, line 9B; On EZ: Not Available).
#' @param accounts.payable A character string indicating the column name for accounts payable, EOY (On 990: (Part X, line 17B); On EZ: Not available).
#' @param grants.payable A character string indicating the column name for grants payable, EOY (On 990: (Part X, line 18B); On EZ: Not available).
#' @param current.assets A character string indicating the user-supplied column name for a pre-aggregated variable for the numerator (current assets). Do not combine with numerator column component arguments (`cash`, `short.invest`,`pledges.recieve`, `accounts.recieve`, `inventories.sale`, `prepaid.expense`).
#' @param current.liabilities A character string indicating the user-supplied column name for a pre-aggregated variable for the denominator (current liabilities). Do not combine with denominator column component arguments (`accounts.payable`, `grants.payable`).
#' @param winsorize The winsorization value (between 0 and 1), defaults to 0.98 which winsorizes at 99th and 1st percentile values.   
#' 
#' @return The original dataframe with the current ratio (`cr`),
#' a winsorized version (`cr.w`), a standardized z-score version (`cr.z`), 
#' and a percentile version (`cr.p`).  
#'  
#' @details The current ratio is used to measure the overall liquidity of a nonprofit organization.
#' In its simplest form, it shows how many dollars of current assets an organization has to cover its 
#' current obligations. The higher the ratio, the more liquid the organization.As a rule of thumb, organizations 
#' should strive for a current ratio of 1.0 or higher. An organization with a ratio of 1.0 would have one dollar 
#' of assets to pay for every dollar of current liabilities.
#'   
#' @examples 
#' x1 <- rnorm( 1000,100,30 )
#' x2 <- rnorm( 1000,200,30 )
#' x3 <- rnorm( 1000,100,30 )
#' x4 <- rnorm( 1000,200,30 )
#' x5 <- rnorm( 1000,100,30 )
#' x6 <- rnorm( 1000,200,30 )
#' x7 <- rnorm( 1000,100,30 )
#' x8 <- rnorm( 1000,200,30 )
#' x7[ c(15,300,600) ] <- 0
#' x8[ c(15,300,600) ] <- 0
#'
#' dat <- data.frame( x1,x2, x3, x4, x5, x6, x7, x8)
#'
#' # specify own column names
#' d <- get_cr( df=dat, cash = 'x1', short.invest = 'x2', pledges.recieve = 'x3', accounts.recieve = 'x4', 
#'             inventories.sale = 'x5', prepaid.expenses = 'x6', accounts.payable = 'x7', grants.payable = 'x8', current.assets = NULL, current.liabilities = NULL )
#'
#'head( d )
#'
#' # run with default column names
#' dat_01 <- dat
#'
#' colnames( dat_01 ) <- c( 'ASSET_CASH_EOY', 'ASSET_SAVING_EOY', 'ASSET_PLEDGE_NET_BOY', 
#'                        'ASSET_ACC_NET_EOY', 'ASSET_INV_SALE_EOY', 'ASSET_EXP_PREPAID_EOY', 
#'                        'LIAB_ACC_PAYABLE_EOY', 'LIAB_GRANT_PAYABLE_EOY' )
#'
#' d <- get_cr( dat_01 )
#' 
#' head( d )
#'
#' # specify column names for aggregated variables only
#' x.den <- x7 + x8
#' x.num <- x1 + x2 + x3 + x4 + x5 + x6
#'
#' dat_02 <- cbind( dat, x.den, x.num)
#'
#' d <- get_cr( df=dat_02, cash = NULL, short.invest = NULL, pledges.recieve = NULL, accounts.recieve = NULL, 
#'            inventories.sale = NULL, prepaid.expenses = NULL, accounts.payable = NULL, grants.payable = NULL, current.assets = 'x.num', current.liabilities = 'x.den' )
#' 
#' head ( d )
#'
#' # specify column names for mixture of aggregated (denominator) and individual variables (numerator)
#' # and winsorize at 0.025 and 0.975 percentiles instead of 0.01 and 0.99
#' d <- get_cr( df=dat_02, cash = 'x1', short.invest = 'x2', pledges.recieve = 'x3', accounts.recieve = 'x4', 
#'              inventories.sale = 'x5', prepaid.expenses = 'x6', accounts.payable = NULL, grants.payable = NULL, current.assets = NULL, current.liabilities = 'x.den', winsorize=0.95 )
#'
#' head ( d )
#'
#' # specify column names for mixture of aggregated (numerator) and individual variables (denominator)
#' # and winsorize at 0.025 and 0.975 percentiles instead of 0.01 and 0.99
#'
#' d <- get_cr( df=dat_02, cash = NULL, short.invest = NULL, pledges.recieve = NULL, accounts.recieve = NULL, 
#'              inventories.sale = NULL, prepaid.expenses = NULL, accounts.payable = 'x7', grants.payable = 'x8', current.assets = 'x.num', current.liabilities = NULL, winsorize=0.95 )
#' head ( d )
#'
#'
#' ## Errors ##
#'
#' # incorrectly specify denominator
#' get_cr( df=dat_02, cash = 'x1', short.invest = 'x2', pledges.recieve = 'x3', accounts.recieve = 'x4', 
#'         inventories.sale = 'x5', prepaid.expenses = 'x6', accounts.payable = NULL, grants.payable = 'x8', current.assets = NULL, current.liabilities = NULL, winsorize=0.98 )
#' 
#' # incorrectly specify numerator with conflicting arguments
#' get_cr( df=dat_02, cash = 'x1', short.invest = 'x2', pledges.recieve = 'x3', accounts.recieve = 'x4', 
#'         inventories.sale = 'x5', prepaid.expenses = 'x6', accounts.payable = NULL, grants.payable = NULL, current.assets = 'x.num', current.liabilities = 'x.den' )
#'
#' # incorrectly specify numerator with conflicting arguments
#' get_cr( df=dat_02, cash = 'x1', short.invest = 'x2', pledges.recieve = 'x3', accounts.recieve = 'x4', 
#'         inventories.sale = 'x5', prepaid.expenses = 'x6', accounts.payable = 'x7', grants.payable = 'x8', current.assets = NULL, current.liabilities = 'x.den' )
#'
#' # supplying no arguments for the numerator
#' get_cr( df=dat_02, cash = NULL, short.invest = NULL, pledges.recieve = NULL, accounts.recieve = NULL, 
#'        inventories.sale = NULL, prepaid.expenses = NULL, accounts.payable = 'x7', grants.payable = 'x8', current.assets = NULL, current.liabilities = NULL )
#'
#' get_cr( df=dat_03, cash = NULL, short.invest = NULL, pledges.recieve = NULL, accounts.recieve = NULL, 
#'         inventories.sale = NULL, prepaid.expenses = NULL, accounts.payable = NULL, grants.payable = NULL, current.assets = NULL, current.liabilities = 'x.den' )
#'
#' # supplying no arguments for the denominator
#' get_cr( df=dat_02, cash = 'x1', short.invest = 'x2', pledges.recieve = 'x3', accounts.recieve = 'x4', 
#'        inventories.sale = 'x5', prepaid.expenses = 'x6', accounts.payable = NULL, grants.payable = NULL, current.assets = NULL, current.liabilities = NULL )
#'
#' get_cr( df=dat_02, cash = NULL, short.invest = NULL, pledges.recieve = NULL, accounts.recieve = NULL, 
#'        inventories.sale = NULL, prepaid.expenses = NULL, accounts.payable = NULL, grants.payable = NULL, current.assets = 'x.num', current.liabilities = NULL )
#'
#'@export
get_cr<-function( df, cash = 'ASSET_CASH_EOY', short.invest = 'ASSET_SAVING_EOY', pledges.recieve = 'ASSET_PLEDGE_NET_BOY', accounts.recieve = 'ASSET_ACC_NET_EOY', inventories.sale = 'ASSET_INV_SALE_EOY', prepaid.expenses = 'ASSET_EXP_PREPAID_EOY', accounts.payable = 'LIAB_ACC_PAYABLE_EOY', grants.payable = 'LIAB_GRANT_PAYABLE_EOY', current.assets = NULL, current.liabilities = NULL, winsorize=0.98 )
{
  # checks
  if ( winsorize > 1 | winsorize < 0 )
  { stop( "winsorize argument must be 0 < w < 1" ) }
  

  if ( ( ( length( c( cash, short.invest, pledges.recieve, accounts.recieve, inventories.sale, prepaid.expenses ) ) < 6 )==F | is.null( current.assets )==F ) &
       ( ( is.null( accounts.payable )==T | is.null( grants.payable )==T) &
         is.null( current.liabilities )==T ) )
  { stop( "The denominator has been incorrectly specified. Ensure you are passing the correct data field to the correct argument." ) }
  
  if ( ( ( length( c( accounts.payable, grants.payable ) ) < 2 )==F | is.null( current.liabilities )==F ) &
       ( ( is.null( cash )==T | is.null( short.invest )==T | 
           is.null( pledges.recieve )==T | is.null( accounts.recieve )==T |
           is.null( inventories.sale )==T | is.null( prepaid.expenses )==T) &
           is.null( current.assets )==T ) )
  { stop( "The numerator has been incorrectly specified. Ensure you are passing the correct data field to the correct argument." ) }
  
  if ( ( length( c( cash, short.invest, pledges.recieve, accounts.recieve, inventories.sale, prepaid.expenses ) ) <= 6 ) &
       ( length( c( cash, short.invest, pledges.recieve, accounts.recieve, inventories.sale, prepaid.expenses ) ) >= 1 ) & 
    ( is.null( current.assets )==F ) )
  { stop( "The numerator has been incorrectly specified with conflicting arguments. Ensure you are passing the correct data field to the correct argument." ) }

  if ( ( length( c( accounts.payable, grants.payable ) ) <= 2 ) &
       ( length( c( accounts.payable, grants.payable ) ) >= 1 ) & 
       ( is.null( current.liabilities )==F ) )
  { stop( "The denominator has been incorrectly specified with conflicting arguments. Ensure you are passing the correct data field to the correct argument." ) }
  
  
  
  if ( ( length( c( cash, short.invest, pledges.recieve, accounts.recieve, inventories.sale, prepaid.expenses ) ) < 6 ) ==F & ( is.null( accounts.payable )==F & is.null( grants.payable )==F ) ){
    a <- df[[ cash ]] + df[[ short.invest ]] + df[[ pledges.recieve ]] + df[[ accounts.recieve ]] + df[[ inventories.sale ]] + df[[ prepaid.expenses ]]
  l <- df[[ accounts.payable ]]+ df[[ grants.payable ]] 
  }
  
  else if ( (length( c( cash, short.invest, pledges.recieve, accounts.recieve, inventories.sale, prepaid.expenses ) ) < 6 ) ==F & ( is.null( accounts.payable )==T & is.null( grants.payable )==T & is.null( current.liabilities )==F ) ){
    a <- df[[ cash ]] + df[[ short.invest ]] + df[[ pledges.recieve ]] + df[[ accounts.recieve ]] + df[[ inventories.sale ]] + df[[ prepaid.expenses ]]
  l <- df[[ current.liabilities ]]
  }
  
  else if ( (length( c( cash, short.invest, pledges.recieve, accounts.recieve, inventories.sale, prepaid.expenses ) ) == 0 ) ==T & ( is.null( accounts.payable )==T & is.null( grants.payable )==T & is.null( current.liabilities )==F & is.null( current.assets )==F ) ){
    a <- df[[ current.assets ]]
  l <- df[[ current.liabilities ]] 
  }
  
  else if ( (length( c( cash, short.invest, pledges.recieve, accounts.recieve, inventories.sale, prepaid.expenses ) ) == 0 ) ==T & ( is.null( accounts.payable )==F & is.null( grants.payable )==F & is.null( current.liabilities )==T & is.null( current.assets )==F ) ){
    a <- df[[ current.assets ]]
    l <- df[[ accounts.payable ]]+ df[[ grants.payable ]] 
  }
            
  
  if( winsorize > 1 | winsorize < 0 ){
    stop( 'winsorize argument must be 0 < w < 1' )
  }

  print( paste0('Current liabilities cannot be equal zero: ',sum( l==0 ),' cases have been replaced with NA' ))
  
  l[ l==0 ] <- NA
  
  cr <- a/l
  
  top.p    <- 1 - (1-winsorize)/2
  bottom.p <- 0 + (1-winsorize)/2
  top      <- quantile( cr, top.p, na.rm=T )
  bottom   <- quantile( cr, bottom.p, na.rm=T )
  cr.w    <- cr
  cr.w[ cr.w > top    ] <- top
  cr.w[ cr.w < bottom ] <- bottom
  
  cr.n <- scale( cr.w )
  
  cr.p <- dplyr::ntile( cr, 100 )
  
  CR <- data.frame( cr, cr.w, cr.n, cr.p )
  
  print( summary( CR ) )
  
  par( mfrow=c(2,2) )
  plot( density(cr,   na.rm=T), main="Current Ratio (CR)" )
  plot( density(cr.w, na.rm=T), main="CR Winsorized" )
  plot( density(cr.n, na.rm=T), main="CR Standardized as Z" )
  plot( density(cr.p, na.rm=T), main="CR as Percentile" )
  
  df.cr <- cbind( df, CR )
  return( df.cr )
}


  
