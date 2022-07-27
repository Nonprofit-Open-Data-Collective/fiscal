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
#' @param df A \code{data.frame} containing the required field for computing the metric. The metric will be appended to this dataset.
#' @param cash A character string indicating the column name for cash, EOY (On 990: Part X, line 1B; On EZ:Part I, line 22 (cash and short-term investments only)) with the default name supplied.
#' @param short.invest A character string indicating the column name for short-term investments, EOY (On 990: Part X, line 2B; On EZ:Part I, line 22 (cash and short-term investments only)) with the default name supplied.
#' @param pledges.receive A character string indicating the column name for pledges and grant receivables, EOY On 990: Part X, line 3B; On EZ: Not Available) with the default name supplied.
#' @param accounts.receive A character string indicating the column name for accounts receivables, EOY (On 990: Part X, line 4B; On EZ: Not Available) with the default name supplied.
#' @param inventories.sale A character string indicating the column name for inventories for sale or use, EOY (On 990: Part X, line 8B; On EZ: Not Available) with the default name supplied.
#' @param prepaid.expense A character string indicating the column name for prepaid expenses and deferred charges, EOY (On 990: Part X, line 9B; On EZ: Not Available) with the default name supplied.
#' @param accounts.payable A character string indicating the column name for accounts payable, EOY (On 990: (Part X, line 17B); On EZ: Not available) with the default name supplied.
#' @param grants.payable A character string indicating the column name for grants payable, EOY (On 990: (Part X, line 18B); On EZ: Not available) with the default name supplied.
#' @param numerator A character string indicating the user-supplied column name for a pre-aggregated variable for the numerator (current assets). Do not combine with numerator column component arguments (`cash`, `short.invest`,`pledges.receive`, `accounts.receive`, `inventories.sale`, `prepaid.expense`).
#' @param denominator A character string indicating the user-supplied column name for a pre-aggregated variable for the denominator (current liabilities). Do not combine with denominator column component arguments (`accounts.payable`, `grants.payable`).
#' @param winsorize The winsorization value (between 0 and 1), defaults to 0.98 which winsorizes at 99th and 1st percentile values.   
#' 
#' @return Object of class \code{data.frame}: the original dataframe with the current ratio (`cr`),
#' a winsorized version (`cr.w`), a standardized z-score version (`cr.z`), 
#' and a percentile version (`cr.p`).  
#'  
#' @details The current ratio is used to measure the overall liquidity of a nonprofit organization.
#' In its simplest form, it shows how many dollars of current assets an organization has to cover its 
#' current obligations. The higher the ratio, the more liquid the organization.As a rule of thumb, organizations 
#' should strive for a current ratio of 1.0 or higher. An organization with a ratio of 1.0 would have one dollar 
#' of assets to pay for every dollar of current liabilities. Note: computation of this metric is available to only 990 filers
#' not for 990-EZ filers. The default inputs use column names for variables available only to 990 filers.
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
#' 
#' 
#' dat <- data.frame( x1,x2, x3, x4, x5, x6, x7, x8)
#' 
#' # specify own column names
#' d <- get_cr( df=dat, cash='x1', short.invest='x2', pledges.receive='x3', 
#'              accounts.receive='x4', inventories.sale='x5', prepaid.expenses = 'x6', 
#'              accounts.payable = 'x7', grants.payable = 'x8', winsorize=0.98 )
#' 
#' head( d )
#' 
#' # run with default column names
#' dat_01 <- dat
#' 
#' colnames( dat_01 ) <- c( "F9_10_ASSET_CASH_EOY", "F9_10_ASSET_SAVING_EOY", "F9_10_ASSET_PLEDGE_NET_EOY",
#'                          "F9_10_ASSET_ACC_NET_EOY", "F9_10_ASSET_INV_SALE_EOY", "F9_10_ASSET_EXP_PREPAID_EOY",
#'                          "F9_10_LIAB_ACC_PAYABLE_EOY", "F9_10_LIAB_GRANT_PAYABLE_EOY")
#' 
#' 
#' d <- get_cr( dat_01 )
#' 
#' # coerce one column to factor
#' dat_01$F9_10_ASSET_ACC_NET_EOY <- as.factor( dat_01$F9_10_ASSET_ACC_NET_EOY )
#' 
#' d <- get_cr( dat_01 )
#' 
#' # winsorize at 0.025 and 0.975 percentiles instead of 0.01 and 0.99
#' d <- get_cr( df=dat, cash='x1', short.invest='x2', pledges.receive='x3', 
#'              accounts.receive='x4', inventories.sale='x5', prepaid.expenses = 'x6', 
#'              accounts.payable = 'x7', grants.payable = 'x8', winsorize=0.95 )
#' 
#' d <- get_cr( dat_01, winsorize = 0.95 )
#' 
#' # aggregate variables into single numerator and denominator
#' x.den <- x7 + x8
#' x.num <- x1 + x2 + x3 + x4 + x5 + x6
#' 
#' dat_02 <- cbind( dat, x.den, x.num)
#' 
#' d <- get_cr( dat_02, numerator = "x.num", denominator = "x.den" )
#' 
#' # using 990 data
#' load( '/Volumes/My Passport for Mac/Urban Institute/Summer Projects/Fiscal/fiscal/R/sysdata.rda' )
#' d <- get_cr( df = part010810 )
#' 
#' # now coerce one of the variables to numeric
#' part010810$F9_10_ASSET_INV_SALE_EOY <- as.character( part010810$F9_10_ASSET_INV_SALE_EOY )
#' 
#' d <- get_cr( df = part010810 )
#' 
#' # incorrectly specify denominator
#' get_cr( df = dat_02, cash = 'x1', short.invest = 'x2', pledges.receive = 'x3', accounts.receive = 'x4', 
#'         inventories.sale = 'x5', prepaid.expenses = 'x6', accounts.payable = NULL, grants.payable = NULL, 
#'         numerator = NULL, denominator = NULL, winsorize=0.98 )
#' 
#' # incorrectly specify numerator with conflicting arguments
#' get_cr( df = dat_02, cash = 'x1', short.invest = 'x2', pledges.receive = 'x3', accounts.receive = 'x4', 
#'         inventories.sale = 'x5', prepaid.expenses = 'x6', accounts.payable = 'x7', grants.payable = 'x8', 
#'         numerator = 'x.num', denominator = NULL, winsorize=0.98 )
#' 
#' # incorrectly specify denominator with conflicting arguments
#' get_cr( df = dat_02, cash = 'x1', short.invest = 'x2', pledges.receive = 'x3', accounts.receive = 'x4', 
#'         inventories.sale = 'x5', prepaid.expenses = 'x6', accounts.payable = 'x7', grants.payable = 'x8', 
#'         numerator = NULL, denominator = 'x.den', winsorize=0.98 )
#' 
#' # supplying no arguments for the numerator
#' get_cr( df = dat_02, cash = NULL, short.invest = NULL, pledges.receive = NULL, accounts.receive = NULL, 
#'         inventories.sale = NULL, prepaid.expenses = NULL, accounts.payable = 'x7', grants.payable = 'x8', 
#'         numerator = NULL, denominator = NULL, winsorize=0.98 )
#' 
#' # supplying no arguments for the denominator
#' get_cr( df = dat_02, cash = 'x1', short.invest = 'x2', pledges.receive = 'x3', accounts.receive = 'x4', 
#'         inventories.sale = 'x5', prepaid.expenses = 'x6', accounts.payable = NULL, grants.payable = NULL, 
#'         numerator = NULL, denominator = NULL, winsorize=0.98 ) 
#' 
#' # supplying argument for one of the parameters in the numerator that is greater than length 1
#' get_cr( df = dat_02, cash = 'x1', short.invest = 'x2', pledges.receive = 'x3', accounts.receive = 'x4', 
#'         inventories.sale = c( 'x5', 'x6' ), prepaid.expenses = 'x6', accounts.payable = 'x7', grants.payable = 'x8', 
#'         numerator = NULL, denominator = NULL, winsorize=0.98 )
#' 
#' get_cr( df = dat_02, cash = 'x1', short.invest = 'x2', pledges.receive = 'x3', accounts.receive = 'x4', 
#'         inventories.sale = 'x5', prepaid.expenses = 'x6', accounts.payable = c( 'x7', 'x8' ), grants.payable = 'x8', 
#'         numerator = NULL, denominator = NULL, winsorize=0.98 ) 
#'@export
get_cr <- function( df, cash = 'F9_10_ASSET_CASH_EOY', 
                  short.invest = 'F9_10_ASSET_SAVING_EOY', 
                  pledges.receive = 'F9_10_ASSET_PLEDGE_NET_EOY', 
                  accounts.receive = 'F9_10_ASSET_ACC_NET_EOY', 
                  inventories.sale = 'F9_10_ASSET_INV_SALE_EOY', 
                  prepaid.expenses = 'F9_10_ASSET_EXP_PREPAID_EOY', 
                  accounts.payable = 'F9_10_LIAB_ACC_PAYABLE_EOY', 
                  grants.payable = 'F9_10_LIAB_GRANT_PAYABLE_EOY', 
                  numerator = NULL, denominator = NULL, winsorize=0.98 )
{
  
  # checks
  if( winsorize > 1 | winsorize < 0 )
  { stop( "winsorize accounts.receivegument must be 0 < w < 1" ) }
  
  if( is.null( cash )==F & is.null( short.invest )==F & is.null( pledges.receive )==F & is.null( accounts.receive )==F &
      is.null( inventories.sale )==F & is.null( prepaid.expenses )==F & is.null( accounts.payable )==F &
      is.null( grants.payable )==F ) {
    
    if( cash == "F9_10_ASSET_CASH_EOY" & short.invest == "F9_10_ASSET_SAVING_EOY" &
        pledges.receive == "F9_10_ASSET_PLEDGE_NET_EOY" & accounts.receive == "F9_10_ASSET_ACC_NET_EOY" & 
        accounts.payable == "F9_10_LIAB_ACC_PAYABLE_EOY" & inventories.sale == "F9_10_ASSET_INV_SALE_EOY" &
        prepaid.expenses == "F9_10_ASSET_EXP_PREPAID_EOY" &
        grants.payable == "F9_10_LIAB_GRANT_PAYABLE_EOY" & is.null( numerator )==F & is.null( denominator )==F ) {
      
      warning( "Default argument inputs overridden with specified numerator and denominator arguments" ) 
      
      cash <- NULL
      short.invest <- NULL
      pledges.receive <- NULL
      accounts.receive <- NULL
      inventories.sale <- NULL
      prepaid.expenses <- NULL
      accounts.payable <- NULL
      grants.payable <- NULL
      
    }
    
  } 

  if ( ( ( length( c( cash, short.invest, pledges.receive, accounts.receive, inventories.sale, prepaid.expenses ) ) < 6 )==F | is.null( numerator )==F ) &
       ( ( is.null( accounts.payable )==T & is.null( grants.payable )==T ) &
         is.null( denominator )==T ) )
  { stop( "The denominator has been incorrectly specified. Ensure you are passing the correct data field to the correct argument." ) }

  if ( ( ( length( c( accounts.payable, grants.payable ) ) < 2 )==F | is.null( denominator )==F ) &
       ( ( is.null( cash )==T | is.null( short.invest )==T | 
           is.null( pledges.receive )==T | is.null( accounts.receive )==T |
           is.null( inventories.sale )==T | is.null( prepaid.expenses )==T ) &
         is.null( numerator )==T ) )
  { stop( "The numerator has been incorrectly specified. Ensure you are passing the correct data field to the correct argument." ) }
  

  if ( ( length( c( cash, short.invest, pledges.receive, accounts.receive, inventories.sale, prepaid.expenses ) ) <= 6 ) &
       ( length( c( cash, short.invest, pledges.receive, accounts.receive, inventories.sale, prepaid.expenses ) ) >= 1 ) & 
       ( is.null( numerator )==F ) )
  { stop( "The numerator has been incorrectly specified with conflicting arguments. Ensure you accounts.receivee passhort.investng the correct data field to the correct argument." ) }
  
  if ( ( length( c( accounts.payable, grants.payable ) ) >= 2 ) & 
       ( is.null( denominator )==F ) )
  { stop( "The denominator has been incorrectly specified with conflicting arguments. Ensure you accounts.receivee passhort.investng the correct data field to the correct argument." ) }
  
  if( ( length( cash ) > 1 | length( cash ) < 1 ) & is.null( numerator ) == T ) 
  { stop( "`cash` must be a single quoted string or a vector with a maximum length of one." ) }
  
  if( ( length( short.invest ) > 1 | length( short.invest ) < 1 ) & is.null( numerator ) == T ) 
  { stop( "`short.invest` must be a single quoted string or a vector with a maximum length of one." ) }
  
  if( ( length( pledges.receive ) > 1 | length( pledges.receive ) < 1 ) & is.null( numerator ) == T ) 
  { stop( "`pledges.receive` must be a single quoted string or a vector with a maximum length of one." ) }
  
  if( ( length( accounts.receive ) > 1 | length( accounts.receive ) < 1 ) & is.null( numerator ) == T ) 
  { stop( "`accounts.receive` must be a single quoted string or a vector with a maximum length of one." ) }
  
  if( ( length( inventories.sale ) > 1 | length( inventories.sale ) < 1 ) & is.null( numerator ) == T ) 
  { stop( "`inventories.sale` must be a single quoted string or a vector with a maximum length of one." ) }
  
  if( ( length( prepaid.expenses ) > 1 | length( prepaid.expenses ) < 1 ) & is.null( numerator ) == T ) 
  { stop( "`prepaid.expenses` must be a single quoted string or a vector with a maximum length of one." ) }
  
  if( ( length( accounts.payable ) > 1 | length( accounts.payable ) < 1 ) & is.null( denominator ) == T ) 
  { stop( "`accounts.payable` must be a single quoted string or a vector with a maximum length of one." ) }
  
  if( ( length( grants.payable ) > 1 | length( grants.payable ) < 1 ) & is.null( denominator ) == T ) 
  { stop( "`accounts.payable` must be a single quoted string or a vector with a maximum length of one." ) }
  
  if( ( length( numerator ) > 1 & length( c( cash, short.invest, pledges.receive, accounts.receive ) ) == 0 ) )
  { stop( "`numerator` must be a single quoted string or a vector with a maximum length of one." ) }
  
  if( ( length( denominator ) > 1 & length( c( accounts.payable ) ) == 0 ) )
  { stop( "`denominator` must be a single quoted string or a vector with a maximum length of one." ) }
  
  
  
  # copy data
  dat <- df
  
  ## ensure variable classes are numeric ##
  
  # run coerce_numeric and loop through all variables required and that matched by the two input arguments
  v <- c( colnames( dat )[which( colnames( dat ) %in% cash ) ], colnames( dat )[which( colnames( dat ) %in% short.invest )],
          colnames( dat )[which( colnames( dat ) %in% accounts.receive )], colnames( dat )[which( colnames( dat ) %in% pledges.receive )], 
          colnames( dat )[which( colnames( dat ) %in% inventories.sale )], colnames( dat )[which( colnames( dat ) %in% prepaid.expenses )],
          colnames( dat )[which( colnames( dat ) %in% accounts.payable )], colnames( dat )[which( colnames( dat ) %in% grants.payable )],
          colnames( dat )[which( colnames( dat ) %in% numerator )], colnames( dat )[which( colnames( dat ) %in% denominator )] )
  
  dat <- coerce_numeric( d = dat, vars = v )
  
  if( is.null( numerator) == T & is.null( denominator ) == T ){
    
    num <- dat[[ cash ]] + dat[[ short.invest ]] + dat[[ pledges.receive ]] + dat[[ accounts.receive ]]+ dat[[ inventories.sale ]] + dat[[ prepaid.expenses ]]
    den <- dat[[ accounts.payable ]] + dat[[ grants.payable ]]
    
  }
  
  if( is.null( numerator) == F & is.null( denominator ) == T ){
    
    num <- dat[[ numerator ]]
    den <- dat[[ accounts.payable ]] + dat[[ grants.payable ]] 
    
  }
  
  if( is.null( numerator) == T & is.null( denominator ) == F ){
    
    num <- dat[[ cash ]] + dat[[ short.invest ]] + dat[[ pledges.receive ]] + dat[[ accounts.receive ]]+ dat[[ inventories.sale ]] + dat[[ prepaid.expenses ]]
    den <- dat[[ denominator ]] 
    
  }
  
  if( is.null( numerator) == F & is.null( denominator ) == F ){
    
    num <- dat[[ numerator ]]
    den <- dat[[ denominator ]] 
    
  }
  
  # can't divide by zero
  print( paste0( "Payables cannot be zero: ", sum( den==0, na.rm = T ), " cases have been replaced with NA." ) )
  den[ den == 0 ] <- NA 
  
  cr <- num / den
  
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
  plot( density( cr,   na.rm=T ), main="Current Ratio (CR)" )
  plot( density( cr.w, na.rm=T ), main="CR Winsorized" )
  plot( density( cr.n, na.rm=T ), main="CR Standardized as Z" )
  plot( density( cr.p, na.rm=T ), main="CR as Percentile" )
  
  df.cr <- data.frame( cbind( df, CR ) )
  return( df.cr )
}


  
