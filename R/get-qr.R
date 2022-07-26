###---------------------------------------------------
###   QUICK RATIO
###---------------------------------------------------

#' @title
#' Quick Ratio
#'
#' @description
#' Calculate the quick ratio and append it to the dataframe. 
#'
#' @param df A \code{data.frame} containing the required field for computing the metric. The metric will be appended to this dataset.
#' @param cash A character string indicating the column name for cash, EOY (On 990: Part X, line 1B; On EZ: Not Available).
#' @param si A character string indicating the column name for short-term investments, EOY (On 990: Part X, line 2B;On EZ: Not Available).
#' @param pr A character string indicating the column name for pledges and grant receivables, EOY On 990: Part X, line 3B; On EZ: Not Available).
#' @param ar A character string indicating the column name for accounts receivables, EOY (On 990: Part X, line 4B; On EZ: Not Available).
#' @param ap A character string indicating the column name for accounts payable, EOY (On 990: Part X, line 17B; On EZ: Not Available).
#' @param gp A character string indicating the column name for grants payable, EOY (On 990: Part X, line 18B; On EZ: Not Available).
#' @param numerator A character string indicating the user-supplied column name for a pre-aggregated variable for the numerator (current assets). Do not combine with numerator column component arguments (`cash`, `si`,`pr`, `ar` ).
#' @param denominator A character string indicating the user-supplied column name for a pre-aggregated variable for the denominator (current liabilities). Do not combine with denominator column component arguments (`ap`, `gp`).
#'
#' @return Object of class \code{data.frame}: the original dataframe appended with the quick ratio (`qr`), 
#' a winsorized version (`qr.w`), a standardized z-score version (`qr.z`), 
#' and a percentile version (`qr.p`).   
#'
#' @details This metric indicates how well an organization can cover its liabilities with its readily 
#' available cash. When an organization has a quick ratio of 1, its quick assets are equal to its current 
#' liabilities. This also indicates that the organization can pay off its current debts without selling its 
#' long-term assets. If an organization has a quick ratio higher than 1, this means that it owns more quick 
#' assets than current liabilities. Note: computation of this metric is available to only 990 filers and not for 990-EZ filers. 
#' The default inputs use column names for variables available only to 990 filers.
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
#' d <- get_qr( df=dat, cash='x1', si='x2', pr='x3', ar='x4', ap='x5', gp='x6', winsorize=0.98 )
#' 
#' head( d )
#' 
#' # run with default column names
#' dat_01 <- dat
#' colnames( dat_01 ) <- c( "F9_10_ASSET_CASH_EOY", "F9_10_ASSET_SAVING_EOY", "F9_10_ASSET_PLEDGE_NET_EOY",
#'                          "F9_10_ASSET_ACC_NET_EOY", "F9_10_LIAB_ACC_PAYABLE_EOY", "F9_10_LIAB_GRANT_PAYABLE_EOY")
#' 
#' 
#' d <- get_qr( dat_01 )
#' 
#' # coerce one column to factor
#' dat_01$F9_10_ASSET_CASH_EOY <- as.factor( dat_01$F9_10_ASSET_CASH_EOY )
#' 
#' d <- get_qr( dat_01 )
#' 
#' # winsorize at 0.025 and 0.975 percentiles instead of 0.01 and 0.99
#' d <- get_qr( df=dat, cash='x1', si='x2', pr='x3', ar='x4', ap='x5', gp='x6', winsorize = 0.95 )
#' 
#' d <- get_qr( dat_01, winsorize = 0.95 )
#' 
#' # aggregate variables into single numerator and denominator
#' x.den <- x5 + x6
#' x.num <- x1 + x2 + x3 + x4
#' 
#' dat_02 <- cbind( dat, x.den, x.num)
#' 
#' 
#' # using 990 data
#' load( '/Volumes/My Passport for Mac/Urban Institute/Summer Projects/Fiscal/fiscal/R/sysdata.rda' )
#' d <- get_qr( df = part010810 )
#' 
#' # now coerce one of the variables to numeric
#' part010810$F9_10_LIAB_ACC_PAYABLE_EOY <- as.character( part010810$F9_10_LIAB_ACC_PAYABLE_EOY )
#' 
#' d <- get_qr( df = part010810 )
#' 
#' # incorrectly specify denominator
#' get_qr( df = dat_02, cash = 'x1', si = 'x2', pr = 'x3', ar = 'x4', 
#'         ap = NULL, gp = 'x6', numerator = NULL, denominator = NULL, winsorize=0.98 )
#' 
#' # incorrectly specify numerator with conflicting arguments
#' get_qr( df = dat_02, cash = 'x1', si = 'x2', pr = 'x3', ar = 'x4', 
#'         ap = NULL, gp = NULL, numerator = 'x.num', denominator = 'x.den' , winsorize=0.98 )
#' 
#' 
#' # incorrectly specify numerator with conflicting arguments
#' get_qr( df = dat_02, cash = 'x1', si = 'x2', pr = 'x3', ar = 'x4', 
#'         ap = 'x5', gp = 'x6', numerator = NULL, denominator = 'x.den' , winsorize=0.98 )  
#' 
#' # supplying no arguments for the numerator
#' get_qr( df = dat_02, cash = NULL, si = NULL, pr = NULL, ar = NULL, 
#'         ap = 'x5', gp = 'x6', numerator = NULL, denominator = NULL , winsorize=0.98 )  
#' 
#' # supplying no arguments for the denominator
#' get_qr( df = dat_02, cash = 'x1', si = 'x2', pr = 'x3', ar = 'x4', 
#'         ap = NULL, gp = NULL, numerator = NULL, denominator = NULL , winsorize=0.98 )  
#' 
#' # supplying argument for one of the parameters in the numerator that is greater than length 1
#' get_qr( df = dat_02, cash = 'x1', si = 'x2', pr = 'x3', ar = 'x4', 
#'         ap = c( 'x5', 'x6' ), gp = 'x6', numerator = NULL, denominator = NULL, winsorize=0.98 )
#' 
#' get_qr( df = dat_02, cash = NULL, si = NULL, pr = NULL, ar = NULL, 
#'         ap = NULL, gp = NULL, numerator = c( 'x5', 'x6' ), denominator = 'x.den' , winsorize=0.98 )  
#' 
#' @export
get_qr <- function( df, 
                    cash = "F9_10_ASSET_CASH_EOY", 
                    si = "F9_10_ASSET_SAVING_EOY", 
                    pr = "F9_10_ASSET_PLEDGE_NET_EOY",
                    ar = "F9_10_ASSET_ACC_NET_EOY", 
                    ap = "F9_10_LIAB_ACC_PAYABLE_EOY",
                    gp = "F9_10_LIAB_GRANT_PAYABLE_EOY",
                    numerator = NULL,
                    denominator = NULL,
                    winsorize=0.98 )
{
  
  # checks
  if( winsorize > 1 | winsorize < 0 )
  { stop( "winsorize argument must be 0 < w < 1" ) }
  
  if ( ( ( length( c( cash, si, pr, ar ) ) < 4 )==F | is.null( numerator )==F ) &
       ( ( is.null( ap )==T | is.null( gp )==T ) &
         is.null( denominator )==T ) )
  { stop( "The denominator has been incorrectly specified. Ensure you are passing the correct data field to the correct argument." ) }
  
  if ( ( ( length( c( ap, gp ) ) < 2 )==F | is.null( denominator )==F ) &
       ( ( is.null( cash )==T | is.null( si )==T | 
           is.null( pr )==T | is.null( ar )==T ) &
         is.null( numerator )==T ) )
  { stop( "The numerator has been incorrectly specified. Ensure you are passing the correct data field to the correct argument." ) }

    
  if ( ( length( c( cash, si, pr, ar ) ) <= 4 ) &
       ( length( c( cash, si, pr, ar ) ) >= 1 ) & 
       ( is.null( numerator )==F ) )
  { stop( "The numerator has been incorrectly specified with conflicting arguments. Ensure you are passing the correct data field to the correct argument." ) }
  
  if ( ( length( c( ap, gp ) ) <= 2 ) &
       ( length( c( ap, gp ) ) >= 1 ) & 
       ( is.null( denominator )==F ) )
  { stop( "The denominator has been incorrectly specified with conflicting arguments. Ensure you are passing the correct data field to the correct argument." ) }
  
  if( ( length( cash ) > 1 | length( cash ) < 1 ) & is.null( numerator ) == T ) 
  { stop( "`cash` must be a single quoted string or a vector with a maximum length of one." ) }
  
  if( ( length( si ) > 1 | length( si ) < 1 ) & is.null( numerator ) == T ) 
  { stop( "`si` must be a single quoted string or a vector with a maximum length of one." ) }
  
  if( ( length( pr ) > 1 | length( pr ) < 1 ) & is.null( numerator ) == T ) 
  { stop( "`pr` must be a single quoted string or a vector with a maximum length of one." ) }
  
  if( ( length( ar ) > 1 | length( ar ) < 1 ) & is.null( numerator ) == T ) 
  { stop( "`ar` must be a single quoted string or a vector with a maximum length of one." ) }
  
  if( ( length( ap ) > 1 | length( ap ) < 1 ) & is.null( denominator ) == T ) 
  { stop( "`ap` must be a single quoted string or a vector with a maximum length of one." ) }

  if( ( length( gp ) > 1 | length( gp ) < 1 ) & is.null( denominator ) == T ) 
  { stop( "`gp` must be a single quoted string or a vector with a maximum length of one." ) }
  
  if( ( length( numerator ) > 1 & length( c( cash, si, pr, ar ) ) == 0 ) )
  { stop( "`numerator` must be a single quoted string or a vector with a maximum length of one." ) }
  
  if( ( length( denominator ) > 1 & length( c( ap, gp ) ) == 0 ) )
  { stop( "`denominator` must be a single quoted string or a vector with a maximum length of one." ) }
  
  
  
  # copy data
  dat <- df
  
  ## ensure variable classes are numeric ##
  
  # run coerce_numeric and loop through all variables required and that matched by the two input arguments
  v <- c( colnames( dat )[which( colnames( dat ) %in% cash ) ], colnames( dat )[which( colnames( dat ) %in% si )],
          colnames( dat )[which( colnames( dat ) %in% ar )], colnames( dat )[which( colnames( dat ) %in% pr )], 
          colnames( dat )[which( colnames( dat ) %in% ap )],colnames( dat )[which( colnames( dat ) %in% gp )])
  
  dat <- coerce_numeric( d = dat, vars = v )
  
  
  num <- dat[[ cash ]] + dat[[ si ]] + dat[[ pr ]] + dat[[ ar ]]
  den <- dat[[ ap ]] + dat[[ gp ]]
  
  # can't divide by zero
  print( paste0( "Payables cannot be zero: ", sum( den==0, na.rm = T ), " cases have been replaced with NA." ) )
  den[ den == 0 ] <- NA 
  
  qr <- num / den
  
  top.p    <- 1 - (1-winsorize)/2
  bottom.p <- 0 + (1-winsorize)/2
  top      <- quantile( qr, top.p, na.rm=T )
  bottom   <- quantile( qr, bottom.p, na.rm=T )
  qr.w    <- qr
  qr.w[ qr.w > top    ] <- top
  qr.w[ qr.w < bottom ] <- bottom
  
  qr.n <- scale( qr.w )
  
  qr.p <- dplyr::ntile( qr, 100 )
  
  QR <- data.frame( qr, qr.w, qr.n, qr.p )
  
  print( summary( QR ) )
  
  par( mfrow=c(2,2) )
  plot( density( qr,   na.rm=T ), main="Quick Ratio (QR)" )
  plot( density( qr.w, na.rm=T ), main="QR Winsorized" )
  plot( density( qr.n, na.rm=T ), main="QR Standardized as Z" )
  plot( density( qr.p, na.rm=T ), main="QR as Percentile" )
  
  df.qr <- data.frame( cbind( df, QR ) )
  return( df.qr )
}
