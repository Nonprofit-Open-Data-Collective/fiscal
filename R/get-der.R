###---------------------------------------------------
###   DEBT TO EQUITY RATIO
###---------------------------------------------------

#' @title
#' Debt to Equity Ratio 
#'
#' @description
#' Calculate the debt to equity ratio and append it to the dataframe. 
#'
#' @param df A \code{data.frame} containing the required field for computing the metric. The metric will be appended to this dataset.
#' @param debt A character string indicating the column name for total debt, EOY (On 990: Part X, line 17B; On EZ: Not Available) with the default name supplied.
#' @param equity A character string indicating the column name for unrestricted net assets, EOY (On 990: Part X, line 27B; On EZ: Not Available) with the default name supplied.
#' @param winsorize The winsorization value (between 0 and 1), defaults to 0.98 which winsorizes at 99th and 1st percentile values.   
#' 
#' @usage get_der( df, debt = 'F9_10_LIAB_ACC_PAYABLE_EOY', equity = 'F9_10_NAFB_UNRESTRICT_EOY', winsorize=0.98 )
#' 
#' @return Object of class \code{data.frame}: the original dataframe appended with the debt to equity ratio (`der`), 
#'  a winsorized version (`der.w`), a standardized z-score version (`der.z`), 
#'  and a percentile version (`der.p`).   
#'
#' @details Debt to Equity Ratio is a a leverage ratio that defines the total amount of debt relative to the 
#' unrestricted assets owned by an organization.This metric shows the big picture view of how much an organization 
#' owes relative to what it owns.Higher values mean it is more highly leveraged, meaning it has low capacity for future 
#' borrowing. As an example: if an organization has a total-debt-to-net-assets ratio of 0.4, 40% of its assets 
#' are financed by creditors, and 60% are its own unrestricted, available equity. As this percentage 
#' creeps above the 50% mark, it can call into question the organization’s ability to manage debt, 
#' which could jeopardize the delivery of programs and services. However, for developers, extremely 
#' low values may mean the organization is not capitalizing enough on its equity to expand.
#' This value can be negative if an organization has either overpaid on its debts or if it has negative 
#' unrestricted net assets.One limitation of this metric is that it does not provide any indication of 
#' asset quality or liquidity since it lumps tangible and intangible assets together. Note: computation of this metric 
#' is available to only 990 filers and not for 990-EZ filers. The default inputs use column names for variables 
#' available only to 990 filers. Note: This ratio can be interchanged with total liabilities over total net assets 
#' (which should be comparable for EZ filers and full 990 filers), but for Community Development Corporations, the 
#' more important metric is unrestricted net assets, which isn’t available for EZ filers.
#' 
#' @examples
#' x1 <- rnorm( 1000, 100, 30 )
#' x2 <- rnorm( 1000, 200, 30 )
#' x2[ c( 15, 300, 600 ) ] <- 0
#' 
#' dat <- data.frame( x1, x2 )
#' 
#' # specify own column names
#' d <- get_der( df = dat, debt = "x1", equity = "x2" )
#' 
#' 
#' head( d )
#' 
#' # run with default column names
#' x1[ seq( from = 1, to = 1000, 50 ) ] <- NA
#' x2[ seq( from = 1, to = 1000, 71 ) ] <- NA
#' 
#' dat_01 <- data.frame( x1, x2 )
#' 
#' colnames( dat_01 ) <- c( "F9_10_LIAB_ACC_PAYABLE_EOY", "F9_10_NAFB_UNRESTRICT_EOY" )
#' 
#' d <- get_der( dat_01 )
#' 
#' # coerce one column to factor
#' dat_01$F9_10_LIAB_ACC_PAYABLE_EOY <- as.factor( dat_01$F9_10_LIAB_ACC_PAYABLE_EOY )
#' 
#' d <- get_der( dat_01 )
#' 
#' # winsorize at 0.025 and 0.975 percentiles instead of 0.01 and 0.99
#' d <- get_der( df = dat, debt = "x1", equity = "x2", winsorize = 0.95 )
#' 
#' d <- get_der( dat_01, winsorize = 0.95 )
#' 
#' 
#' # using 990 data
#' load( '/Volumes/My Passport for Mac/Urban Institute/Summer Projects/Fiscal/fiscal/R/sysdata.rda' )
#' d <- get_der( df = part010810 )
#' 
#' # now coerce one of the variables to numeric
#' part010810$F9_01_EXP_TOT_CY <- as.character( part010810$F9_01_EXP_TOT_CY )
#' 
#' d <- get_der( df = part010810 )
#' 
#' 
#' ## Errors ##
#' 
#' # numerator not specified
#' d <- get_der( df = dat, debt = NULL, equity = "x2" )
#' 
#' # denominator not specified
#' d <- get_der( df = dat, debt = "x1", equity = NULL )
#' 
#' # neither numerator nor denominator specified
#' d <- get_der( df = dat, debt = NULL, equity = NULL )
#' 
#' # column names vector not of correct length
#' d <- get_der( df = dat, debt = c("e","b","c"), equity = "e")
#' 
#' # column names vector not of correct length
#' d <- get_der( df = dat, debt = "e", equity = c( "e", "b", "c") )
#' @export
get_der <- function( df, 
                     debt = "F9_10_LIAB_ACC_PAYABLE_EOY", 
                     equity = "F9_10_NAFB_UNRESTRICT_EOY", 
                     winsorize=0.98 )
{
  
  # function checks
  if( winsorize > 1 | winsorize < 0 )
  { stop( "winsorize argument must be 0 < w < 1" ) }
  
  if( is.null( debt )==T & is.null( equity )==F )
  { stop( "The numerator has been incorrectly specified. Ensure you are passing the correct data field to the correct argument." ) }
  
  if( is.null( debt )==F & is.null( equity )==T )
  { stop( "The denominator has been incorrectly specified. Ensure you are passing the correct data field to the correct argument." ) }
  
  if( is.null( debt )==T & is.null( equity )==T )
  { stop( "The argument fields are empty. Please supply column names for each argument or execute the function with default inputs." ) }
  
  if( length( debt ) > 2 | length( debt ) < 1 )
  { stop( "`debt` must be a single quoted string or a vector with a minimum length of one and maximum length of two." ) }
  
  if( length( equity ) > 2 | length( equity ) < 1 )
  { stop( "`equity` must be a single quoted string or a vector with a minimum length of one and maximum length of two." ) }
  
  
  # copy data
  dat <- df
  
  ## ensure variable classes are numeric ##
  
  # run coerce_numeric and loop through all variables required and that matched by the two input arguments
  v <- c( colnames( dat )[which( colnames( dat ) %in% equity ) ], colnames( dat )[which( colnames( dat ) %in% debt )] )
  dat <- coerce_numeric( d = dat, vars = v )
  
  
  # check to ensure both sets of variable names are included in input data when not specifying column names.
  # edge cases
  
  # BEGIN first outer conditional
  # BEGIN first outer conditional
  if ( sum( debt %in% c( "F9_10_LIAB_ACC_PAYABLE_EOY") )==2 & sum( equity %in% c( "F9_10_NAFB_UNRESTRICT_EOY") )==2 ) {
    
    # BEGIN series of nested conditionals 
    
    
    # if at least one of the columns in missing from the input dataset, use only the column that is present
    if ( length( which( colnames( dat ) %in% debt ) )==1 | length( which( colnames( dat ) %in% equity ) )==1 ) {
      
      if ( length( which(colnames( dat ) %in% debt ) )==2 ){
        
        # create a column that concatenates two numerator variables into single column if both columns for numerator are present
        dat[ which( is.na( dat[ debt[2] ] )==F ), "d"] <- dat[ which( is.na( dat[ debt[2] ] )==F ), debt[2] ] 
        dat[ which( is.na( dat[ debt[1] ] )==F ), "d"] <- dat[ which( is.na( dat[ debt[1] ] )==F ), debt[1] ] 
      }
      # if at least one of the numerator columns in missing from the input dataset, use only the column that is present
      if ( length( which( colnames( dat ) %in% debt ) )==1 ){
        
        this <- which( colnames( dat ) %in% debt )
        
        dat[ , "d"] <- dat[ , this ]
      }
      
      
      if ( length( which( colnames( dat ) %in% equity ) )==2 ){
        # create a column that concatenates two numerator variables into single column if both columns for denominator are present
        dat[ which( is.na( dat[ equity[2] ] )==F ), "e"] <- dat[ which( is.na( dat[ equity[2] ] )==F ), equity[2] ] 
        dat[ which( is.na( dat[ equity[1] ] )==F ), "e"] <- dat[ which( is.na( dat[ equity[1] ] )==F ), equity[1] ] 
      }
      # if at least one of the denominator columns in missing from the input dataset, use only the column that is present
      if ( length( which( colnames( dat ) %in% equity ))==1  ){
        
        this <- which( colnames( dat ) %in% equity )
        
        dat[ , "e"] <- dat[ , this ]
      }
      # ENp series of nestep conditionals 
      
      # now, assign p anp e, respectively
      d <- dat[[ "d"]]
      e <- dat[[ "e"]]
    }
    
    # if all four columns are present, exit conditional
    if ( length( debt )==2 & length( equity )==2 & length(which( colnames( dat ) %in% debt ) )==2 & length(which( colnames( dat ) %in% equity ) )==2 ) {
      
      # create a column that concatenates two numerator variables into single column
      dat[ which( is.na( dat[ debt[2] ] )==F ), "d"] <- dat[ which( is.na( dat[ debt[2] ] )==F ), debt[2] ]
      dat[ which( is.na( dat[ debt[1] ] )==F ), "d"] <- dat[ which( is.na( dat[ debt[1] ] )==F ), debt[1] ]
      
      # create a column that concatenates two denominator variables into single column
      dat[ which( is.na( dat[ equity[2] ] )==F ), "e"] <- dat[ which( is.na( dat[ equity[2] ] )==F ), equity[2] ]
      dat[ which( is.na( dat[ equity[1] ] )==F ), "e"] <- dat[ which( is.na( dat[ equity[1] ] )==F ), equity[1] ]
      
      
      d <- dat[[ "d"]]
      e <- dat[[ "e"]]
    }
  }
  # END first outer conditional
  
  
  
  # all other cases are nestep in the following conditionals
  
  if ( sum( debt %in% c( "F9_10_LIAB_ACC_PAYABLE_EOY") )!=2 & sum( equity %in% c( "F9_10_NAFB_UNRESTRICT_EOY") )!=2 ){          
    
    if ( length( debt )==2 & length( equity )==2 ) {
      
      # create a column that concatenates two numerator variables into single column
      dat[ which( is.na( dat[ debt[2] ] )==F ), "d"] <- dat[ which( is.na( dat[ debt[2] ] )==F ), debt[2] ]
      dat[ which( is.na( dat[ debt[1] ] )==F ), "d"] <- dat[ which( is.na( dat[ debt[1] ] )==F ), debt[1] ]
      
      # create a column that concatenates two denominator variables into single column
      dat[ which( is.na( dat[ equity[2] ] )==F ), "e"] <- dat[ which( is.na( dat[ equity[2] ] )==F ), equity[2] ]
      dat[ which( is.na( dat[ equity[1] ] )==F ), "e"] <- dat[ which( is.na( dat[ equity[1] ] )==F ), equity[1] ]
      
      
      d <- dat[[ "d"]]
      e <- dat[[ "e"]]
    }
    
    else if ( length( debt )==2 & length( equity )==1 ) {
      
      # create a column that concatenates two denominator variables into single column
      dat[ which( is.na( dat[ debt[2] ] )==F ), "d"] <- dat[ which( is.na( dat[ debt[2] ] )==F ), debt[2] ]
      dat[ which( is.na( dat[ debt[1] ] )==F ), "d"] <- dat[ which( is.na( dat[ debt[1] ] )==F ), debt[1] ]
      
      
      d <- dat[[ "d"]]
      e <- dat[[ equity ]]
    }
    
    else if ( length( debt )==1 & length( equity )==2 ) {
      
      # create a column that concatenates two numerator variables into single column
      dat[ which( is.na( dat[ equity[2] ] )==F ), "e"] <- dat[ which( is.na( dat[ equity[2] ] )==F ), equity[2] ]
      dat[ which( is.na( dat[ equity[1] ] )==F ), "e"] <- dat[ which( is.na( dat[ equity[1] ] )==F ), equity[1] ]
      
      
      d <- dat[[ debt ]]
      e <- dat[[ "e"]]
    }
    
    else if ( length( debt )==1 & length( equity )==1 ) {
      
      d <- dat[[ debt ]]
      e <- dat[[ equity ]]
    }
    
    
  }    

  # can't divide by zero
  print( paste0( "Equity cannot be equal to zero: ", sum( e==0, na.rm = T ), " cases have been replaced with NA." ) )
  e[ e == 0 ] <- NA 
  
  der <- d / e
  
  top.p    <- 1 - (1-winsorize)/2
  bottom.p <- 0 + (1-winsorize)/2
  top      <- quantile( der, top.p, na.rm=T )
  bottom   <- quantile( der, bottom.p, na.rm=T )
  der.w    <- der
  der.w[ der.w > top    ] <- top
  der.w[ der.w < bottom ] <- bottom
  
  der.n <- scale( der.w )
  
  der.p <- dplyr::ntile( der, 100 )
  
  DER <- data.frame( der, der.w, der.n, der.p )
  
  print( summary( DER ) )
  
  par( mfrow=c(2,2) )
  plot( density(der,   na.rm=T), main="Debt to Equity Ratio (DER)" )
  plot( density(der.w, na.rm=T), main="DER Winsorized" )
  plot( density(der.n, na.rm=T), main="DER Standardized as Z" )
  plot( density(der.p, na.rm=T), main="DER as Percentile" )
  
  df.der <- data.frame( cbind( df, DER ) )
  return( df.der )
}





