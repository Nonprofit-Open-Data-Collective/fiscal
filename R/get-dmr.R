###---------------------------------------------------
###   DEBT MANAGEMENT RATIO
###---------------------------------------------------

#' @title
#' Debt Management Ratio 
#'
#' @description
#' Calculate the debt management ratio and append it to the dataframe. 
#'
#' @param df A \code{data.frame} containing the required field for computing the metric. The metric will be appended to this dataset.
#' @param liabilities A character string indicating the column name for total liabilities, EOY (On 990: Part X, line 26B; On EZ: Part II, line 26B) with the default name supplied.
#' @param net.assets A character string indicating the column name for unrestricted net assets, EOY (On 990: Part X, line 27B; On EZ: Not Available) with the default name supplied.
#' @param winsorize The winsorization value (between 0 and 1), defaults to 0.98 which winsorizes at 99th and 1st percentile values.   
#' 
#' @usage get_dmr( df, liabilities = "F9_10_LIAB_TOT_EOY", 
#' net.assets = "F9_10_NAFB_UNRESTRICT_EOY", 
#' winsorize = 0.98 )
#' 
#' @return Object of class \code{data.frame}: the original dataframe appended with the debt management ratio (`dmr`), 
#'  a winsorized version (`dmr.w`), a standardized z-score version (`dmr.z`), 
#'  and a percentile version (`dmr.p`).   
#'
#' @details This metric measures how much an organization is relying on funding from lending entities and the 
#' amount of free, unrestricted cash it has to pay back those loans. A high value in this metric could mean 
#' an organization is highly leveraged and thus that it has reduced ability to borrow more money in the future. 
#' Conversely, a low value indicates the organization may not be leveraging its assets to achieve the most 
#' growth and impact it could or that there is a dearth of credit available from lending entities.
#' Note: computation of this metric is available to both 990 and 990-EZ filers.
#' 
#' @import dplyr
#' @import stringr
#' @import magrittr
#' 
#' @examples
#' library( fiscal )
#' x1 <- rnorm( 1000, 100, 30 )
#' x2 <- rnorm( 1000, 200, 30 )
#' x2[ c( 15, 300, 600 ) ] <- 0
#' 
#' dat <- data.frame( x1, x2 )
#' 
#' # specify own column names
#' d <- get_dmr( df = dat, liabilities = "x1", net.assets = "x2" )
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
#' colnames( dat_01 ) <- c( "F9_10_LIAB_TOT_EOY", "F9_10_NAFB_UNRESTRICT_EOY" )
#' 
#' d <- get_dmr( dat_01 )
#' 
#' # coerce one column to factor
#' dat_01$F9_10_LIAB_TOT_EOY <- as.factor( dat_01$F9_10_LIAB_TOT_EOY )
#' 
#' d <- get_dmr( dat_01 )
#' 
#' # winsorize at 0.025 and 0.975 percentiles instead of 0.01 and 0.99
#' d <- get_dmr( df = dat, liabilities = "x1", net.assets = "x2", winsorize = 0.95 )
#' 
#' d <- get_dmr( dat_01, winsorize = 0.95 )
#' 
#' 
#' # using 990 data
#' data( part010810 )
#' d <- get_dmr( df = part010810 )
#' 
#' # now coerce one of the variables to numeric
#' part010810$F9_10_LIAB_TOT_EOY <- as.character( part010810$F9_10_LIAB_TOT_EOY )
#' 
#' d <- get_dmr( df = part010810 )
#' 
#' \dontrun{
#' ## Errors ##
#' 
#' # numerator not specified
#' d <- get_dmr( df = dat, liabilities = NULL, net.assets = "x2" )
#' 
#' # denominator not specified
#' d <- get_dmr( df = dat, liabilities = "x1", net.assets = NULL )
#' 
#' # neither numerator nor denominator specified
#' d <- get_dmr( df = dat, liabilities = NULL, net.assets = NULL )
#' 
#' # column names vector not of correct length
#' d <- get_dmr( df = dat, liabilities = c("e","b","c"), net.assets = "e")
#' 
#' # column names vector not of correct length
#' d <- get_dmr( df = dat, liabilities = "e", net.assets = c( "e", "b", "c") )
#' }
#' @export
get_dmr <- function( df, liabilities = "F9_10_LIAB_TOT_EOY", 
                     net.assets = "F9_10_NAFB_UNRESTRICT_EOY", 
                     winsorize = 0.98 )
{
  # function checks
  if( winsorize > 1 | winsorize < 0 )
  { stop( "winsorize argument must be 0 < w < 1" ) }
  
  if( is.null( liabilities )==T & is.null( net.assets )==F )
  { stop( "The numerator has been incorrectly specified. Ensure you are passing the correct data field to the correct argument." ) }
  
  if( is.null( liabilities )==F & is.null( net.assets )==T )
  { stop( "The denominator has been incorrectly specified. Ensure you are passing the correct data field to the correct argument." ) }
  
  if( is.null( liabilities )==T & is.null( net.assets )==T )
  { stop( "The argument fields are empty. Please supply column names for each argument or execute the function with default inputs." ) }
  
  if( length( liabilities ) > 2 | length( liabilities ) < 1 )
  { stop( "`liabilities` must be a single quoted string or a vector with a minimum length of one and maximum length of two." ) }
  
  if( length( net.assets ) > 2 | length( net.assets ) < 1 )
  { stop( "`net.assets` must be a single quoted string or a vector with a minimum length of one and maximum length of two." ) }
  
  
  # copy data
  dat <- df
  
  ## ensure variable classes are numeric ##
  
  # run coerce_numeric and loop through all variables required and that matched by the two input arguments
  v <- c( colnames( dat )[which( colnames( dat ) %in% net.assets ) ], colnames( dat )[which( colnames( dat ) %in% liabilities )] )
  dat <- coerce_numeric( d = dat, vars = v )
  
  
  # check to ensure both sets of variable names are included in input data when not specifying column names.
  # edge cases
  
  # BEGIN first outer conditional
  # BEGIN first outer conditional
  if ( sum( liabilities %in% c( "F9_10_LIAB_TOT_EOY") )==2 & sum( net.assets %in% c( "F9_10_NAFB_UNRESTRICT_EOY") )==2 ) {
    
    # BEGIN series of nested conditionals 
    
    
    # if at least one of the columns in missing from the input dataset, use only the column that is present
    if ( length( which( colnames( dat ) %in% liabilities ) )==1 | length( which( colnames( dat ) %in% net.assets ) )==1 ) {
      
      if ( length( which(colnames( dat ) %in% liabilities ) )==2 ){
        
        # create a column that concatenates two numerator variables into single column if both columns for numerator are present
        dat[ which( is.na( dat[ liabilities[2] ] )==F ), "l"] <- dat[ which( is.na( dat[ liabilities[2] ] )==F ), liabilities[2] ] 
        dat[ which( is.na( dat[ liabilities[1] ] )==F ), "l"] <- dat[ which( is.na( dat[ liabilities[1] ] )==F ), liabilities[1] ] 
      }
      # if at least one of the numerator columns in missing from the input dataset, use only the column that is present
      if ( length( which( colnames( dat ) %in% liabilities ) )==1 ){
        
        this <- which( colnames( dat ) %in% liabilities )
        
        dat[ , "l"] <- dat[ , this ]
      }
      
      
      if ( length( which( colnames( dat ) %in% net.assets ) )==2 ){
        # create a column that concatenates two numerator variables into single column if both columns for denominator are present
        dat[ which( is.na( dat[ net.assets[2] ] )==F ), "n"] <- dat[ which( is.na( dat[ net.assets[2] ] )==F ), net.assets[2] ] 
        dat[ which( is.na( dat[ net.assets[1] ] )==F ), "n"] <- dat[ which( is.na( dat[ net.assets[1] ] )==F ), net.assets[1] ] 
      }
      # if at least one of the denominator columns in missing from the input dataset, use only the column that is present
      if ( length( which( colnames( dat ) %in% net.assets ))==1  ){
        
        this <- which( colnames( dat ) %in% net.assets )
        
        dat[ , "n"] <- dat[ , this ]
      }
      # ENp series of nestep conditionals 
      
      # now, assign p anp e, respectively
      l <- dat[[ "l"]]
      n <- dat[[ "n"]]
    }
    
    # if all four columns are present, exit conditional
    if ( length( liabilities )==2 & length( net.assets )==2 & length(which( colnames( dat ) %in% liabilities ) )==2 & length(which( colnames( dat ) %in% net.assets ) )==2 ) {
      
      # create a column that concatenates two numerator variables into single column
      dat[ which( is.na( dat[ liabilities[2] ] )==F ), "l"] <- dat[ which( is.na( dat[ liabilities[2] ] )==F ), liabilities[2] ]
      dat[ which( is.na( dat[ liabilities[1] ] )==F ), "l"] <- dat[ which( is.na( dat[ liabilities[1] ] )==F ), liabilities[1] ]
      
      # create a column that concatenates two denominator variables into single column
      dat[ which( is.na( dat[ net.assets[2] ] )==F ), "n"] <- dat[ which( is.na( dat[ net.assets[2] ] )==F ), net.assets[2] ]
      dat[ which( is.na( dat[ net.assets[1] ] )==F ), "n"] <- dat[ which( is.na( dat[ net.assets[1] ] )==F ), net.assets[1] ]
      
      
      l <- dat[[ "l"]]
      n <- dat[[ "n"]]
    }
  }
  # END first outer conditional
  
  
  
  # all other cases are nestep in the following conditionals
  
  if ( sum( liabilities %in% c( "F9_10_LIAB_TOT_EOY") )!=2 & sum( net.assets %in% c( "F9_10_NAFB_UNRESTRICT_EOY") )!=2 ){          
    
    if ( length( liabilities )==2 & length( net.assets )==2 ) {
      
      # create a column that concatenates two numerator variables into single column
      dat[ which( is.na( dat[ liabilities[2] ] )==F ), "l"] <- dat[ which( is.na( dat[ liabilities[2] ] )==F ), liabilities[2] ]
      dat[ which( is.na( dat[ liabilities[1] ] )==F ), "l"] <- dat[ which( is.na( dat[ liabilities[1] ] )==F ), liabilities[1] ]
      
      # create a column that concatenates two denominator variables into single column
      dat[ which( is.na( dat[ net.assets[2] ] )==F ), "n"] <- dat[ which( is.na( dat[ net.assets[2] ] )==F ), net.assets[2] ]
      dat[ which( is.na( dat[ net.assets[1] ] )==F ), "n"] <- dat[ which( is.na( dat[ net.assets[1] ] )==F ), net.assets[1] ]
      
      
      l <- dat[[ "l"]]
      n <- dat[[ "n"]]
    }
    
    else if ( length( liabilities )==2 & length( net.assets )==1 ) {
      
      # create a column that concatenates two denominator variables into single column
      dat[ which( is.na( dat[ liabilities[2] ] )==F ), "l"] <- dat[ which( is.na( dat[ liabilities[2] ] )==F ), liabilities[2] ]
      dat[ which( is.na( dat[ liabilities[1] ] )==F ), "l"] <- dat[ which( is.na( dat[ liabilities[1] ] )==F ), liabilities[1] ]
      
      
      l <- dat[[ "l"]]
      n <- dat[[ net.assets ]]
    }
    
    else if ( length( liabilities )==1 & length( net.assets )==2 ) {
      
      # create a column that concatenates two numerator variables into single column
      dat[ which( is.na( dat[ net.assets[2] ] )==F ), "n"] <- dat[ which( is.na( dat[ net.assets[2] ] )==F ), net.assets[2] ]
      dat[ which( is.na( dat[ net.assets[1] ] )==F ), "n"] <- dat[ which( is.na( dat[ net.assets[1] ] )==F ), net.assets[1] ]
      
      
      l <- dat[[ liabilities ]]
      n <- dat[[ "n"]]
    }
    
    else if ( length( liabilities )==1 & length( net.assets )==1 ) {
      
      l <- dat[[ liabilities ]]
      n <- dat[[ net.assets ]]
    }
    
    
  }    
 
  # can't divide by zero
  print( paste0( "Unrestricted net assets cannot be equal to zero: ", sum( n==0, na.rm = T ), " cases have been replaced with NA." ) )
  n[ n == 0 ] <- NA 
  
  dmr <- l / n
  
  top.p    <- 1 - (1-winsorize)/2
  bottom.p <- 0 + (1-winsorize)/2
  top      <- quantile( dmr, top.p, na.rm=T )
  bottom   <- quantile( dmr, bottom.p, na.rm=T )
  dmr.w    <- dmr
  dmr.w[ dmr.w > top    ] <- top
  dmr.w[ dmr.w < bottom ] <- bottom
  
  dmr.n <- scale( dmr.w )
  
  dmr.p <- dplyr::ntile( dmr, 100 )
  
  DMR <- data.frame( dmr, dmr.w, dmr.n, dmr.p )
  
  print( summary( DMR ) )
  
  par( mfrow=c(2,2) )
  plot( density(dmr,   na.rm=T), main="Debt Management Ratio (DMR)" )
  plot( density(dmr.w, na.rm=T), main="DMR Winsorized" )
  plot( density(dmr.n, na.rm=T), main="DMR Standardized as Z" )
  plot( density(dmr.p, na.rm=T), main="DMR as Percentile" )
  
  df.dmr <- data.frame( cbind( df, DMR ) )
  return( df.dmr )
}
