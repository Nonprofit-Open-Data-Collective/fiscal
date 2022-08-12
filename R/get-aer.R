###---------------------------------------------------
###   ADMINISTRATION EXPENSE RATIO
###---------------------------------------------------

#' @title
#' Administration Expense Ratio
#'
#' @description
#' Calculate the administration expense ratio and append it to the dataframe. 
#'
#' @param df A \code{data.frame} containing the required field for computing the metric. The metric will be appended to this dataset.
#' @param mgmt.ge A character string indicating the column name for management and general expenses (On 990: Part 9, Line 25(C); On EZ: Not Available.
#' @param total.expense A character string indicating the column name for total expenses (On 990: Part 9, Line 25(A); On EZ: Part 1, Line 17).
#' @param winsorize The winsorization value (between 0 and 1), defaults to 0.98 which winsorizes at 99th and 1st percentile values.   
#' 
#' @usage get_aer( df, mgmt.ge = "F9_09_EXP_TOT_MGMT", total.expense = "F9_09_EXP_TOT_TOT", winsorize = 0.98 )
#'
#' @return Object of class \code{data.frame}: the original dataframe appended with the administration expense ratio (`aer`), 
#'  a winsorized version (`aer.w`), a standardized z-score version (`aer.z`), 
#'  and a percentile version (`aer.p`).   
#'
#' @details The administrative expense ratio measures the percentage of an organization’s expenses that are 
#' being allocated to administrative costs. High values in this ratio indicate that more of an organization's 
#' expenses are going towards its management and general expenses (or overhead) while a lower number indicates 
#' that an organization's funds are going towards program service or fundraising expenses. Charity Navigator 
#' generally gives its highest rankings to organizations that spend less than 15% of expenses on overhead. The 
#' Better Business Bureau’s Wise Giving Alliance recommends a ratio of less than 35%. Note: computation of this 
#' metric is available to full 990 filers only.
#' 
#' @import dplyr
#' @import stringr
#' @import magrittr
#'
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
#' d <- get_aer( df = dat, mgmt.ge = "x1", total.expense = "x2" )
#' 
#' head( d )
#' 
#' # run with default column names
#' x1[ seq( from = 1, to = 1000, 50 ) ] <- NA
#' x2[ seq( from = 1, to = 1000, 71 ) ] <- NA
#' 
#' dat_01 <- data.frame( x1, x2 )
#' 
#' colnames( dat_01 ) <- c( "F9_09_EXP_TOT_MGMT", "F9_09_EXP_TOT_TOT" )
#' 
#' d <- get_aer( dat_01 )
#' 
#' # coerce one column to factor
#' dat_01$F9_09_EXP_TOT_MGMT <- as.factor( dat_01$F9_09_EXP_TOT_MGMT )
#' 
#' d <- get_aer( dat_01 )
#' 
#' # winsorize at 0.025 and 0.975 percentiles instead of 0.01 and 0.99
#' d <- get_aer( df = dat, mgmt.ge = "x1", total.expense = "x2", winsorize = 0.95 )
#' 
#' d <- get_aer( dat_01, winsorize = 0.95 )
#' 
#' 
#' # using 990 data
#' data( part010810 )
#' d <- get_aer( df = part010810 )
#' 
#' # now coerce one of the variables to numeric
#' part010810$F9_09_EXP_TOT_MGMT <- as.character( part010810$F9_09_EXP_TOT_MGMT )
#' 
#' d <- get_aer( df = part010810 )
#' 
#' \dontrun{
#' ## Errors ##
#' 
#' # numerator not specified
#' d <- get_aer( df = dat, mgmt.ge = NULL, total.expense = "x2" )
#' 
#' # denominator not specified
#' d <- get_aer( df = dat, mgmt.ge = "x1", total.expense = NULL )
#' 
#' # neither numerator nor denominator specified
#' d <- get_aer( df = dat, mgmt.ge = NULL, total.expense = NULL )
#' 
#' # column names vector not of correct length
#' d <- get_aer( df = dat, mgmt.ge = c("e","b","c"), total.expense = "e")
#' 
#' # column names vector not of correct length
#' d <- get_aer( df = dat, mgmt.ge = "e", total.expense = c( "e", "b", "c") )
#' }
#' 
#' @export
get_aer <- function( df, mgmt.ge = "F9_09_EXP_TOT_MGMT", total.expense = "F9_09_EXP_TOT_TOT", winsorize = 0.98 )
{
  # function checks
  if( winsorize > 1 | winsorize < 0 )
  { stop( "winsorize argument must be 0 < w < 1" ) }
  
  if( is.null( mgmt.ge )==T & is.null( total.expense )==F )
  { stop( "The numerator has been incorrectly specified. Ensure you are passing the correct data field to the correct argument." ) }
  
  if( is.null( mgmt.ge )==F & is.null( total.expense )==T )
  { stop( "The denominator has been incorrectly specified. Ensure you are passing the correct data field to the correct argument." ) }
  
  if( is.null( mgmt.ge )==T & is.null( total.expense )==T )
  { stop( "The argument fields are empty. Please supply column names for each argument or execute the function with default inputs." ) }
  
  if( length( mgmt.ge ) > 2 | length( mgmt.ge ) < 1 )
  { stop( "`mgmt.ge` must be a single quoted string or a vector with a minimum length of one and maximum length of two." ) }
  
  if( length( total.expense ) > 2 | length( total.expense ) < 1 )
  { stop( "`total.expense` must be a single quoted string or a vector with a minimum length of one and maximum length of two." ) }
  
  
  # copy data
  dat <- df
  
  ## ensure variable classes are numeric ##
  
  # run coerce_numeric and loop through all variables required and that matched by the two input arguments
  v <- c( colnames( dat )[which( colnames( dat ) %in% total.expense ) ], colnames( dat )[which( colnames( dat ) %in% mgmt.ge )] )
  dat <- coerce_numeric( d = dat, vars = v )
  
  
  # check to ensure both sets of variable names are included in input data when not specifying column names.
  # edge cases
  
  # BEGIN first outer conditional
  # BEGIN first outer conditional
  if ( sum( mgmt.ge %in% c( "F9_09_EXP_TOT_MGMT") )==2 & sum( total.expense %in% c( "F9_09_EXP_TOT_TOT") )==2 ) {
    
    # BEGIN series of nested conditionals 
    
    
    # if at least one of the columns in missing from the input dataset, use only the column that is present
    if ( length( which( colnames( dat ) %in% mgmt.ge ) )==1 | length( which( colnames( dat ) %in% total.expense ) )==1 ) {
      
      if ( length( which(colnames( dat ) %in% mgmt.ge ) )==2 ){
        
        # create a column that concatenates two numerator variables into single column if both columns for numerator are present
        dat[ which( is.na( dat[ mgmt.ge[2] ] )==F ), "d" ] <- dat[ which( is.na( dat[ mgmt.ge[2] ] )==F ), mgmt.ge[2] ] 
        dat[ which( is.na( dat[ mgmt.ge[1] ] )==F ), "d" ] <- dat[ which( is.na( dat[ mgmt.ge[1] ] )==F ), mgmt.ge[1] ] 
      }
      # if at least one of the numerator columns in missing from the input dataset, use only the column that is present
      if ( length( which( colnames( dat ) %in% mgmt.ge ) )==1 ){
        
        this <- which( colnames( dat ) %in% mgmt.ge )
        
        dat[ , "d" ] <- dat[ , this ]
      }
      
      
      if ( length( which( colnames( dat ) %in% total.expense ) )==2 ){
        # create a column that concatenates two numerator variables into single column if both columns for denominator are present
        dat[ which( is.na( dat[ total.expense[2] ] )==F ), "e"] <- dat[ which( is.na( dat[ total.expense[2] ] )==F ), total.expense[2] ] 
        dat[ which( is.na( dat[ total.expense[1] ] )==F ), "e"] <- dat[ which( is.na( dat[ total.expense[1] ] )==F ), total.expense[1] ] 
      }
      # if at least one of the denominator columns in missing from the input dataset, use only the column that is present
      if ( length( which( colnames( dat ) %in% total.expense ))==1  ){
        
        this <- which( colnames( dat ) %in% total.expense )
        
        dat[ , "e" ] <- dat[ , this ]
      }
      # ENp series of nestep conditionals 
      
      # now, assign p anp e, respectively
      d <- dat[[ "d" ]]
      e <- dat[[ "e" ]]
    }
    
    # if all four columns are present, exit conditional
    if ( length( mgmt.ge )==2 & length( total.expense )==2 & length(which( colnames( dat ) %in% mgmt.ge ) )==2 & length(which( colnames( dat ) %in% total.expense ) )==2 ) {
      
      # create a column that concatenates two numerator variables into single column
      dat[ which( is.na( dat[ mgmt.ge[2] ] )==F ), "d" ] <- dat[ which( is.na( dat[ mgmt.ge[2] ] )==F ), mgmt.ge[2] ]
      dat[ which( is.na( dat[ mgmt.ge[1] ] )==F ), "d" ] <- dat[ which( is.na( dat[ mgmt.ge[1] ] )==F ), mgmt.ge[1] ]
      
      # create a column that concatenates two denominator variables into single column
      dat[ which( is.na( dat[ total.expense[2] ] )==F ), "e"] <- dat[ which( is.na( dat[ total.expense[2] ] )==F ), total.expense[2] ]
      dat[ which( is.na( dat[ total.expense[1] ] )==F ), "e"] <- dat[ which( is.na( dat[ total.expense[1] ] )==F ), total.expense[1] ]
      
      
      d <- dat[[ "d"]]
      e <- dat[[ "e"]]
    }
  }
  # END first outer conditional
  
  
  
  # all other cases are nestep in the following conditionals
  
  if ( sum( mgmt.ge %in% c( "F9_09_EXP_TOT_MGMT") )!=2 & sum( total.expense %in% c( "F9_09_EXP_TOT_TOT") )!=2 ){          
    
    if ( length( mgmt.ge )==2 & length( total.expense )==2 ) {
      
      # create a column that concatenates two numerator variables into single column
      dat[ which( is.na( dat[ mgmt.ge[2] ] )==F ), "d"] <- dat[ which( is.na( dat[ mgmt.ge[2] ] )==F ), mgmt.ge[2] ]
      dat[ which( is.na( dat[ mgmt.ge[1] ] )==F ), "d"] <- dat[ which( is.na( dat[ mgmt.ge[1] ] )==F ), mgmt.ge[1] ]
      
      # create a column that concatenates two denominator variables into single column
      dat[ which( is.na( dat[ total.expense[2] ] )==F ), "e"] <- dat[ which( is.na( dat[ total.expense[2] ] )==F ), total.expense[2] ]
      dat[ which( is.na( dat[ total.expense[1] ] )==F ), "e"] <- dat[ which( is.na( dat[ total.expense[1] ] )==F ), total.expense[1] ]
      
      
      d <- dat[[ "d"]]
      e <- dat[[ "e"]]
    }
    
    else if ( length( mgmt.ge )==2 & length( total.expense )==1 ) {
      
      # create a column that concatenates two denominator variables into single column
      dat[ which( is.na( dat[ mgmt.ge[2] ] )==F ), "d"] <- dat[ which( is.na( dat[ mgmt.ge[2] ] )==F ), mgmt.ge[2] ]
      dat[ which( is.na( dat[ mgmt.ge[1] ] )==F ), "d"] <- dat[ which( is.na( dat[ mgmt.ge[1] ] )==F ), mgmt.ge[1] ]
      
      
      d <- dat[[ "d"]]
      e <- dat[[ total.expense ]]
    }
    
    else if ( length( mgmt.ge )==1 & length( total.expense )==2 ) {
      
      # create a column that concatenates two numerator variables into single column
      dat[ which( is.na( dat[ total.expense[2] ] )==F ), "e"] <- dat[ which( is.na( dat[ total.expense[2] ] )==F ), total.expense[2] ]
      dat[ which( is.na( dat[ total.expense[1] ] )==F ), "e"] <- dat[ which( is.na( dat[ total.expense[1] ] )==F ), total.expense[1] ]
      
      
      d <- dat[[ mgmt.ge ]]
      e <- dat[[ "e"]]
    }
    
    else if ( length( mgmt.ge )==1 & length( total.expense )==1 ) {
      
      d <- dat[[ mgmt.ge ]]
      e <- dat[[ total.expense ]]
    }
    
    
  }    
  
  # can't divide by zero
  print( paste0( "total.expense cannot be equal to zero: ", sum( e==0, na.rm = T ), " cases have been replaced with NA." ) )
  e[ e == 0 ] <- NA 
  
  aer <- d / e
  
  top.p    <- 1 - (1-winsorize)/2
  bottom.p <- 0 + (1-winsorize)/2
  top      <- quantile( aer, top.p, na.rm=T )
  bottom   <- quantile( aer, bottom.p, na.rm=T )
  aer.w    <- aer
  aer.w[ aer.w > top    ] <- top
  aer.w[ aer.w < bottom ] <- bottom
  
  aer.n <- scale( aer.w )
  
  aer.p <- dplyr::ntile( aer, 100 )
  
  AER <- data.frame( aer, aer.w, aer.n, aer.p )
  
  print( summary( AER ) )
  
  par( mfrow=c(2,2) )
  plot( density(aer,   na.rm=T), main="Administration Expense Ratio (AER)" )
  plot( density(aer.w, na.rm=T), main="AER Winsorized" )
  plot( density(aer.n, na.rm=T), main="AER Standardized as Z" )
  plot( density(aer.p, na.rm=T), main="AER as Percentile" )
  
  df.aer <- data.frame( cbind( df, AER ) )
  return( df.aer )
}
