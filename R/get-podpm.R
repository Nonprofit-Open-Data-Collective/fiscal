###---------------------------------------------------
###   PRE-DEPRECIATION PROFITABILITY MARGIN
###---------------------------------------------------

#' @title
#' Post-Depreciation Profitability Margin
#'
#' @description
#' Calculate the post-depreciation profitability margin and append it to the dataframe. 
#' 
#' @param df A \code{data.frame} containing the required field for computing the metric. The metric will be appended to this dataset.
#' @param expenses A character string indicating the column name for total functional expenses (On 990: Part IX, line 25A; On EZ: Not Available).
#' @param revenue A character string indicating the column name for total revenue, (On 990: Part VIII, line 12A; On EZ: Part I, line 9).
#' @param winsorize The winsorization value (between 0 and 1), defaults to 0.98 which winsorizes at 99th and 1st percentile values.   
#' 
#' @usage get_podpm( df, 
#' expenses = "F9_09_EXP_TOT_TOT",
#' revenue = "F9_08_REV_TOT_TOT", 
#' winsorize = 0.98 )
#' 
#' @return Object of class \code{data.frame}: the original dataframe appended with the post-depreciation profitability margin (`podpm`), 
#'  a winsorized version (`podpm.w`), a standardized z-score version (`podpm.z`), 
#'  and a percentile version (`podpm.p`).   
#'
#' @details Post-depreciation profit is an income measure used to determine profit after incorporating 
#' non-cash expenses on a balance sheet. Post-depreciation profit is calculated because it provides a 
#' picture of an organization’s true available profits net of depreciation expenses. Non-expense items lower 
#' an organization’s reported earnings, so a post-depreciation profit would show a lower profit in comparison 
#' to profits calculated prior to depreciation expenses.High values in this metric are generally desirable 
#' since they indicate that an organization is not losing a lot of its revenues to expenses. Values close to 
#' zero are normal, and negative numbers indicate the organization is functioning at a deficit. Note: computation of this 
#' metric is available to only 990 filers and not for 990-EZ filers. The default inputs use column names for 
#' variables available only to 990 filers.
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
#' d <- get_podpm( df = dat, expenses = "x1", revenue = "x2" )
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
#' colnames( dat_01 ) <- c( "F9_09_EXP_TOT_TOT", "F9_08_REV_TOT_TOT" )
#' 
#' d <- get_podpm( dat_01 )
#' 
#' # coerce one column to factor
#' dat_01$F9_09_EXP_TOT_TOT <- as.factor( dat_01$F9_09_EXP_TOT_TOT )
#' 
#' d <- get_podpm( dat_01 )
#' 
#' # winsorize at 0.025 and 0.975 percentiles instead of 0.01 and 0.99
#' d <- get_podpm( df = dat, expenses = "x1", revenue = "x2", winsorize = 0.95 )
#' 
#' d <- get_podpm( dat_01, winsorize = 0.95 )
#' 
#' 
#' # using 990 data
#' data( part010810 )
#' d <- get_podpm( df = part010810 )
#' 
#' # now coerce one of the variables to numeric
#' part010810$F9_09_EXP_TOT_TOT <- as.character( part010810$F9_09_EXP_TOT_TOT )
#' 
#' d <- get_podpm( df = part010810 )
#' 
#' \dontrun{
#' ## Errors ##
#' 
#' # numerator not specified
#' d <- get_podpm( df = dat, expenses = NULL, revenue = "x2" )
#' 
#' # denominator not specified
#' d <- get_podpm( df = dat, expenses = "x1", revenue = NULL )
#' 
#' # neither numerator nor denominator specified
#' d <- get_podpm( df = dat, expenses = NULL, revenue = NULL )
#' 
#' # column names vector not of correct length
#' d <- get_podpm( df = dat, expenses = c("e","b","c"), revenue = "e")
#' 
#' # column names vector not of correct length
#' d <- get_podpm( df = dat, expenses = "e", revenue = c( "e", "b", "c") )
#' }
#' @export
get_podpm <- function( df, 
                       expenses = "F9_09_EXP_TOT_TOT",
                       revenue = "F9_08_REV_TOT_TOT", 
                       winsorize = 0.98 )
{
  
  # function checks
  if( winsorize > 1 | winsorize < 0 )
  { stop( "winsorize argument must be 0 < w < 1" ) }
  
  if( is.null( expenses )==T & is.null( revenue )==F )
  { stop( "The numerator has been incorrectly specified. Ensure you are passing the correct data field to the correct argument." ) }
  
  if( is.null( expenses )==F & is.null( revenue )==T )
  { stop( "The denominator has been incorrectly specified. Ensure you are passing the correct data field to the correct argument." ) }
  
  if( is.null( expenses )==T & is.null( revenue )==T )
  { stop( "The argument fields are empty. Please supply column names for each argument or execute the function with default inputs." ) }
  
  if( length( expenses ) > 2 | length( expenses ) < 1 )
  { stop( "`expenses` must be a single quoted string or a vector with a minimum length of one and maximum length of two." ) }
  
  if( length( revenue ) > 2 | length( revenue ) < 1 )
  { stop( "`revenue` must be a single quoted string or a vector with a minimum length of one and maximum length of two." ) }
  
  
  # copy data
  dat <- df
  
  ## ensure variable classes are numeric ##
  
  # run coerce_numeric and loop through all variables required and that matched by the two input arguments
  v <- c( colnames( dat )[which( colnames( dat ) %in% revenue ) ], colnames( dat )[which( colnames( dat ) %in% expenses )] )
  dat <- coerce_numeric( d = dat, vars = v )
  
  
  # check to ensure both sets of variable names are included in input data when not specifying column names.
  # edge cases
  
  # BEGIN first outer conditional
  # BEGIN first outer conditional
  if ( sum( expenses %in% c( "F9_09_EXP_TOT_TOT") )==2 & sum( revenue %in% c( "F9_08_REV_TOT_TOT") )==2 ) {
    
    # BEGIN series of nested conditionals 
    
    
    # if at least one of the columns in missing from the input dataset, use only the column that is present
    if ( length( which( colnames( dat ) %in% expenses ) )==1 | length( which( colnames( dat ) %in% revenue ) )==1 ) {
      
      if ( length( which(colnames( dat ) %in% expenses ) )==2 ){
        
        # create a column that concatenates two numerator variables into single column if both columns for numerator are present
        dat[ which( is.na( dat[ expenses[2] ] )==F ), "d"] <- dat[ which( is.na( dat[ expenses[2] ] )==F ), expenses[2] ] 
        dat[ which( is.na( dat[ expenses[1] ] )==F ), "d"] <- dat[ which( is.na( dat[ expenses[1] ] )==F ), expenses[1] ] 
      }
      # if at least one of the numerator columns in missing from the input dataset, use only the column that is present
      if ( length( which( colnames( dat ) %in% expenses ) )==1 ){
        
        this <- which( colnames( dat ) %in% expenses )
        
        dat[ , "d"] <- dat[ , this ]
      }
      
      
      if ( length( which( colnames( dat ) %in% revenue ) )==2 ){
        # create a column that concatenates two numerator variables into single column if both columns for denominator are present
        dat[ which( is.na( dat[ revenue[2] ] )==F ), "e"] <- dat[ which( is.na( dat[ revenue[2] ] )==F ), revenue[2] ] 
        dat[ which( is.na( dat[ revenue[1] ] )==F ), "e"] <- dat[ which( is.na( dat[ revenue[1] ] )==F ), revenue[1] ] 
      }
      # if at least one of the denominator columns in missing from the input dataset, use only the column that is present
      if ( length( which( colnames( dat ) %in% revenue ))==1  ){
        
        this <- which( colnames( dat ) %in% revenue )
        
        dat[ , "e"] <- dat[ , this ]
      }
      # ENp series of nestep conditionals 
      
      # now, assign p anp e, respectively
      d <- dat[[ "d"]]
      e <- dat[[ "e"]]
    }
    
    # if all four columns are present, exit conditional
    if ( length( expenses )==2 & length( revenue )==2 & length(which( colnames( dat ) %in% expenses ) )==2 & length(which( colnames( dat ) %in% revenue ) )==2 ) {
      
      # create a column that concatenates two numerator variables into single column
      dat[ which( is.na( dat[ expenses[2] ] )==F ), "d"] <- dat[ which( is.na( dat[ expenses[2] ] )==F ), expenses[2] ]
      dat[ which( is.na( dat[ expenses[1] ] )==F ), "d"] <- dat[ which( is.na( dat[ expenses[1] ] )==F ), expenses[1] ]
      
      # create a column that concatenates two denominator variables into single column
      dat[ which( is.na( dat[ revenue[2] ] )==F ), "e"] <- dat[ which( is.na( dat[ revenue[2] ] )==F ), revenue[2] ]
      dat[ which( is.na( dat[ revenue[1] ] )==F ), "e"] <- dat[ which( is.na( dat[ revenue[1] ] )==F ), revenue[1] ]
      
      
      d <- dat[[ "d"]]
      e <- dat[[ "e"]]
    }
  }
  # END first outer conditional
  
  
  
  # all other cases are nestep in the following conditionals
  
  if ( sum( expenses %in% c( "F9_09_EXP_TOT_TOT") )!=2 & sum( revenue %in% c( "F9_08_REV_TOT_TOT") )!=2 ){          
    
    if ( length( expenses )==2 & length( revenue )==2 ) {
      
      # create a column that concatenates two numerator variables into single column
      dat[ which( is.na( dat[ expenses[2] ] )==F ), "d"] <- dat[ which( is.na( dat[ expenses[2] ] )==F ), expenses[2] ]
      dat[ which( is.na( dat[ expenses[1] ] )==F ), "d"] <- dat[ which( is.na( dat[ expenses[1] ] )==F ), expenses[1] ]
      
      # create a column that concatenates two denominator variables into single column
      dat[ which( is.na( dat[ revenue[2] ] )==F ), "e"] <- dat[ which( is.na( dat[ revenue[2] ] )==F ), revenue[2] ]
      dat[ which( is.na( dat[ revenue[1] ] )==F ), "e"] <- dat[ which( is.na( dat[ revenue[1] ] )==F ), revenue[1] ]
      
      
      d <- dat[[ "d"]]
      e <- dat[[ "e"]]
    }
    
    else if ( length( expenses )==2 & length( revenue )==1 ) {
      
      # create a column that concatenates two denominator variables into single column
      dat[ which( is.na( dat[ expenses[2] ] )==F ), "d"] <- dat[ which( is.na( dat[ expenses[2] ] )==F ), expenses[2] ]
      dat[ which( is.na( dat[ expenses[1] ] )==F ), "d"] <- dat[ which( is.na( dat[ expenses[1] ] )==F ), expenses[1] ]
      
      
      d <- dat[[ "d"]]
      e <- dat[[ revenue ]]
    }
    
    else if ( length( expenses )==1 & length( revenue )==2 ) {
      
      # create a column that concatenates two numerator variables into single column
      dat[ which( is.na( dat[ revenue[2] ] )==F ), "e"] <- dat[ which( is.na( dat[ revenue[2] ] )==F ), revenue[2] ]
      dat[ which( is.na( dat[ revenue[1] ] )==F ), "e"] <- dat[ which( is.na( dat[ revenue[1] ] )==F ), revenue[1] ]
      
      
      d <- dat[[ expenses ]]
      e <- dat[[ "e"]]
    }
    
    else if ( length( expenses )==1 & length( revenue )==1 ) {
      
      d <- dat[[ expenses ]]
      e <- dat[[ revenue ]]
    }
    
    
  }    
  
  # can't divide by zero
  print( paste0( "Revenues cannot be equal to zero: ", sum( e==0, na.rm = T ), " cases have been replaced with NA." ) )
  e[ e == 0 ] <- NA 
  
  podpm <- ( (e - d) / e )
  
  top.p    <- 1 - (1-winsorize)/2
  bottom.p <- 0 + (1-winsorize)/2
  top      <- quantile( podpm, top.p, na.rm=T )
  bottom   <- quantile( podpm, bottom.p, na.rm=T )
  podpm.w    <- podpm
  podpm.w[ podpm.w > top    ] <- top
  podpm.w[ podpm.w < bottom ] <- bottom
  
  podpm.n <- scale( podpm.w )
  
  podpm.p <- dplyr::ntile( podpm, 100 )
  
  PODPM <- data.frame( podpm, podpm.w, podpm.n, podpm.p )
  
  print( summary( PODPM ) )
  
  par( mfrow=c(2,2) )
  plot( density(podpm,   na.rm=T), main="Post-Depreciation Profitability Margin (PODPM)" )
  plot( density(podpm.w, na.rm=T), main="PODPM Winsorized" )
  plot( density(podpm.n, na.rm=T), main="PODPM Standardized as Z" )
  plot( density(podpm.p, na.rm=T), main="PODPM as Percentile" )
  
  df.podpm <- data.frame( cbind( df, PODPM ) )
  return( df.podpm )
}
