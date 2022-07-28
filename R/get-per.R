###---------------------------------------------------
###   PROGRAM EFFICIENCY RATIO
###---------------------------------------------------

#' @title
#' Program Efficiency Ratio
#'
#' @description
#' Calculate the program efficiency ratio and append it to the dataframe. 
#'
#' @param df A \code{data.frame} containing the required field for computing the metric. The metric will be appended to this dataset.
#' @param pse A character string indicating the column name for program service expenses (On 990: Part 9, Line 25B; On EZ: Pt II, Line 25B).
#' @param total.expense A character string indicating the column name for total expenses (On 990: Part 1, Line 18(B); On EZ: Part 1, Line 17).
#' 
#' @usage get_per( df, 
#' pse = c( "F9_09_EXP_TOT_PROG", "F9_10_ASSET_TOT_EOY" ), 
#' total.expense = "F9_01_EXP_TOT_CY",
#' winsorize = 0.98 )
#'
#' @return Object of class \code{data.frame}: the original dataframe appended with the program efficiency ratio (`per`), 
#'  a winsorized version (`per.w`), a standardized z-score version (`per.z`), 
#'  and a percentile version (`per.p`).   
#'
#' @details The program efficiency ratio measures the percentage of expenses that a nonprofit organization is 
#' spending on its core mission. High values in this ratio indicate that more of an organization's expenses 
#' are going towards its core mission or program services while a lower number indicates that an organization's 
#' expenses are comprised of other things like management or fundraising. Charity Navigator generally gives 
#' the highest rankings to those organizations whose ratio of program expenses is 85% or higher of their total 
#' expenses. Other agencies, such as the Better Business Bureau’s Wise Giving Alliance, recommend a ratio of 
#' 65% or higher. Note: computation of this metric is available to both 990 and 990-EZ filers.
#' 
#' @examples
#' x1 <- rnorm( 1000, 100, 30 )
#' x2 <- rnorm( 1000, 200, 30 )
#' x2[ c( 15, 300, 600 ) ] <- 0
#' 
#' dat <- data.frame( x1, x2 )
#' 
#' # specify own column names
#' d <- get_per( df = dat, pse = "x1", total.expense = "x2" )
#' 
#' head( d )
#' 
#' # run with default column names
#' x3 <- rnorm( 1000, 100, 30 )
#' x3[ seq( from = 1, to = 1000, 50 ) ] <- NA
#' x3[ seq( from = 2, to = 998, 53 ) ] <- 0
#' 
#' 
#' dat_01 <- data.frame( x1, x2, x3 )
#' 
#' colnames( dat_01 ) <- c( "F9_09_EXP_TOT_PROG", "F9_10_ASSET_TOT_EOY", "F9_01_EXP_TOT_CY" )
#' 
#' d <- get_per( dat_01 )
#' 
#' # coerce one column to factper
#' dat_01$F9_09_EXP_TOT_PROG <- as.factor( dat_01$F9_09_EXP_TOT_PROG )
#' 
#' d <- get_per( dat_01 )
#' 
#' # winsorize at 0.025 and 0.975 percentiles instead of 0.01 and 0.99
#' d <- get_per( df = dat, pse = "x1", total.expense = "x2", winsorize = 0.95 )
#' 
#' d <- get_per( dat_01, winsorize = 0.95 )
#' 
#' 
#' # using 990 data
#' load( '/Volumes/My Passport for Mac/Urban Institute/Summer Projects/Fiscal/fiscal/R/sysdata.rda' )
#' d <- get_per( df = part010810 )
#' 
#' # now coerce one of the variables to numeric
#' part010810$F9_09_EXP_TOT_PROG <- as.character( part010810$F9_09_EXP_TOT_PROG )
#' 
#' d <- get_per( df = part010810 )
#' 
#' 
#' ## Errors ##
#' 
#' # numerator not specified
#' d <- get_per( df = dat, pse = NULL, total.expense = "x2" )
#' 
#' # denominator not specified
#' d <- get_per( df = dat, pse = "x1", total.expense = NULL )
#' 
#' # neither numerator nor denominator specified
#' d <- get_per( df = dat, pse = NULL, total.expense = NULL )
#' 
#' # column names vector not of correct length
#' d <- get_per( df = dat, pse = c("e","b","c"), total.expense = "e")
#' 
#' # column names vector not of correct length
#' d <- get_per( df = dat, pse = "e", total.expense = c( "e", "b", "c") ) 
#' @export
get_per <- function( df, 
                     pse = c( "F9_09_EXP_TOT_PROG", "F9_10_ASSET_TOT_EOY" ), 
                     total.expense = "F9_01_EXP_TOT_CY",
                     winsorize = 0.98 )
{
  # function checks
  if( winsorize > 1 | winsorize < 0 )
  { stop( "winsorize argument must be 0 < w < 1" ) }
  
  if( is.null( pse )==T & is.null( total.expense )==F )
  { stop( "The numerator has been incorrectly specified. Ensure you are passing the correct data field to the correct argument." ) }
  
  if( is.null( pse )==F & is.null( total.expense )==T )
  { stop( "The denominator has been incorrectly specified. Ensure you are passing the correct data field to the correct argument." ) }
  
  if( is.null( pse )==T & is.null( total.expense )==T )
  { stop( "The argument fields are empty. Please supply column names for each argument or execute the function with default inputs." ) }
  
  if( length( pse ) > 2 | length( pse ) < 1 )
  { stop( "`pse` must be a single quoted string or a vector with a minimum length of one and maximum length of two." ) }
  
  if( length( total.expense ) > 2 | length( total.expense ) < 1 )
  { stop( "`total.expense` must be a single quoted string or a vector with a minimum length of one and maximum length of two." ) }
  
  
  # copy data
  dat <- df
  
  ## ensure variable classes are numeric ##
  
  # run coerce_numeric and loop through all variables required and that matched by the two input arguments
  v <- c( colnames( dat )[which( colnames( dat ) %in% total.expense ) ], colnames( dat )[which( colnames( dat ) %in% pse )] )
  dat <- coerce_numeric( d = dat, vars = v )
  
  
  # check to ensure both sets of variable names are included in input data when not specifying column names.
  # edge cases
  
  # BEGIN first outer conditional
  # BEGIN first outer conditional
  if ( sum( pse %in% c( "F9_09_EXP_TOT_PROG", "F9_10_ASSET_TOT_EOY") )==2 & sum( total.expense %in% c( "F9_01_EXP_TOT_CY") )==2 ) {
    
    # BEGIN series of nested conditionals 
    
    
    # if at least one of the columns in missing from the input dataset, use only the column that is present
    if ( length( which( colnames( dat ) %in% pse ) )==1 | length( which( colnames( dat ) %in% total.expense ) )==1 ) {
      
      if ( length( which(colnames( dat ) %in% pse ) )==2 ){
        
        # create a column that concatenates two numerator variables into single column if both columns for numerator are present
        dat[ which( is.na( dat[ pse[2] ] )==F ), "d" ] <- dat[ which( is.na( dat[ pse[2] ] )==F ), pse[2] ] 
        dat[ which( is.na( dat[ pse[1] ] )==F ), "d" ] <- dat[ which( is.na( dat[ pse[1] ] )==F ), pse[1] ] 
      }
      # if at least one of the numerator columns in missing from the input dataset, use only the column that is present
      if ( length( which( colnames( dat ) %in% pse ) )==1 ){
        
        this <- which( colnames( dat ) %in% pse )
        
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
      # END series of nested conditionals 
      
      # now, assign p and e, respectively
      d <- dat[[ "d" ]]
      e <- dat[[ "e" ]]
    }
    
    # if all four columns are present, exit conditional
    if ( length( pse )==2 & length( total.expense )==2 & length(which( colnames( dat ) %in% pse ) )==2 & length(which( colnames( dat ) %in% total.expense ) )==2 ) {
      
      # create a column that concatenates two numerator variables into single column
      dat[ which( is.na( dat[ pse[2] ] )==F ), "d" ] <- dat[ which( is.na( dat[ pse[2] ] )==F ), pse[2] ]
      dat[ which( is.na( dat[ pse[1] ] )==F ), "d" ] <- dat[ which( is.na( dat[ pse[1] ] )==F ), pse[1] ]
      
      # create a column that concatenates two denominator variables into single column
      dat[ which( is.na( dat[ total.expense[2] ] )==F ), "e"] <- dat[ which( is.na( dat[ total.expense[2] ] )==F ), total.expense[2] ]
      dat[ which( is.na( dat[ total.expense[1] ] )==F ), "e"] <- dat[ which( is.na( dat[ total.expense[1] ] )==F ), total.expense[1] ]
      
      
      d <- dat[[ "d"]]
      e <- dat[[ "e"]]
    }
  }
  # END first outer conditional
  
  
  
  # all other cases are nestep in the following conditionals
  
  if ( sum( pse %in% c( "F9_09_EXP_TOT_PROG", "F9_10_ASSET_TOT_EOY") )!=2 | sum( total.expense %in% c( "F9_01_EXP_TOT_CY") )!=2 ){          
    
    if ( length( pse )==2 & length( total.expense )==2 ) {
      
      # create a column that concatenates two numerator variables into single column
      dat[ which( is.na( dat[ pse[2] ] )==F ), "d"] <- dat[ which( is.na( dat[ pse[2] ] )==F ), pse[2] ]
      dat[ which( is.na( dat[ pse[1] ] )==F ), "d"] <- dat[ which( is.na( dat[ pse[1] ] )==F ), pse[1] ]
      
      # create a column that concatenates two denominator variables into single column
      dat[ which( is.na( dat[ total.expense[2] ] )==F ), "e"] <- dat[ which( is.na( dat[ total.expense[2] ] )==F ), total.expense[2] ]
      dat[ which( is.na( dat[ total.expense[1] ] )==F ), "e"] <- dat[ which( is.na( dat[ total.expense[1] ] )==F ), total.expense[1] ]
      
      
      d <- dat[[ "d"]]
      e <- dat[[ "e"]]
    }
    
    else if ( length( pse )==2 & length( total.expense )==1 ) {
      
      # create a column that concatenates two denominator variables into single column
      dat[ which( is.na( dat[ pse[2] ] )==F ), "d"] <- dat[ which( is.na( dat[ pse[2] ] )==F ), pse[2] ]
      dat[ which( is.na( dat[ pse[1] ] )==F ), "d"] <- dat[ which( is.na( dat[ pse[1] ] )==F ), pse[1] ]
      
      
      d <- dat[[ "d"]]
      e <- dat[[ total.expense ]]
    }
    
    else if ( length( pse )==1 & length( total.expense )==2 ) {
      
      # create a column that concatenates two numerator variables into single column
      dat[ which( is.na( dat[ total.expense[2] ] )==F ), "e"] <- dat[ which( is.na( dat[ total.expense[2] ] )==F ), total.expense[2] ]
      dat[ which( is.na( dat[ total.expense[1] ] )==F ), "e"] <- dat[ which( is.na( dat[ total.expense[1] ] )==F ), total.expense[1] ]
      
      
      d <- dat[[ pse ]]
      e <- dat[[ "e"]]
    }
    
    else if ( length( pse )==1 & length( total.expense )==1 ) {
      
      d <- dat[[ pse ]]
      e <- dat[[ total.expense ]]
    }
    
    
  }    
  
  # can't divide by zero
  print( paste0( "Total expenses cannot be equal to zero: ", sum( e==0, na.rm = T ), " cases have been replaced with NA." ) )
  e[ e == 0 ] <- NA 
  
  per <- d / e
  
  top.p    <- 1 - (1-winsorize)/2
  bottom.p <- 0 + (1-winsorize)/2
  top      <- quantile( per, top.p, na.rm=T )
  bottom   <- quantile( per, bottom.p, na.rm=T )
  per.w    <- per
  per.w[ per.w > top    ] <- top
  per.w[ per.w < bottom ] <- bottom
  
  per.n <- scale( per.w )
  
  per.p <- dplyr::ntile( per, 100 )
  
  PER <- data.frame( per, per.w, per.n, per.p )
  
  print( summary( PER ) )
  
  par( mfrow=c(2,2) )
  plot( density(per,   na.rm=T), main="Program Efficienct Ratio (PER)" )
  plot( density(per.w, na.rm=T), main="PER Winsorized" )
  plot( density(per.n, na.rm=T), main="PER Standardized as Z" )
  plot( density(per.p, na.rm=T), main="PER as Percentile" )
  
  df.per <- data.frame( cbind( df, PER ) )
  return( df.per )
  }
