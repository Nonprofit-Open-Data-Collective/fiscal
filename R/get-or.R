###---------------------------------------------------
###   OPERATING MARGIN
###---------------------------------------------------

#' @title
#' Operating Margin
#'
#' @description
#' Calculate the operating margin and append it to the dataframe. 
#'
#' @param df A \code{data.frame} containing the required field for computing the metric. The metric will be appended to this dataset.
#' @param equity.eoy Unrestricted net assets, EOY (On 990: Part X, line 27B; On EZ: Not Available).
#' @param equity.boy Unrestricted net assets, BOY (On 990: Part X, line 27A; On EZ: Not Available).
#' 
#' @return Object of class \code{data.frame}: the original dataframe appended with the operating margin (`or`), 
#'  a winsorized version (`or.w`), a standardized z-score version (`or.z`), 
#'  and a percentile version (`or.p`).   
#'
#' @details The operating margin measures what percent of an organizations unrestricted net assets (or free 
#' cash) from the beginning of 2019 it had at the end of 2019. 
#' 
#' @examples
#' x1 <- rnorm( 1000, 100, 30 )
#' x2 <- rnorm( 1000, 200, 30 )
#' x2[ c( 15, 300, 600 ) ] <- 0
#' 
#' dat <- data.frame( x1, x2 )
#' 
#' # specify own column names
#' d <- get_or( df = dat, equity.eoy = "x1", equity.boy = "x2" )
#' 
#' head( d )
#' 
#' # run with default column names
#' x1[ seq( from = 1, to = 1000, 50 ) ] <- NA
#' x2[ seq( from = 1, to = 1000, 71 ) ] <- NA
#' 
#' dat_01 <- data.frame( x1, x2 )
#' 
#' colnames( dat_01 ) <- c( "F9_10_NAFB_UNRESTRICT_EOY", "F9_10_NAFB_UNRESTRICT_BOY" )
#' 
#' d <- get_or( dat_01 )
#' 
#' # coerce one column to factor
#' dat_01$F9_10_NAFB_UNRESTRICT_EOY <- as.factor( dat_01$F9_10_NAFB_UNRESTRICT_EOY )
#' 
#' d <- get_or( dat_01 )
#' 
#' # winsorize at 0.025 and 0.975 percentiles instead of 0.01 and 0.99
#' d <- get_or( df = dat, equity.eoy = "x1", equity.boy = "x2", winsorize = 0.95 )
#' 
#' d <- get_or( dat_01, winsorize = 0.95 )
#' 
#' 
#' # using 990 data
#' load( '/Volumes/My Passport for Mac/Urban Institute/Summer Projects/Fiscal/fiscal/R/sysdata.rda' )
#' d <- get_or( df = part010810 )
#' 
#' # now coerce one of the variables to numeric
#' part010810$F9_10_NAFB_UNRESTRICT_EOY <- as.character( part010810$F9_10_NAFB_UNRESTRICT_EOY )
#' 
#' d <- get_or( df = part010810 )
#' 
#' 
#' ## Errors ##
#' 
#' # numerator not specified
#' d <- get_or( df = dat, equity.eoy = NULL, equity.boy = "x2" )
#' 
#' # denominator not specified
#' d <- get_or( df = dat, equity.eoy = "x1", equity.boy = NULL )
#' 
#' # neither numerator nor denominator specified
#' d <- get_or( df = dat, equity.eoy = NULL, equity.boy = NULL )
#' 
#' # column names vector not of correct length
#' d <- get_or( df = dat, equity.eoy = c("e","b","c"), equity.boy = "e")
#' 
#' # column names vector not of correct length
#' d <- get_or( df = dat, equity.eoy = "e", equity.boy = c( "e", "b", "c") )
#' @export
get_or <- function( df, 
                    equity.eoy = "F9_10_NAFB_UNRESTRICT_EOY", 
                    equity.boy = "F9_10_NAFB_UNRESTRICT_BOY", 
                    winsorize=0.98 )
{
  # function checks
  if( winsorize > 1 | winsorize < 0 )
  { stop( "winsorize argument must be 0 < w < 1" ) }
  
  if( is.null( equity.eoy )==T & is.null( equity.boy )==F )
  { stop( "The numerator has been incorrectly specified. Ensure you are passing the correct data field to the correct argument." ) }
  
  if( is.null( equity.eoy )==F & is.null( equity.boy )==T )
  { stop( "The denominator has been incorrectly specified. Ensure you are passing the correct data field to the correct argument." ) }
  
  if( is.null( equity.eoy )==T & is.null( equity.boy )==T )
  { stop( "The argument fields are empty. Please supply column names for each argument or execute the function with default inputs." ) }
  
  if( length( equity.eoy ) > 2 | length( equity.eoy ) < 1 )
  { stop( "`equity.eoy` must be a single quoted string or a vector with a minimum length of one and maximum length of two." ) }
  
  if( length( equity.boy ) > 2 | length( equity.boy ) < 1 )
  { stop( "`equity.boy` must be a single quoted string or a vector with a minimum length of one and maximum length of two." ) }
  
  
  # copy data
  dat <- df
  
  ## ensure variable classes are numeric ##
  
  # run coerce_numeric and loop through all variables required and that matched by the two input arguments
  v <- c( colnames( dat )[which( colnames( dat ) %in% equity.boy ) ], colnames( dat )[which( colnames( dat ) %in% equity.eoy )] )
  dat <- coerce_numeric( d = dat, vars = v )
  
  
  # check to ensure both sets of variable names are included in input data when not specifying column names.
  # edge cases
  
  # BEGIN first outer conditional
  # BEGIN first outer conditional
  if ( sum( equity.eoy %in% c( "F9_10_NAFB_UNRESTRICT_EOY") )==2 & sum( equity.boy %in% c( "F9_10_NAFB_UNRESTRICT_BOY") )==2 ) {
    
    # BEGIN series of nested conditionals 
    
    
    # if at least one of the columns in missing from the input dataset, use only the column that is present
    if ( length( which( colnames( dat ) %in% equity.eoy ) )==1 | length( which( colnames( dat ) %in% equity.boy ) )==1 ) {
      
      if ( length( which(colnames( dat ) %in% equity.eoy ) )==2 ){
        
        # create a column that concatenates two numerator variables into single column if both columns for numerator are present
        dat[ which( is.na( dat[ equity.eoy[2] ] )==F ), "d" ] <- dat[ which( is.na( dat[ equity.eoy[2] ] )==F ), equity.eoy[2] ] 
        dat[ which( is.na( dat[ equity.eoy[1] ] )==F ), "d" ] <- dat[ which( is.na( dat[ equity.eoy[1] ] )==F ), equity.eoy[1] ] 
      }
      # if at least one of the numerator columns in missing from the input dataset, use only the column that is present
      if ( length( which( colnames( dat ) %in% equity.eoy ) )==1 ){
        
        this <- which( colnames( dat ) %in% equity.eoy )
        
        dat[ , "d" ] <- dat[ , this ]
      }
      
      
      if ( length( which( colnames( dat ) %in% equity.boy ) )==2 ){
        # create a column that concatenates two numerator variables into single column if both columns for denominator are present
        dat[ which( is.na( dat[ equity.boy[2] ] )==F ), "e"] <- dat[ which( is.na( dat[ equity.boy[2] ] )==F ), equity.boy[2] ] 
        dat[ which( is.na( dat[ equity.boy[1] ] )==F ), "e"] <- dat[ which( is.na( dat[ equity.boy[1] ] )==F ), equity.boy[1] ] 
      }
      # if at least one of the denominator columns in missing from the input dataset, use only the column that is present
      if ( length( which( colnames( dat ) %in% equity.boy ))==1  ){
        
        this <- which( colnames( dat ) %in% equity.boy )
        
        dat[ , "e" ] <- dat[ , this ]
      }
      # END series of nested conditionals 
      
      # now, assign p and e, respectively
      d <- dat[[ "d" ]]
      e <- dat[[ "e" ]]
    }
    
    # if all four columns are present, exit conditional
    if ( length( equity.eoy )==2 & length( equity.boy )==2 & length(which( colnames( dat ) %in% equity.eoy ) )==2 & length(which( colnames( dat ) %in% equity.boy ) )==2 ) {
      
      # create a column that concatenates two numerator variables into single column
      dat[ which( is.na( dat[ equity.eoy[2] ] )==F ), "d" ] <- dat[ which( is.na( dat[ equity.eoy[2] ] )==F ), equity.eoy[2] ]
      dat[ which( is.na( dat[ equity.eoy[1] ] )==F ), "d" ] <- dat[ which( is.na( dat[ equity.eoy[1] ] )==F ), equity.eoy[1] ]
      
      # create a column that concatenates two denominator variables into single column
      dat[ which( is.na( dat[ equity.boy[2] ] )==F ), "e"] <- dat[ which( is.na( dat[ equity.boy[2] ] )==F ), equity.boy[2] ]
      dat[ which( is.na( dat[ equity.boy[1] ] )==F ), "e"] <- dat[ which( is.na( dat[ equity.boy[1] ] )==F ), equity.boy[1] ]
      
      
      d <- dat[[ "d"]]
      e <- dat[[ "e"]]
    }
  }
  # END first outer conditional
  
  
  
  # all other cases are nestep in the following conditionals
  
  if ( sum( equity.eoy %in% c( "F9_10_NAFB_UNRESTRICT_EOY") )!=2 & sum( equity.boy %in% c( "F9_10_NAFB_UNRESTRICT_BOY") )!=2 ){          
    
    if ( length( equity.eoy )==2 & length( equity.boy )==2 ) {
      
      # create a column that concatenates two numerator variables into single column
      dat[ which( is.na( dat[ equity.eoy[2] ] )==F ), "d"] <- dat[ which( is.na( dat[ equity.eoy[2] ] )==F ), equity.eoy[2] ]
      dat[ which( is.na( dat[ equity.eoy[1] ] )==F ), "d"] <- dat[ which( is.na( dat[ equity.eoy[1] ] )==F ), equity.eoy[1] ]
      
      # create a column that concatenates two denominator variables into single column
      dat[ which( is.na( dat[ equity.boy[2] ] )==F ), "e"] <- dat[ which( is.na( dat[ equity.boy[2] ] )==F ), equity.boy[2] ]
      dat[ which( is.na( dat[ equity.boy[1] ] )==F ), "e"] <- dat[ which( is.na( dat[ equity.boy[1] ] )==F ), equity.boy[1] ]
      
      
      d <- dat[[ "d"]]
      e <- dat[[ "e"]]
    }
    
    else if ( length( equity.eoy )==2 & length( equity.boy )==1 ) {
      
      # create a column that concatenates two denominator variables into single column
      dat[ which( is.na( dat[ equity.eoy[2] ] )==F ), "d"] <- dat[ which( is.na( dat[ equity.eoy[2] ] )==F ), equity.eoy[2] ]
      dat[ which( is.na( dat[ equity.eoy[1] ] )==F ), "d"] <- dat[ which( is.na( dat[ equity.eoy[1] ] )==F ), equity.eoy[1] ]
      
      
      d <- dat[[ "d"]]
      e <- dat[[ equity.boy ]]
    }
    
    else if ( length( equity.eoy )==1 & length( equity.boy )==2 ) {
      
      # create a column that concatenates two numerator variables into single column
      dat[ which( is.na( dat[ equity.boy[2] ] )==F ), "e"] <- dat[ which( is.na( dat[ equity.boy[2] ] )==F ), equity.boy[2] ]
      dat[ which( is.na( dat[ equity.boy[1] ] )==F ), "e"] <- dat[ which( is.na( dat[ equity.boy[1] ] )==F ), equity.boy[1] ]
      
      
      d <- dat[[ equity.eoy ]]
      e <- dat[[ "e"]]
    }
    
    else if ( length( equity.eoy )==1 & length( equity.boy )==1 ) {
      
      d <- dat[[ equity.eoy ]]
      e <- dat[[ equity.boy ]]
    }
    
    
  }    
  
  # can't divide by zero
  print( paste0( "equity.boy cannot be equal to zero: ", sum( e==0, na.rm = T ), " cases have been replaced with NA." ) )
  e[ e == 0 ] <- NA 
  
  or <- d / e
  
  top.p    <- 1 - (1-winsorize)/2
  bottom.p <- 0 + (1-winsorize)/2
  top      <- quantile( or, top.p, na.rm=T )
  bottom   <- quantile( or, bottom.p, na.rm=T )
  or.w    <- or
  or.w[ or.w > top    ] <- top
  or.w[ or.w < bottom ] <- bottom
  
  or.n <- scale( or.w )
  
  or.p <- dplyr::ntile( or, 100 )
  
  OR <- data.frame( or, or.w, or.n, or.p )
  
  print( summary( OR ) )
  
  par( mfrow=c(2,2) )
  plot( density(or,   na.rm=T), main="Operating Margin (OR)" )
  plot( density(or.w, na.rm=T), main="OR Winsorized" )
  plot( density(or.n, na.rm=T), main="OR Standardized as Z" )
  plot( density(or.p, na.rm=T), main="OR as Percentile" )
  
  df.or <- data.frame( cbind( df, OR ) )
  return( df.or )
}

