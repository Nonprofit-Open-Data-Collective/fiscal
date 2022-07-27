###---------------------------------------------------
###   EQUITY RATIO
###---------------------------------------------------

#' @title
#' Equity Ratio 
#'
#' @description
#' Calculate the equity ratio and append it to the dataframe. 
#'
#' @param df A \code{data.frame} containing the required field for computing the metric. The metric will be appended to this dataset with the default name supplied.
#' @param net.assets A character string indicating the column name for net assets, EOY (On 990: Part X, Line 33B; On EZ: Part I, Line 21) with the default name supplied.
#' @param total.assets A character string indicating the column name for total assets, EOY (On 990: Part X, Line 16B; On EZ: Part II, Line 25B) with the default name supplied.
#' @param winsorize The winsorization value (between 0 and 1), defaults to 0.98 which winsorizes at 99th and 1st percentile values.   
#' 
#' @return Object of class \code{data.frame}: the original dataframe appended with the equity ratio (`er`), 
#'  a winsorized version (`er.w`), a standardized z-score version (`er.z`), 
#'  and a percentile version (`er.p`).   
#'
#' @details This metric indicates how much of an organization’s assets are owned free and clear or how much 
#' equity it has in its total assets. Nonprofits with greater amounts of equity are more flexible in the face 
#' of financial shocks than organizations with comparatively lesser amounts of equity because they can (1) 
#' borrow money from capital markets and (2) convert those assets to cash to offset financial shocks. High 
#' values in this indicator are generally better, as they show that an organization has substantial equity in 
#' its assets. Low or negative values indicate an organization has higher liabilities and is generally more 
#' leveraged and thus more vulnerable to shocks. However, low values also indicate that an organization may 
#' be investing more of its equity for growth. Note: computation of this metric is available to both 990 and 990-EZ filers.
#' 
#' @examples
#' x1 <- rnorm( 1000, 100, 30 )
#' x2 <- rnorm( 1000, 200, 30 )
#' x2[ c( 15, 300, 600 ) ] <- 0
#' 
#' dat <- data.frame( x1, x2 )
#' 
#' # specify own column names
#' d <- get_er( df = dat, net.assets = "x1", total.assets = "x2" )
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
#' colnames( dat_01 ) <- c( "F9_10_NAFB_TOT_EOY", "F9_10_ASSET_TOT_EOY" )
#' 
#' d <- get_er( dat_01 )
#' 
#' # coerce one column to factor
#' dat_01$F9_10_NAFB_TOT_EOY <- as.factor( dat_01$F9_10_NAFB_TOT_EOY )
#' 
#' d <- get_er( dat_01 )
#' 
#' # winsorize at 0.025 and 0.975 percentiles instead of 0.01 and 0.99
#' d <- get_er( df = dat, net.assets = "x1", total.assets = "x2", winsorize = 0.95 )
#' 
#' d <- get_er( dat_01, winsorize = 0.95 )
#' 
#' 
#' # using 990 data
#' load( '/Volumes/My Passport for Mac/Urban Institute/Summer Projects/Fiscal/fiscal/R/sysdata.rda' )
#' d <- get_er( df = part010810 )
#' 
#' # now coerce one of the variables to numeric
#' part010810$F9_01_EXP_TOT_CY <- as.character( part010810$F9_01_EXP_TOT_CY )
#' 
#' d <- get_er( df = part010810 )
#' 
#' 
#' ## Errors ##
#' 
#' # numerator not specified
#' d <- get_er( df = dat, net.assets = NULL, total.assets = "x2" )
#' 
#' # denominator not specified
#' d <- get_er( df = dat, net.assets = "x1", total.assets = NULL )
#' 
#' # neither numerator nor denominator specified
#' d <- get_er( df = dat, net.assets = NULL, total.assets = NULL )
#' 
#' # column names vector not of correct length
#' d <- get_er( df = dat, net.assets = c("e","b","c"), total.assets = "e")
#' 
#' # column names vector not of correct length
#' d <- get_er( df = dat, net.assets = "e", total.assets = c( "e", "b", "c") )
#' @export
get_er <- function( df, net.assets = 'F9_10_NAFB_TOT_EOY', total.assets = 'F9_10_ASSET_TOT_EOY', winsorize=0.98 )
{

  # function checks
  if( winsorize > 1 | winsorize < 0 )
  { stop( "winsorize argument must be 0 < w < 1" ) }
  
  if( is.null( net.assets )==T & is.null( total.assets )==F )
  { stop( "The numerator has been incorrectly specified. Ensure you are passing the correct data field to the correct argument." ) }
  
  if( is.null( net.assets )==F & is.null( total.assets )==T )
  { stop( "The denominator has been incorrectly specified. Ensure you are passing the correct data field to the correct argument." ) }
  
  if( is.null( net.assets )==T & is.null( total.assets )==T )
  { stop( "The argument fields are empty. Please supply column names for each argument or execute the function with default inputs." ) }
  
  if( length( net.assets ) > 2 | length( net.assets ) < 1 )
  { stop( "`net.assets` must be a single quoted string or a vector with a minimum length of one and maximum length of two." ) }
  
  if( length( total.assets ) > 2 | length( total.assets ) < 1 )
  { stop( "`total.assets` must be a single quoted string or a vector with a minimum length of one and maximum length of two." ) }
  
  
  # copy data
  dat <- df
  
  ## ensure variable classes are numeric ##
  
  # run coerce_numeric and loop through all variables required and that matched by the two input arguments
  v <- c( colnames( dat )[which( colnames( dat ) %in% total.assets ) ], colnames( dat )[which( colnames( dat ) %in% net.assets )] )
  dat <- coerce_numeric( d = dat, vars = v )
  
  
  # check to ensure both sets of variable names are included in input data when not specifying column names.
  # edge cases
  
  # BEGIN first outer conditional
  # BEGIN first outer conditional
  if ( sum( net.assets %in% c( "F9_10_NAFB_TOT_EOY") )==2 & sum( total.assets %in% c( "F9_10_ASSET_TOT_EOY") )==2 ) {
    
    # BEGIN series of nested conditionals 
    
    
    # if at least one of the columns in missing from the input dataset, use only the column that is present
    if ( length( which( colnames( dat ) %in% net.assets ) )==1 | length( which( colnames( dat ) %in% total.assets ) )==1 ) {
      
      if ( length( which(colnames( dat ) %in% net.assets ) )==2 ){
        
        # create a column that concatenates two numerator variables into single column if both columns for numerator are present
        dat[ which( is.na( dat[ net.assets[2] ] )==F ), "d"] <- dat[ which( is.na( dat[ net.assets[2] ] )==F ), net.assets[2] ] 
        dat[ which( is.na( dat[ net.assets[1] ] )==F ), "d"] <- dat[ which( is.na( dat[ net.assets[1] ] )==F ), net.assets[1] ] 
      }
      # if at least one of the numerator columns in missing from the input dataset, use only the column that is present
      if ( length( which( colnames( dat ) %in% net.assets ) )==1 ){
        
        this <- which( colnames( dat ) %in% net.assets )
        
        dat[ , "d"] <- dat[ , this ]
      }
      
      
      if ( length( which( colnames( dat ) %in% total.assets ) )==2 ){
        # create a column that concatenates two numerator variables into single column if both columns for denominator are present
        dat[ which( is.na( dat[ total.assets[2] ] )==F ), "e"] <- dat[ which( is.na( dat[ total.assets[2] ] )==F ), total.assets[2] ] 
        dat[ which( is.na( dat[ total.assets[1] ] )==F ), "e"] <- dat[ which( is.na( dat[ total.assets[1] ] )==F ), total.assets[1] ] 
      }
      # if at least one of the denominator columns in missing from the input dataset, use only the column that is present
      if ( length( which( colnames( dat ) %in% total.assets ))==1  ){
        
        this <- which( colnames( dat ) %in% total.assets )
        
        dat[ , "e"] <- dat[ , this ]
      }
      # ENp series of nestep conditionals 
      
      # now, assign p anp e, respectively
      d <- dat[[ "d"]]
      e <- dat[[ "e"]]
    }
    
    # if all four columns are present, exit conditional
    if ( length( net.assets )==2 & length( total.assets )==2 & length(which( colnames( dat ) %in% net.assets ) )==2 & length(which( colnames( dat ) %in% total.assets ) )==2 ) {
      
      # create a column that concatenates two numerator variables into single column
      dat[ which( is.na( dat[ net.assets[2] ] )==F ), "d"] <- dat[ which( is.na( dat[ net.assets[2] ] )==F ), net.assets[2] ]
      dat[ which( is.na( dat[ net.assets[1] ] )==F ), "d"] <- dat[ which( is.na( dat[ net.assets[1] ] )==F ), net.assets[1] ]
      
      # create a column that concatenates two denominator variables into single column
      dat[ which( is.na( dat[ total.assets[2] ] )==F ), "e"] <- dat[ which( is.na( dat[ total.assets[2] ] )==F ), total.assets[2] ]
      dat[ which( is.na( dat[ total.assets[1] ] )==F ), "e"] <- dat[ which( is.na( dat[ total.assets[1] ] )==F ), total.assets[1] ]
      
      
      d <- dat[[ "d"]]
      e <- dat[[ "e"]]
    }
  }
  # END first outer conditional
  
  
  
  # all other cases are nestep in the following conditionals
  
  if ( sum( net.assets %in% c( "F9_10_NAFB_TOT_EOY") )!=2 & sum( total.assets %in% c( "F9_10_ASSET_TOT_EOY") )!=2 ){          
    
    if ( length( net.assets )==2 & length( total.assets )==2 ) {
      
      # create a column that concatenates two numerator variables into single column
      dat[ which( is.na( dat[ net.assets[2] ] )==F ), "d"] <- dat[ which( is.na( dat[ net.assets[2] ] )==F ), net.assets[2] ]
      dat[ which( is.na( dat[ net.assets[1] ] )==F ), "d"] <- dat[ which( is.na( dat[ net.assets[1] ] )==F ), net.assets[1] ]
      
      # create a column that concatenates two denominator variables into single column
      dat[ which( is.na( dat[ total.assets[2] ] )==F ), "e"] <- dat[ which( is.na( dat[ total.assets[2] ] )==F ), total.assets[2] ]
      dat[ which( is.na( dat[ total.assets[1] ] )==F ), "e"] <- dat[ which( is.na( dat[ total.assets[1] ] )==F ), total.assets[1] ]
      
      
      d <- dat[[ "d"]]
      e <- dat[[ "e"]]
    }
    
    else if ( length( net.assets )==2 & length( total.assets )==1 ) {
      
      # create a column that concatenates two denominator variables into single column
      dat[ which( is.na( dat[ net.assets[2] ] )==F ), "d"] <- dat[ which( is.na( dat[ net.assets[2] ] )==F ), net.assets[2] ]
      dat[ which( is.na( dat[ net.assets[1] ] )==F ), "d"] <- dat[ which( is.na( dat[ net.assets[1] ] )==F ), net.assets[1] ]
      
      
      d <- dat[[ "d"]]
      e <- dat[[ total.assets ]]
    }
    
    else if ( length( net.assets )==1 & length( total.assets )==2 ) {
      
      # create a column that concatenates two numerator variables into single column
      dat[ which( is.na( dat[ total.assets[2] ] )==F ), "e"] <- dat[ which( is.na( dat[ total.assets[2] ] )==F ), total.assets[2] ]
      dat[ which( is.na( dat[ total.assets[1] ] )==F ), "e"] <- dat[ which( is.na( dat[ total.assets[1] ] )==F ), total.assets[1] ]
      
      
      d <- dat[[ net.assets ]]
      e <- dat[[ "e"]]
    }
    
    else if ( length( net.assets )==1 & length( total.assets )==1 ) {
      
      d <- dat[[ net.assets ]]
      e <- dat[[ total.assets ]]
    }
    
    
  }    
  
  # can't divide by zero
  print( paste0( "Total assets cannot be equal to zero: ", sum( e==0, na.rm = T ), " cases have been replaced with NA." ) )
  e[ e == 0 ] <- NA 
  
  er <- d / e
  
  top.p    <- 1 - (1-winsorize)/2
  bottom.p <- 0 + (1-winsorize)/2
  top      <- quantile( er, top.p, na.rm=T )
  bottom   <- quantile( er, bottom.p, na.rm=T )
  er.w    <- er
  er.w[ er.w > top    ] <- top
  er.w[ er.w < bottom ] <- bottom
  
  er.n <- scale( er.w )
  
  er.p <- dplyr::ntile( er, 100 )
  
  ER <- data.frame( er, er.w, er.n, er.p )
  
  print( summary( ER ) )
  
  par( mfrow=c(2,2) )
  plot( density(er,   na.rm=T), main="Equity Ratio (ER)" )
  plot( density(er.w, na.rm=T), main="ER Winsorized" )
  plot( density(er.n, na.rm=T), main="ER Standardized as Z" )
  plot( density(er.p, na.rm=T), main="ER as Percentile" )
  
  df.er <- data.frame( cbind( df, ER ) )
  return( df.er )
}



