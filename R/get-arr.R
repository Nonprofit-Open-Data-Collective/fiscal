###---------------------------------------------------
###   ASSETS TO REVENUES RATIO
###---------------------------------------------------

#' @title
#' Assets to Revenues Ratio
#'
#' @description
#' Calculate the assets to revenues ratio and append it to the dataframe. 
#'
#'@usage get_arr( df, total.assets = "F9_10_ASSET_TOT_EOY", 
#' total.revenue = c( "F9_08_REV_TOT_TOT", "F9_01_REV_TOT_CY" ), winsorize = 0.98 )
#' 
#' @param df A \code{data.frame} containing the required field for computing the metric. The metric will be appended to this dataset.
#' @param total.assets A character string indicating the column name for total assets (On 990: Part X, Line 16B; On EZ: Pt II, Line 25B).
#' @param total.revenue A character string indicating the column name for total revenue (On 990: (Part VIII, Line 12A); On EZ: Part I, Line 9).
#' @param winsorize The winsorization value (between 0 and 1), defaults to 0.98 which winsorizes at 99th and 1st percentile values.   
#' 
#' @return Object of class \code{data.frame}: the original dataframe appended with the assets to revenues ratio (`arr`), 
#'  a winsorized version (`arr.w`), a stanarrdized z-score version (`arr.z`), 
#'  and a percentile version (`arr.p`).   
#'
#' @details This metric measures how many dollars of assets it has per dollar of annual revenues (how many 
#' dollars of revenue have been converted into assets in the past or that year for the current year). Generally, 
#' this ratio should be higher for organizations with large inventories of developments or large endowments.
#' Note: computation of this metric is available to both 990 and 990-EZ filers.
#' 
#' @import dplyr
#' @import stringr
#' @import magrittr
#' 
#' @examples
#' library( fiscal )
#' x1 <- rnorm( 1000,100,30 )
#' x2 <- rnorm( 1000,100,30 )
#' 
#' dat <- data.frame( x1, x2 )
#' 
#' # specify own column names
#' d <- get_arr( df = dat, total.assets = "x1", total.revenue = "x2" )
#' 
#' 
#' head( d )
#' 
#' # run with default column names
#' x3 <- rnorm( 1000,100,30 )
#' x3[ seq( from = 1, to = 1000, 50 ) ] <- NA
#' 
#' dat_01 <- data.frame( x1, x2, x3 )
#' 
#' colnames( dat_01 ) <- c( "F9_10_ASSET_TOT_EOY", "F9_08_REV_TOT_TOT",
#'                          "F9_01_REV_TOT_CY" )
#' 
#' d <- get_arr( dat_01 )
#' 
#' # specify only PC variables
#' d <- get_arr( dat_01, total.assets = "F9_10_ASSET_TOT_EOY", total.revenue = "F9_08_REV_TOT_TOT" )
#' 
#' # coerce one column to factor
#' dat_01$F9_10_ASSET_TOT_EOY <- as.factor( dat_01$F9_10_ASSET_TOT_EOY )
#' 
#' d <- get_arr( dat_01 )
#' 
#' # winsorize at 0.025 and 0.975 percentiles instead of 0.01 and 0.99
#' d <- get_arr( df = dat, total.assets = "x1", total.revenue ="x2", winsorize=0.95 )
#' 
#' d <- get_arr( dat_01, winsorize = 0.95 )
#' 
#' # assume only one PC variable for the denominator is present in the dataset and we run with default parameters
#' dat_02 <- dat_01
#' 
#' colnames( dat_02 ) <- c( "F9_10_ASSET_TOT_EOY", "F9_08_REV_TOT_TOT",
#'                          "x" )
#' 
#' d <- get_arr( dat_02, winsorize = 0.95 )
#' 
#' colnames( dat_02 ) <- c( "F9_10_ASSET_TOT_EOY", "x",
#'                          "F9_01_REV_TOT_CY" )
#' 
#' d <- get_arr( dat_02, winsorize = 0.95 )
#' 
#' # assume denominator variables are default names but numerator is defined
#' colnames( dat_02 ) <- c( "x1", "F9_08_REV_TOT_TOT",
#'                          "F9_01_REV_TOT_CY" )
#' 
#' d <- get_arr( dat_02, total.assets = "x1", winsorize = 0.95 )
#' 
#' 
#' # using 990 data
#' data( part010810 )
#' d <- get_arr(df=part010810)
#' 
#' # now coerce one of the variables to numeric
#' part010810$F9_10_ASSET_TOT_EOY <- as.character( part010810$F9_10_ASSET_TOT_EOY )
#' 
#' d <- get_arr(df=part010810)
#' 
#' \dontrun{
#' ## Errors ##
#' 
#' # numerator not specified
#' d <- get_arr( df = dat, total.assets = NULL, total.revenue = "x2")
#' 
#' # denominator not specified
#' d <- get_arr( df = dat, total.assets = "x1", total.revenue = NULL )
#' 
#' # neither numerator nor denominator specified
#' d <- get_arr( df = dat, total.assets = NULL, total.revenue = NULL )
#' }
#' 
#' @export
get_arr <- function( df, total.assets = "F9_10_ASSET_TOT_EOY", 
                     total.revenue = c( "F9_08_REV_TOT_TOT", "F9_01_REV_TOT_CY" ), winsorize = 0.98 )
{
  # function checks
  if( winsorize > 1 | winsorize < 0 )
  { stop( "winsorize argument must be 0 < w < 1" ) }
  
  if( is.null( total.assets )==T & is.null( total.revenue )==F )
  { stop( "The numerator has been incorrectly specified. Ensure you are passing the correct data field to the correct argument." ) }
  
  if( is.null( total.assets )==F & is.null( total.revenue )==T )
  { stop( "The denominator has been incorrectly specified. Ensure you are passing the correct data field to the correct argument." ) }
  
  if( is.null( total.assets )==T & is.null( total.revenue )==T )
  { stop( "The argument fields are empty. Please supply column names for each argument or execute the function with default inputs." ) }
  
  if( length( total.assets ) > 2 | length( total.assets ) < 1 )
  { stop( "`total.assets` must be a single quoted string or a vector with a minimum length of one and maximum length of two." ) }
  
  if( length( total.revenue ) > 2 | length( total.revenue ) < 1 )
  { stop( "`total.revenue` must be a single quoted string or a vector with a minimum length of one and maximum length of two." ) }
 
  if( length( total.revenue ) > 2 | length( total.revenue ) < 1 )
  { stop( "`total.revenue` must be a single quoted string or a vector with a minimum length of one and maximum length of two." ) }
  
  
  # copy data
  dat <- df
  
  ## ensure variable classes are numeric ##
  
  # run coerce_numeric and loop through all variables required and that matched by the two input arguments
  v <- c( colnames( dat )[which( colnames( dat ) %in% total.revenue ) ], colnames( dat )[which( colnames( dat ) %in% total.assets )] )
  dat <- coerce_numeric( d = dat, vars = v )
  
  
  # check to ensure both sets of variable names are included in input data when not specifying column names.
  # edge cases
  
  # BEGIN first outer conditional
  if ( sum( total.assets %in% c( "F9_10_ASSET_TOT_EOY") )==2 & sum( total.revenue %in% c( "F9_08_REV_TOT_TOT", "F9_01_REV_TOT_CY") )==2 ) {
    
    # BEGIN series of nested conditionals 
    
    
    # if at least one of the columns in missing from the input dataset, use only the column that is present
    if ( length( which( colnames( dat ) %in% total.assets ) )==1 | length( which( colnames( dat ) %in% total.revenue ) )==1 ) {
      
      if ( length( which(colnames( dat ) %in% total.assets ) )==2 ){
        
        # create a column that concatenates two numerator variables into single column if both columns for numerator are present
        dat[ which( is.na( dat[ total.assets[2] ] )==F ), "d"] <- dat[ which( is.na( dat[ total.assets[2] ] )==F ), total.assets[2] ] 
        dat[ which( is.na( dat[ total.assets[1] ] )==F ), "d"] <- dat[ which( is.na( dat[ total.assets[1] ] )==F ), total.assets[1] ] 
      }
      # if at least one of the numerator columns in missing from the input dataset, use only the column that is present
      if ( length( which( colnames( dat ) %in% total.assets ) )==1 ){
        
        this <- which( colnames( dat ) %in% total.assets )
        
        dat[ , "d"] <- dat[ , this ]
      }
      
      
      if ( length( which( colnames( dat ) %in% total.revenue ) )==2 ){
        # create a column that concatenates two numerator variables into single column if both columns for denominator are present
        dat[ which( is.na( dat[ total.revenue[2] ] )==F ), "e"] <- dat[ which( is.na( dat[ total.revenue[2] ] )==F ), total.revenue[2] ] 
        dat[ which( is.na( dat[ total.revenue[1] ] )==F ), "e"] <- dat[ which( is.na( dat[ total.revenue[1] ] )==F ), total.revenue[1] ] 
      }
      # if at least one of the denominator columns in missing from the input dataset, use only the column that is present
      if ( length( which( colnames( dat ) %in% total.revenue ) )==1  ){
        
        this <- which( colnames( dat ) %in% total.revenue )
        
        dat[ , "e"] <- dat[ , this ]
      }
      # END series of nested conditionals 
      
      # now, assign p anp e, respectively
      d <- dat[[ "d"]]
      e <- dat[[ "e"]]
    }
    
    # if all four columns are present, exit conditional
    if ( length( total.assets )==2 & length( total.revenue )==2 & length(which( colnames( dat ) %in% total.assets ) )==2 & length(which( colnames( dat ) %in% total.revenue ) )==2 ) {
      
      # create a column that concatenates two numerator variables into single column
      dat[ which( is.na( dat[ total.assets[2] ] )==F ), "d"] <- dat[ which( is.na( dat[ total.assets[2] ] )==F ), total.assets[2] ]
      dat[ which( is.na( dat[ total.assets[1] ] )==F ), "d"] <- dat[ which( is.na( dat[ total.assets[1] ] )==F ), total.assets[1] ]
      
      # create a column that concatenates two denominator variables into single column
      dat[ which( is.na( dat[ total.revenue[2] ] )==F ), "e"] <- dat[ which( is.na( dat[ total.revenue[2] ] )==F ), total.revenue[2] ]
      dat[ which( is.na( dat[ total.revenue[1] ] )==F ), "e"] <- dat[ which( is.na( dat[ total.revenue[1] ] )==F ), total.revenue[1] ]
      
      
      d <- dat[[ "d"]]
      e <- dat[[ "e"]]
    }
  }
  # END first outer conditional
  
  
  
  # all other cases are nested in the following set of conditionals
  
  if ( sum( total.assets %in% c( "F9_10_ASSET_TOT_EOY") )!=2 | sum( total.revenue %in% c( "F9_08_REV_TOT_TOT", "F9_01_REV_TOT_CY") )!=2 ){          
    
    if ( length( total.assets )==2 & length( total.revenue )==2 ) {
      
      # create a column that concatenates two numerator variables into single column
      dat[ which( is.na( dat[ total.assets[2] ] )==F ), "d"] <- dat[ which( is.na( dat[ total.assets[2] ] )==F ), total.assets[2] ]
      dat[ which( is.na( dat[ total.assets[1] ] )==F ), "d"] <- dat[ which( is.na( dat[ total.assets[1] ] )==F ), total.assets[1] ]
      
      # create a column that concatenates two denominator variables into single column
      dat[ which( is.na( dat[ total.revenue[2] ] )==F ), "e"] <- dat[ which( is.na( dat[ total.revenue[2] ] )==F ), total.revenue[2] ]
      dat[ which( is.na( dat[ total.revenue[1] ] )==F ), "e"] <- dat[ which( is.na( dat[ total.revenue[1] ] )==F ), total.revenue[1] ]
      
      
      d <- dat[[ "d"]]
      e <- dat[[ "e"]]
    }
    
    else if ( length( total.assets )==2 & length( total.revenue )==1 ) {
      
      # create a column that concatenates two denominator variables into single column
      dat[ which( is.na( dat[ total.assets[2] ] )==F ), "d"] <- dat[ which( is.na( dat[ total.assets[2] ] )==F ), total.assets[2] ]
      dat[ which( is.na( dat[ total.assets[1] ] )==F ), "d"] <- dat[ which( is.na( dat[ total.assets[1] ] )==F ), total.assets[1] ]
      
      
      d <- dat[[ "d"]]
      e <- dat[[ total.revenue ]]
    }
    
    # missing one of the denominator variables
    else if ( ( length( total.assets )==1 & length( total.revenue )==2 ) &
              sum( colnames( dat ) %in% c( "F9_08_REV_TOT_TOT", "F9_01_REV_TOT_CY") )==1 ){
      
      warning( "One denominator variable detected in the dataset but not the other. Ratios calculated in the presence of the missing variable. Check to ensure data is sound.")
      
      d <- dat[[ total.assets ]]
      e <- dat[[ total.revenue[ which( total.revenue %in% colnames( dat ) ) ] ]]
    }
    
    
    else if ( length( total.assets )==1 & length( total.revenue )==2 ) {
      
      # create a column that concatenates two numerator variables into single column
      dat[ which( is.na( dat[ total.revenue[2] ] )==F ), "e"] <- dat[ which( is.na( dat[ total.revenue[2] ] )==F ), total.revenue[2] ]
      dat[ which( is.na( dat[ total.revenue[1] ] )==F ), "e"] <- dat[ which( is.na( dat[ total.revenue[1] ] )==F ), total.revenue[1] ]
      
      
      d <- dat[[ total.assets ]]
      e <- dat[[ "e"]]
    }
    
    else if ( length( total.assets )==1 & length( total.revenue )==1 ) {
      
      d <- dat[[ total.assets ]]
      e <- dat[[ total.revenue ]]
    }
    
    
  }

  # can't divide by zero
  print( paste0( "Total revenue cannot be equal to zero: ", sum( e==0, na.rm = T ), " cases have been replaced with NA." ) )
  e[ e == 0 ] <- NA 
  
  arr <- d / e
  
  top.p    <- 1 - (1-winsorize)/2
  bottom.p <- 0 + (1-winsorize)/2
  top      <- quantile( arr, top.p, na.rm=T )
  bottom   <- quantile( arr, bottom.p, na.rm=T )
  arr.w    <- arr
  arr.w[ arr.w > top    ] <- top
  arr.w[ arr.w < bottom ] <- bottom
  
  arr.n <- scale( arr.w )
  
  arr.p <- dplyr::ntile( arr, 100 )
  
  ARR <- data.frame( arr, arr.w, arr.n, arr.p )
  
  print( summary( ARR ) )
  
  par( mfrow=c(2,2) )
  plot( density(arr,   na.rm=T), main="Assets to Revenue Ratio (ARR)" )
  plot( density(arr.w, na.rm=T), main="ARR Winsorized" )
  plot( density(arr.n, na.rm=T), main="ARR Standardized as Z" )
  plot( density(arr.p, na.rm=T), main="ARR as Percentile" )
  
  df.arr <- data.frame( cbind( df, ARR ) )
  return( df.arr )

}
