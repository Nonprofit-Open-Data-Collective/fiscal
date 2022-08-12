###---------------------------------------------------
###   LAND TO ASSETS RATIO
###---------------------------------------------------

#' @title
#' Lands to Assets Ratio
#'
#' @description
#' Calculate the lands to assets ratio and append it to the dataframe. 
#'
#' @param df A \code{data.frame} containing the required field for computing the metric. The metric will be appended to this dataset.
#' @param land Land, buildings, and equipment (Part X, line 10B; On EZ: On EZ: Not Available).
#' @param assets Total assets (On 990: Part X, Line 16B; On EZ: Part II, line 25B).
#' @param winsorize The winsorization value (between 0 and 1), defaults to 0.98 which winsorizes at 99th and 1st percentile values.   
#' 
#' @usage get_lar( df, land = "F9_10_ASSET_LAND_BLDG_DEPREC", assets = "F9_10_ASSET_TOT_EOY", winsorize = 0.98 )
#'
#' @return Object of class \code{data.frame}: the original dataframe appended with the lands to assets ratio (`lar`), 
#'  a winsorized version (`lar.w`), a standardized z-score version (`lar.z`), 
#'  and a percentile version (`lar.p`).   
#'
#' @details This metric shows the proportion of an organization’s total assets that are made up by land and 
#' buildings. It shows how diversified their asset portfolio is and somewhat how specialized their line of 
#' work is within housing or building development. Higher values mean that an organization has more of its 
#' asset portfolio in land and buildings, and lower values mean that development or land and building holdings 
#' represent a small share of what the organization’s activities and finances are tied up in. Note: computation of 
#' this metric is available to full 990 filers only.
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
#' d <- get_lar( df = dat, land = "x1", assets = "x2" )
#' 
#' head( d )
#' 
#' # run with default column names
#' x1[ seq( from = 1, to = 1000, 50 ) ] <- NA
#' x2[ seq( from = 1, to = 1000, 71 ) ] <- NA
#' 
#' dat_01 <- data.frame( x1, x2 )
#' 
#' colnames( dat_01 ) <- c( "F9_10_ASSET_LAND_BLDG_DEPREC", "F9_10_ASSET_TOT_EOY" )
#' 
#' d <- get_lar( dat_01 )
#' 
#' # coerce one column to factor
#' dat_01$F9_10_ASSET_LAND_BLDG_DEPREC <- as.factor( dat_01$F9_10_ASSET_LAND_BLDG_DEPREC )
#' 
#' d <- get_lar( dat_01 )
#' 
#' # winsorize at 0.025 and 0.975 percentiles instead of 0.01 and 0.99
#' d <- get_lar( df = dat, land = "x1", assets = "x2", winsorize = 0.95 )
#' 
#' d <- get_lar( dat_01, winsorize = 0.95 )
#' 
#' 
#' # using 990 data
#' data( part010810 )
#' d <- get_lar( df = part010810 )
#' 
#' # now coerce one of the variables to numeric
#' part010810$F9_10_ASSET_LAND_BLDG_DEPREC <- as.character( part010810$F9_10_ASSET_LAND_BLDG_DEPREC )
#' 
#' d <- get_lar( df = part010810 )
#' 
#' \dontrun{
#' ## Errors ##
#' 
#' # numerator not specified
#' d <- get_lar( df = dat, land = NULL, assets = "x2" )
#' 
#' # denominator not specified
#' d <- get_lar( df = dat, land = "x1", assets = NULL )
#' 
#' # neither numerator nor denominator specified
#' d <- get_lar( df = dat, land = NULL, assets = NULL )
#' 
#' # column names vector not of correct length
#' d <- get_lar( df = dat, land = c("e","b","c"), assets = "e")
#' 
#' # column names vector not of correct length
#' d <- get_lar( df = dat, land = "e", assets = c( "e", "b", "c") )
#' }
#' @export
get_lar <- function( df, land = "F9_10_ASSET_LAND_BLDG_DEPREC", assets = "F9_10_ASSET_TOT_EOY", winsorize = 0.98 )
{
  
  # function checks
  if( winsorize > 1 | winsorize < 0 )
  { stop( "winsorize argument must be 0 < w < 1" ) }
  
  if( is.null( land )==T & is.null( assets )==F )
  { stop( "The numerator has been incorrectly specified. Ensure you are passing the correct data field to the correct argument." ) }
  
  if( is.null( land )==F & is.null( assets )==T )
  { stop( "The denominator has been incorrectly specified. Ensure you are passing the correct data field to the correct argument." ) }
  
  if( is.null( land )==T & is.null( assets )==T )
  { stop( "The argument fields are empty. Please supply column names for each argument or execute the function with default inputs." ) }
  
  if( length( land ) > 2 | length( land ) < 1 )
  { stop( "`land` must be a single quoted string or a vector with a minimum length of one and maximum length of two." ) }
  
  if( length( assets ) > 2 | length( assets ) < 1 )
  { stop( "`assets` must be a single quoted string or a vector with a minimum length of one and maximum length of two." ) }
  
  
  # copy data
  dat <- df
  
  ## ensure variable classes are numeric ##
  
  # run coerce_numeric and loop through all variables required and that matched by the two input arguments
  v <- c( colnames( dat )[which( colnames( dat ) %in% assets ) ], colnames( dat )[which( colnames( dat ) %in% land )] )
  dat <- coerce_numeric( d = dat, vars = v )
  
  
  # check to ensure both sets of variable names are included in input data when not specifying column names.
  # edge cases
  
  # BEGIN first outer conditional
  # BEGIN first outer conditional
  if ( sum( land %in% c( "F9_10_ASSET_LAND_BLDG_DEPREC") )==2 & sum( assets %in% c( "F9_10_ASSET_TOT_EOY") )==2 ) {
    
    # BEGIN series of nested conditionals 
    
    
    # if at least one of the columns in missing from the input dataset, use only the column that is present
    if ( length( which( colnames( dat ) %in% land ) )==1 | length( which( colnames( dat ) %in% assets ) )==1 ) {
      
      if ( length( which(colnames( dat ) %in% land ) )==2 ){
        
        # create a column that concatenates two numerator variables into single column if both columns for numerator are present
        dat[ which( is.na( dat[ land[2] ] )==F ), "d" ] <- dat[ which( is.na( dat[ land[2] ] )==F ), land[2] ] 
        dat[ which( is.na( dat[ land[1] ] )==F ), "d" ] <- dat[ which( is.na( dat[ land[1] ] )==F ), land[1] ] 
      }
      # if at least one of the numerator columns in missing from the input dataset, use only the column that is present
      if ( length( which( colnames( dat ) %in% land ) )==1 ){
        
        this <- which( colnames( dat ) %in% land )
        
        dat[ , "d" ] <- dat[ , this ]
      }
      
      
      if ( length( which( colnames( dat ) %in% assets ) )==2 ){
        # create a column that concatenates two numerator variables into single column if both columns for denominator are present
        dat[ which( is.na( dat[ assets[2] ] )==F ), "e"] <- dat[ which( is.na( dat[ assets[2] ] )==F ), assets[2] ] 
        dat[ which( is.na( dat[ assets[1] ] )==F ), "e"] <- dat[ which( is.na( dat[ assets[1] ] )==F ), assets[1] ] 
      }
      # if at least one of the denominator columns in missing from the input dataset, use only the column that is present
      if ( length( which( colnames( dat ) %in% assets ))==1  ){
        
        this <- which( colnames( dat ) %in% assets )
        
        dat[ , "e" ] <- dat[ , this ]
      }
      # ENp series of nestep conditionals 
      
      # now, assign p anp e, respectively
      d <- dat[[ "d" ]]
      e <- dat[[ "e" ]]
    }
    
    # if all four columns are present, exit conditional
    if ( length( land )==2 & length( assets )==2 & length(which( colnames( dat ) %in% land ) )==2 & length(which( colnames( dat ) %in% assets ) )==2 ) {
      
      # create a column that concatenates two numerator variables into single column
      dat[ which( is.na( dat[ land[2] ] )==F ), "d" ] <- dat[ which( is.na( dat[ land[2] ] )==F ), land[2] ]
      dat[ which( is.na( dat[ land[1] ] )==F ), "d" ] <- dat[ which( is.na( dat[ land[1] ] )==F ), land[1] ]
      
      # create a column that concatenates two denominator variables into single column
      dat[ which( is.na( dat[ assets[2] ] )==F ), "e"] <- dat[ which( is.na( dat[ assets[2] ] )==F ), assets[2] ]
      dat[ which( is.na( dat[ assets[1] ] )==F ), "e"] <- dat[ which( is.na( dat[ assets[1] ] )==F ), assets[1] ]
      
      
      d <- dat[[ "d"]]
      e <- dat[[ "e"]]
    }
  }
  # END first outer conditional
  
  
  
  # all other cases are nestep in the following conditionals
  
  if ( sum( land %in% c( "F9_10_ASSET_LAND_BLDG_DEPREC") )!=2 & sum( assets %in% c( "F9_10_ASSET_TOT_EOY") )!=2 ){          
    
    if ( length( land )==2 & length( assets )==2 ) {
      
      # create a column that concatenates two numerator variables into single column
      dat[ which( is.na( dat[ land[2] ] )==F ), "d"] <- dat[ which( is.na( dat[ land[2] ] )==F ), land[2] ]
      dat[ which( is.na( dat[ land[1] ] )==F ), "d"] <- dat[ which( is.na( dat[ land[1] ] )==F ), land[1] ]
      
      # create a column that concatenates two denominator variables into single column
      dat[ which( is.na( dat[ assets[2] ] )==F ), "e"] <- dat[ which( is.na( dat[ assets[2] ] )==F ), assets[2] ]
      dat[ which( is.na( dat[ assets[1] ] )==F ), "e"] <- dat[ which( is.na( dat[ assets[1] ] )==F ), assets[1] ]
      
      
      d <- dat[[ "d"]]
      e <- dat[[ "e"]]
    }
    
    else if ( length( land )==2 & length( assets )==1 ) {
      
      # create a column that concatenates two denominator variables into single column
      dat[ which( is.na( dat[ land[2] ] )==F ), "d"] <- dat[ which( is.na( dat[ land[2] ] )==F ), land[2] ]
      dat[ which( is.na( dat[ land[1] ] )==F ), "d"] <- dat[ which( is.na( dat[ land[1] ] )==F ), land[1] ]
      
      
      d <- dat[[ "d"]]
      e <- dat[[ assets ]]
    }
    
    else if ( length( land )==1 & length( assets )==2 ) {
      
      # create a column that concatenates two numerator variables into single column
      dat[ which( is.na( dat[ assets[2] ] )==F ), "e"] <- dat[ which( is.na( dat[ assets[2] ] )==F ), assets[2] ]
      dat[ which( is.na( dat[ assets[1] ] )==F ), "e"] <- dat[ which( is.na( dat[ assets[1] ] )==F ), assets[1] ]
      
      
      d <- dat[[ land ]]
      e <- dat[[ "e"]]
    }
    
    else if ( length( land )==1 & length( assets )==1 ) {
      
      d <- dat[[ land ]]
      e <- dat[[ assets ]]
    }
    
    
  }    
  
  # can't divide by zero
  print( paste0( "assets cannot be equal to zero: ", sum( e==0, na.rm = T ), " cases have been replaced with NA." ) )
  e[ e == 0 ] <- NA 
  
  lar <- d / e
  
  top.p    <- 1 - (1-winsorize)/2
  bottom.p <- 0 + (1-winsorize)/2
  top      <- quantile( lar, top.p, na.rm=T )
  bottom   <- quantile( lar, bottom.p, na.rm=T )
  lar.w    <- lar
  lar.w[ lar.w > top    ] <- top
  lar.w[ lar.w < bottom ] <- bottom
  
  lar.n <- scale( lar.w )
  
  lar.p <- dplyr::ntile( lar, 100 )
  
  LAR <- data.frame( lar, lar.w, lar.n, lar.p )
  
  print( summary( LAR ) )
  
  par( mfrow=c(2,2) )
  plot( density(lar,   na.rm=T), main="Land to Assets Ratio (LAR)" )
  plot( density(lar.w, na.rm=T), main="LAR Winsorized" )
  plot( density(lar.n, na.rm=T), main="LAR Standardized as Z" )
  plot( density(lar.p, na.rm=T), main="LAR as Percentile" )
  
  df.lar <- data.frame( cbind( df, LAR ) )
  return( df.lar )
}
