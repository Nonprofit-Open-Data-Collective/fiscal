###---------------------------------------------------
###   DEBT TO ASSET RATIO
###---------------------------------------------------

#' @title
#' Debt to Asset Ratio 
#'
#' @description
#' Calculate the debt to asset ratio and append it to the dataframe. 
#'
#' @param df A \code{data.frame} containing the required field for computing the metric. The metric will be appended to this dataset.
#' @param debt Column name(s) for total liabilities (must be quoted). (On 990: Part X, line 26B; On EZ: Part II, line 26B) with the default name supplied.
#' @param assets Column name(s) for total assets, EOY (must be quoted). (On 990: Part X, line 16B; On EZ: Part II, line 25B) with the default name supplied.
#' @param winsorize The winsorization value (between 0 and 1), defaults to 0.98 which winsorizes at 99th and 1st percentile values.   
#' 
#' @usage get_dar( df, c( "F9_10_LIAB_TOT_EOY", "F9_01_NAFB_LIAB_TOT_EOY") , assets = c("F9_10_ASSET_TOT_EOY", "F9_01_NAFB_ASSET_TOT_EOY"), winsorize = 0.98 )
#' 
#' @return Object of class \code{data.frame}: the original dataframe appended with the debt to asset ratio (`dar`), 
#'  a winsorized version (`dar.w`), a standardized z-score version (`dar.z`), 
#'  and a percentile version (`dar.p`).   
#'
#' @details Total-debt-to-total-assets is a leverage ratio that defines the total 
#'  amount of debt relative to assets owned by a company. Using this metric, 
#'  analysts can compare one company's leverage with that of other companies 
#'  in the same industry. This information can reflect how financially stable 
#'  a company is. The higher the ratio, the higher the degree of leverage (DoL) and, 
#'  consequently, the higher the risk (\href{Investopedia}{https://www.investopedia.com/terms/t/totaldebttototalassets.asp}).
#'  Note: computation of this metric is available to both 990 and 990-EZ filers.
#' 
#' 
#' @examples
#' x1 <- rnorm( 1000,100,30 )
#' x2 <- rnorm( 1000,200,30 )
#' x2[ c(15,300,600) ] <- 0
#' 
#' dat <- data.frame( x1,x2 )
#' 
#' # specify own column names
#' d <- get_dar( df = dat, debt = "x1", assets = "x2" )
#' 
#' 
#' head( d )
#' 
#' # run with default column names
#' x3 <- rnorm( 1000,100,30 )
#' x4 <- rnorm( 1000,200,30 )
#' x3[ seq( from = 1, to = 1000, 50 ) ] <- NA
#' x4[ seq( from = 1, to = 1000, 71 ) ] <- NA
#' 
#' dat_01 <- data.frame( x1, x2, x3, x4 )
#' 
#' colnames( dat_01 ) <- c( "F9_10_LIAB_TOT_EOY", "F9_01_NAFB_LIAB_TOT_EOY",
#'                          "F9_10_ASSET_TOT_EOY", "F9_01_NAFB_ASSET_TOT_EOY")
#' 
#' d <- get_dar( dat_01 )
#' 
#' # specify only PC variables
#' d <- get_dar( dat_01, debt = "F9_10_LIAB_TOT_EOY", assets = "F9_10_ASSET_TOT_EOY" )
#' 
#' # coerce one column to factor
#' dat_01$F9_10_LIAB_TOT_EOY <- as.factor( dat_01$F9_10_LIAB_TOT_EOY )
#' 
#' d <- get_dar( dat_01 )
#' 
#' # winsorize at 0.025 and 0.975 percentiles instead of 0.01 and 0.99
#' d <- get_dar( df = dat, debt = "x1", assets ="x2", winsorize=0.95 )
#' 
#' d <- get_dar( dat_01, winsorize = 0.95 )
#' 
#' # assume only one PC variable for the numerator or denominator is present in the dataset and we run with default parameters
#' dat_02 <- dat_01
#' 
#' colnames( dat_02 ) <- c( "F9_10_LIAB_TOT_EOY", "F9_01_NAFB_LIAB_TOT_EOY",
#'                          "x", "F9_01_NAFB_ASSET_TOT_EOY")
#' 
#' 
#' d <- get_dar( dat_02, winsorize = 0.95 )
#' 
#' colnames( dat_02 ) <- c( "F9_10_LIAB_TOT_EOY", "x",
#'                          "F9_10_ASSET_TOT_EOY", "F9_01_NAFB_ASSET_TOT_EOY")
#' 
#' d <- get_dar( dat_02, winsorize = 0.95 )
#' 
#' 
#' # using 990 data
#' 
#' d <- get_dar(df=part010810)
#' 
#' # now coerce one of the variables to numeric
#' part010810$F9_01_NAFB_ASSET_TOT_EOY <- as.character( part010810$F9_01_NAFB_ASSET_TOT_EOY )
#' 
#' d <- get_dar(df=part010810)
#' 
#' 
#' ## Errors ##
#' 
#' # numerator not specified
#' d <- get_dar( df = dat, debt = NULL, assets = "x2")
#' 
#' # denominator not specified
#' d <- get_dar( df = dat, debt = "x1", assets = NULL )
#' 
# neither numerator nor denominator specified
#' d <- get_dar( df = dat, debt = NULL, assets = NULL )
#' @export
get_dar <- function( df, 
                     debt = c( "F9_10_LIAB_TOT_EOY", "F9_01_NAFB_LIAB_TOT_EOY") , 
                     assets = c("F9_10_ASSET_TOT_EOY", "F9_01_NAFB_ASSET_TOT_EOY"), 
                     winsorize=0.98 )
{
  # checks
  if( winsorize > 1 | winsorize < 0 )
  { stop( "winsorize argument must be 0 < w < 1" ) }
  
  if( is.null( debt )==T & is.null( assets )==F )
  { stop( "The numerator has been incorrectly specified. Ensure you are passing the correct data field to the correct argument." ) }
  
  if( is.null( debt )==F & is.null( assets )==T )
  { stop( "The denominator has been incorrectly specified. Ensure you are passing the correct data field to the correct argument." ) }
  
  if( is.null( debt )==T & is.null( assets )==T )
  { stop( "The argument fields are empty. Please supply column names for each argument or execute the function with default inputs." ) }
  
  # copy data
  dat <- df
  
   
  
  ## ensure variable classes are numeric ##
  
  # run coerce_numeric and loop through all variables required and that matched by the two input arguments
  v <- c( colnames( dat )[which( colnames( dat ) %in% assets)], colnames( dat )[which( colnames( dat ) %in% debt)] )
  dat <- coerce_numeric( d = dat, vars = v )

  
  # check to ensure both sets of variable names are included in input data when not specifying column names.
  # edge cases
  
  # BEGIN first outer conditional
  # BEGIN first outer conditional
  if ( sum( debt %in% c( "F9_10_LIAB_TOT_EOY", "F9_01_NAFB_LIAB_TOT_EOY") )==2 & sum( assets %in% c( "F9_10_ASSET_TOT_EOY", "F9_01_NAFB_ASSET_TOT_EOY") )==2 ) {
    
    # BEGIN series of nested conditionals 
    
    
    # if at least one of the columns in missing from the input dataset, use only the column that is present
    if ( length( which( colnames( dat ) %in% debt ) )==1 | length( which( colnames( dat ) %in% assets ) )==1 ) {
      
      if ( length( which(colnames( dat ) %in% debt ) )==2 ){
        
        # create a column that concatenates two numerator variables into single column if both columns for numerator are present
        dat[ which( is.na( dat[ debt[2] ] )==F ), "d"] <- as.numeric( as.character( dat[ which( is.na( dat[ debt[2] ] )==F ), debt[2] ] ) )
        dat[ which( is.na( dat[ debt[1] ] )==F ), "d"] <- as.numeric( as.character( dat[ which( is.na( dat[ debt[1] ] )==F ), debt[1] ] ) )
      }
      # if at least one of the numerator columns in missing from the input dataset, use only the column that is present
       if ( length( which( colnames( dat ) %in% debt ) )==1 ){
        
        this <- which( colnames( dat ) %in% debt )
        
        dat[ , "d"] <- dat[ , this ]
      }
      
      
      if ( length( which( colnames( dat ) %in% assets ) )==2 ){
        # create a column that concatenates two numerator variables into single column if both columns for denominator are present
        dat[ which( is.na( dat[ assets[2] ] )==F ), "a"] <- as.numeric( as.character( dat[ which( is.na( dat[ assets[2] ] )==F ), assets[2] ] ) )
        dat[ which( is.na( dat[ assets[1] ] )==F ), "a"] <- as.numeric( as.character( dat[ which( is.na( dat[ assets[1] ] )==F ), assets[1] ] ) )
      }
      # if at least one of the denominator columns in missing from the input dataset, use only the column that is present
       if ( length( which( colnames( dat ) %in% assets ))==1  ){
        
        this <- which( colnames( dat ) %in% assets )
        
        dat[ , "a"] <- dat[ , this ]
      }
      # END series of nested conditionals 
      
      # now, assign p and e, respectively
      d <- dat[[ "d"]]
      a <- dat[[ "a"]]
    }
    
    # if all four columns are present, exit conditional
    if ( length( debt )==2 & length( assets )==2 & length(which( colnames( dat ) %in% debt ) )==2 & length(which( colnames( dat ) %in% assets ) )==2 ) {
      
      # create a column that concatenates two numerator variables into single column
      dat[ which( is.na( dat[ debt[2] ] )==F ), "d"] <- dat[ which( is.na( dat[ debt[2] ] )==F ), debt[2] ]
      dat[ which( is.na( dat[ debt[1] ] )==F ), "d"] <- dat[ which( is.na( dat[ debt[1] ] )==F ), debt[1] ]
      
      # create a column that concatenates two denominator variables into single column
      dat[ which( is.na( dat[ assets[2] ] )==F ), "a"] <- dat[ which( is.na( dat[ assets[2] ] )==F ), assets[2] ]
      dat[ which( is.na( dat[ assets[1] ] )==F ), "a"] <- dat[ which( is.na( dat[ assets[1] ] )==F ), assets[1] ]
      
      
      d <- dat[[ "d"]]
      a <- dat[[ "a"]]
    }
  }
  # END first outer conditional
  
  
  
  # all other cases are nested in the following conditionals
  
 if ( sum( debt %in% c( "F9_10_LIAB_TOT_EOY", "F9_01_NAFB_LIAB_TOT_EOY") )!=2 & sum( assets %in% c( "F9_10_ASSET_TOT_EOY", "F9_01_NAFB_ASSET_TOT_EOY") )!=2 ){          
    
    if ( length( debt )==2 & length( assets )==2 ) {
      
      # create a column that concatenates two numerator variables into single column
      dat[ which( is.na( dat[ debt[2] ] )==F ), "d"] <- dat[ which( is.na( dat[ debt[2] ] )==F ), debt[2] ]
      dat[ which( is.na( dat[ debt[1] ] )==F ), "d"] <- dat[ which( is.na( dat[ debt[1] ] )==F ), debt[1] ]
      
      # create a column that concatenates two denominator variables into single column
      dat[ which( is.na( dat[ assets[2] ] )==F ), "a"] <- dat[ which( is.na( dat[ assets[2] ] )==F ), assets[2] ]
      dat[ which( is.na( dat[ assets[1] ] )==F ), "a"] <- dat[ which( is.na( dat[ assets[1] ] )==F ), assets[1] ]
      
      
      d <- dat[[ "d"]]
      a <- dat[[ "a"]]
    }
    
     else if ( length( debt )==2 & length( assets )==1 ) {
      
      # create a column that concatenates two denominator variables into single column
      dat[ which( is.na( dat[ debt[2] ] )==F ), "d"] <- dat[ which( is.na( dat[ debt[2] ] )==F ), debt[2] ]
      dat[ which( is.na( dat[ debt[1] ] )==F ), "d"] <- dat[ which( is.na( dat[ debt[1] ] )==F ), debt[1] ]
      
      
      d <- dat[[ "d"]]
      a <- dat[[ assets ]]
    }
    
     else if ( length( debt )==1 & length( assets )==2 ) {
      
      # create a column that concatenates two numerator variables into single column
      dat[ which( is.na( dat[ assets[2] ] )==F ), "a"] <- dat[ which( is.na( dat[ assets[2] ] )==F ), assets[2] ]
      dat[ which( is.na( dat[ assets[1] ] )==F ), "a"] <- dat[ which( is.na( dat[ assets[1] ] )==F ), assets[1] ]
      
      
      d <- dat[[ debt ]]
      a <- dat[[ "a"]]
    }
    
     else if ( length( debt )==1 & length( assets )==1 ) {
      
      d <- dat[[ debt ]]
      a <- dat[[ assets ]]
    }
  
  
 }
  # can't divide by zero
  print( paste0( "Assets cannot be equal to zero: ", sum( a==0 , na.rm=T), " cases have been replaced with NA." ) )
  
  a[ a == 0 ] <- NA 

  dar <- d / a

  top.p    <- 1 - (1-winsorize)/2
  bottom.p <- 0 + (1-winsorize)/2
  top      <- quantile( dar, top.p, na.rm=T )
  bottom   <- quantile( dar, bottom.p, na.rm=T )
  dar.w    <- dar
  dar.w[ dar.w > top    ] <- top
  dar.w[ dar.w < bottom ] <- bottom

  dar.z <- scale( dar.w )

  dar.p <- dplyr::ntile( dar, 100 )

  DAR <- data.frame( dar, dar.w, dar.z, dar.p )

  print( summary( DAR ) )

  par( mfrow=c(2,2) )
  plot( density(dar,   na.rm=T), main="Debt to Asset Ratio (DAR)" )
  plot( density(dar.w, na.rm=T), main="DAR Winsorized" )
  plot( density(dar.z, na.rm=T), main="DAR Standardized as Z" )
  plot( density(dar.p, na.rm=T), main="DAR as Percentile" )

  df.dar <- data.frame( cbind( df, DAR ) )
  return( df.dar )
}



