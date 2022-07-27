###---------------------------------------------------
###   GOVERNMENT GRANTS RATIO
###---------------------------------------------------

#' @title
#' Government Grants Ratio
#'
#' @description
#' Calculate the government grants ratio and append it to the dataframe. 
#'
#' @param df A \code{data.frame} containing the required field for computing the metric. The metric will be appended to this dataset.
#' @param ggc A character string indicating the column name for government grant contributions (On 990: Part VIII, Line 1(E); On EZ: Not Available).
#' @param total.revenue A character string indicating the column name for total revenue (On 990: (Part VIII, Line 12A); On EZ: Part I, Line 9).
#' 
#' @return Object of class \code{data.frame}: the original dataframe appended with the government grants ratio (`ggr`), 
#'  a winsorized version (`ggr.w`), a standardized z-score version (`ggr.z`), 
#'  and a percentile version (`ggr.p`).   
#'
#' @details The government grants ratio measures the percentage of an organization’s revenues that come from 
#' government grants and contributions. High values in this ratio indicate that more of an organization's 
#' revenues come from government grants while a lower number indicates that an organization's funds come from 
#' other donation sources (private foundations), earned income, or investment income. Note: computation of this 
#' metric is available to only 990 filers and not for 990-EZ filers. The default inputs use column names for 
#' variables available only to 990 filers.
#' 
#' @examples
#' x1 <- rnorm( 1000, 100, 30 )
#' x2 <- rnorm( 1000, 200, 30 )
#' x2[ c( 15, 300, 600 ) ] <- 0
#' 
#' dat <- data.frame( x1, x2 )
#' 
#' # specify own column names
#' d <- get_ggr( df = dat, ggc = "x1", total.revenue = "x2" )
#' 
#' head( d )
#' 
#' # run with default column names
#' x1[ seq( from = 1, to = 1000, 50 ) ] <- NA
#' x2[ seq( from = 1, to = 1000, 71 ) ] <- NA
#' 
#' dat_01 <- data.frame( x1, x2 )
#' 
#' colnames( dat_01 ) <- c( "F9_08_REV_CONTR_GOVT_GRANT", "F9_08_REV_TOT_TOT" )
#' 
#' d <- get_ggr( dat_01 )
#' 
#' # coerce one column to factor
#' dat_01$F9_08_REV_CONTR_GOVT_GRANT <- as.factor( dat_01$F9_08_REV_CONTR_GOVT_GRANT )
#' 
#' d <- get_ggr( dat_01 )
#' 
#' # winsorize at 0.025 and 0.975 percentiles instead of 0.01 and 0.99
#' d <- get_ggr( df = dat, ggc = "x1", total.revenue = "x2", winsorize = 0.95 )
#' 
#' d <- get_ggr( dat_01, winsorize = 0.95 )
#' 
#' 
#' # using 990 data
#' load( '/Volumes/My Passport for Mac/Urban Institute/Summer Projects/Fiscal/fiscal/R/sysdata.rda' )
#' d <- get_ggr( df = part010810 )
#' 
#' # now coerce one of the variables to numeric
#' part010810$F9_08_REV_CONTR_GOVT_GRANT <- as.character( part010810$F9_08_REV_CONTR_GOVT_GRANT )
#' 
#' d <- get_ggr( df = part010810 )
#' 
#' 
#' ## Errors ##
#' 
#' # numerator not specified
#' d <- get_ggr( df = dat, ggc = NULL, total.revenue = "x2" )
#' 
#' # denominator not specified
#' d <- get_ggr( df = dat, ggc = "x1", total.revenue = NULL )
#' 
#' # neither numerator nor denominator specified
#' d <- get_ggr( df = dat, ggc = NULL, total.revenue = NULL )
#' 
#' # column names vector not of correct length
#' d <- get_ggr( df = dat, ggc = c("e","b","c"), total.revenue = "e")
#' 
#' # column names vector not of correct length
#' d <- get_ggr( df = dat, ggc = "e", total.revenue = c( "e", "b", "c") )
#' @export
get_ggr <- function( df, 
                     ggc = "F9_08_REV_CONTR_GOVT_GRANT", 
                     total.revenue = "F9_08_REV_TOT_TOT", 
                     winsorize=0.98 )
{
  
  # function checks
  if( winsorize > 1 | winsorize < 0 )
  { stop( "winsorize argument must be 0 < w < 1" ) }
  
  if( is.null( ggc )==T & is.null( total.revenue )==F )
  { stop( "The numerator has been incorrectly specified. Ensure you are passing the correct data field to the correct argument." ) }
  
  if( is.null( ggc )==F & is.null( total.revenue )==T )
  { stop( "The denominator has been incorrectly specified. Ensure you are passing the correct data field to the correct argument." ) }
  
  if( is.null( ggc )==T & is.null( total.revenue )==T )
  { stop( "The argument fields are empty. Please supply column names for each argument or execute the function with default inputs." ) }
  
  if( length( ggc ) > 2 | length( ggc ) < 1 )
  { stop( "`ggc` must be a single quoted string or a vector with a minimum length of one and maximum length of two." ) }
  
  if( length( total.revenue ) > 2 | length( total.revenue ) < 1 )
  { stop( "`total.revenue` must be a single quoted string or a vector with a minimum length of one and maximum length of two." ) }
  
  
  # copy data
  dat <- df
  
  ## ensure variable classes are numeric ##
  
  # run coerce_numeric and loop through all variables required and that matched by the two input arguments
  v <- c( colnames( dat )[which( colnames( dat ) %in% total.revenue ) ], colnames( dat )[which( colnames( dat ) %in% ggc )] )
  dat <- coerce_numeric( d = dat, vars = v )
  
  
  # check to ensure both sets of variable names are included in input data when not specifying column names.
  # edge cases
  
  # BEGIN first outer conditional
  # BEGIN first outer conditional
  if ( sum( ggc %in% c( "F9_08_REV_CONTR_GOVT_GRANT") )==2 & sum( total.revenue %in% c( "F9_08_REV_TOT_TOT") )==2 ) {
    
    # BEGIN series of nested conditionals 
    
    
    # if at least one of the columns in missing from the input dataset, use only the column that is present
    if ( length( which( colnames( dat ) %in% ggc ) )==1 | length( which( colnames( dat ) %in% total.revenue ) )==1 ) {
      
      if ( length( which(colnames( dat ) %in% ggc ) )==2 ){
        
        # create a column that concatenates two numerator variables into single column if both columns for numerator are present
        dat[ which( is.na( dat[ ggc[2] ] )==F ), "d" ] <- dat[ which( is.na( dat[ ggc[2] ] )==F ), ggc[2] ] 
        dat[ which( is.na( dat[ ggc[1] ] )==F ), "d" ] <- dat[ which( is.na( dat[ ggc[1] ] )==F ), ggc[1] ] 
      }
      # if at least one of the numerator columns in missing from the input dataset, use only the column that is present
      if ( length( which( colnames( dat ) %in% ggc ) )==1 ){
        
        this <- which( colnames( dat ) %in% ggc )
        
        dat[ , "d" ] <- dat[ , this ]
      }
      
      
      if ( length( which( colnames( dat ) %in% total.revenue ) )==2 ){
        # create a column that concatenates two numerator variables into single column if both columns for denominator are present
        dat[ which( is.na( dat[ total.revenue[2] ] )==F ), "e"] <- dat[ which( is.na( dat[ total.revenue[2] ] )==F ), total.revenue[2] ] 
        dat[ which( is.na( dat[ total.revenue[1] ] )==F ), "e"] <- dat[ which( is.na( dat[ total.revenue[1] ] )==F ), total.revenue[1] ] 
      }
      # if at least one of the denominator columns in missing from the input dataset, use only the column that is present
      if ( length( which( colnames( dat ) %in% total.revenue ))==1  ){
        
        this <- which( colnames( dat ) %in% total.revenue )
        
        dat[ , "e" ] <- dat[ , this ]
      }
      # ENp series of nestep conditionals 
      
      # now, assign p anp e, respectively
      d <- dat[[ "d" ]]
      e <- dat[[ "e" ]]
    }
    
    # if all four columns are present, exit conditional
    if ( length( ggc )==2 & length( total.revenue )==2 & length(which( colnames( dat ) %in% ggc ) )==2 & length(which( colnames( dat ) %in% total.revenue ) )==2 ) {
      
      # create a column that concatenates two numerator variables into single column
      dat[ which( is.na( dat[ ggc[2] ] )==F ), "d" ] <- dat[ which( is.na( dat[ ggc[2] ] )==F ), ggc[2] ]
      dat[ which( is.na( dat[ ggc[1] ] )==F ), "d" ] <- dat[ which( is.na( dat[ ggc[1] ] )==F ), ggc[1] ]
      
      # create a column that concatenates two denominator variables into single column
      dat[ which( is.na( dat[ total.revenue[2] ] )==F ), "e"] <- dat[ which( is.na( dat[ total.revenue[2] ] )==F ), total.revenue[2] ]
      dat[ which( is.na( dat[ total.revenue[1] ] )==F ), "e"] <- dat[ which( is.na( dat[ total.revenue[1] ] )==F ), total.revenue[1] ]
      
      
      d <- dat[[ "d"]]
      e <- dat[[ "e"]]
    }
  }
  # END first outer conditional
  
  
  
  # all other cases are nestep in the following conditionals
  
  if ( sum( ggc %in% c( "F9_08_REV_CONTR_GOVT_GRANT") )!=2 & sum( total.revenue %in% c( "F9_08_REV_TOT_TOT") )!=2 ){          
    
    if ( length( ggc )==2 & length( total.revenue )==2 ) {
      
      # create a column that concatenates two numerator variables into single column
      dat[ which( is.na( dat[ ggc[2] ] )==F ), "d"] <- dat[ which( is.na( dat[ ggc[2] ] )==F ), ggc[2] ]
      dat[ which( is.na( dat[ ggc[1] ] )==F ), "d"] <- dat[ which( is.na( dat[ ggc[1] ] )==F ), ggc[1] ]
      
      # create a column that concatenates two denominator variables into single column
      dat[ which( is.na( dat[ total.revenue[2] ] )==F ), "e"] <- dat[ which( is.na( dat[ total.revenue[2] ] )==F ), total.revenue[2] ]
      dat[ which( is.na( dat[ total.revenue[1] ] )==F ), "e"] <- dat[ which( is.na( dat[ total.revenue[1] ] )==F ), total.revenue[1] ]
      
      
      d <- dat[[ "d"]]
      e <- dat[[ "e"]]
    }
    
    else if ( length( ggc )==2 & length( total.revenue )==1 ) {
      
      # create a column that concatenates two denominator variables into single column
      dat[ which( is.na( dat[ ggc[2] ] )==F ), "d"] <- dat[ which( is.na( dat[ ggc[2] ] )==F ), ggc[2] ]
      dat[ which( is.na( dat[ ggc[1] ] )==F ), "d"] <- dat[ which( is.na( dat[ ggc[1] ] )==F ), ggc[1] ]
      
      
      d <- dat[[ "d"]]
      e <- dat[[ total.revenue ]]
    }
    
    else if ( length( ggc )==1 & length( total.revenue )==2 ) {
      
      # create a column that concatenates two numerator variables into single column
      dat[ which( is.na( dat[ total.revenue[2] ] )==F ), "e"] <- dat[ which( is.na( dat[ total.revenue[2] ] )==F ), total.revenue[2] ]
      dat[ which( is.na( dat[ total.revenue[1] ] )==F ), "e"] <- dat[ which( is.na( dat[ total.revenue[1] ] )==F ), total.revenue[1] ]
      
      
      d <- dat[[ ggc ]]
      e <- dat[[ "e"]]
    }
    
    else if ( length( ggc )==1 & length( total.revenue )==1 ) {
      
      d <- dat[[ ggc ]]
      e <- dat[[ total.revenue ]]
    }
    
    
  }    
  
  # can't divide by zero
  print( paste0( "Total revenue cannot be equal to zero: ", sum( e==0, na.rm = T ), " cases have been replaced with NA." ) )
  e[ e == 0 ] <- NA 
  
  ggr <- d / e
  
  top.p    <- 1 - (1-winsorize)/2
  bottom.p <- 0 + (1-winsorize)/2
  top      <- quantile( ggr, top.p, na.rm=T )
  bottom   <- quantile( ggr, bottom.p, na.rm=T )
  ggr.w    <- ggr
  ggr.w[ ggr.w > top    ] <- top
  ggr.w[ ggr.w < bottom ] <- bottom
  
  ggr.n <- scale( ggr.w )
  
  ggr.p <- dplyr::ntile( ggr, 100 )
  
  GGR <- data.frame( ggr, ggr.w, ggr.n, ggr.p )
  
  print( summary( GGR ) )
  
  par( mfrow=c(2,2) )
  plot( density(ggr,   na.rm=T), main="Government Grants Ratio (GGR)" )
  plot( density(ggr.w, na.rm=T), main="GGR Winsorized" )
  plot( density(ggr.n, na.rm=T), main="GGR Standardized as Z" )
  plot( density(ggr.p, na.rm=T), main="GGR as Percentile" )
  
  df.ggr <- data.frame( cbind( df, GGR ) )
  return( df.ggr )
  
}
