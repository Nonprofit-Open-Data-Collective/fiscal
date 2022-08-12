###---------------------------------------------------
###   SELF SUFFICIENCY RATIO
###---------------------------------------------------

#' @title
#' Self Sufficiency Ratio 
#'
#' @description
#' Calculate the self sufficiency ratio and append it to the dataframe. 
#'
#' @param df A \code{data.frame} containing the required field for computing the metric. The metric will be appended to this dataset.
#' @param prog.serv.rev Column name(s) for program service revenue (must be quoted), EOY (Part 8, Line 2g(A); On EZ: Part 1, Line 2). If specifying column names for both PC and EZ scope variables, they must be specified as a vector of class character.
#' @param total.expense Column name(s) for total expenses (must be quoted), EOY (On 990: Part IX, line 25A; On EZ: Part 1, Line 17). If specifying column names for both PC and EZ scope variables, they must be specified as a vector of class character.
#' @param winsorize The winsorization value (between 0 and 1), defaults to 0.98 which winsorizes at 99th and 1st percentile values.   
#' 
#' @usage get_ssr( df, prog.serv.rev = c( "F9_08_REV_PROG_TOT_TOT", "F9_01_REV_PROG_TOT_CY"), 
#' total.expense = c( "F9_09_EXP_TOT_TOT", "F9_01_EXP_TOT_CY"), 
#' winsorize = 0.98 )
#' 
#' @return Object of class \code{data.frame}: the original dataframe appended with the self sufficiency ratio (`ssr`), 
#'  a winsorized version (`ssr.w`), a standardized z-score version (`ssr.z`), 
#'  and a percentile version (`ssr.p`).   
#'
#' @details The Self Sufficiency Ratio measures the proportion of operating expenses that are covered by earned income.
#' This metric is a good measure of how long an organization can survive without grants. Higher values 
#' mean it is more self-sufficient, meaning it could cover its costs longer without collecting any grants, 
#' rents, royalties, or sales of inventory. This ratio is primarily useful for organizations that have 
#' earned revenue through developers’ fees, management fees, memberships, or tuition. Higher values mean 
#' organizations have more autonomy and flexibility. They generally improve over time as an organization 
#' grows. In the early stages, these ratios tend to be lower but the goal is to make them as high as possible.
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
#' d <- get_ssr( df = dat, prog.serv.rev = "x1", total.expense = "x2" )
#' 
#' 
#' head( d )
#' 
#' # run with default column names
#' x3 <- rnorm( 1000, 100, 30 )
#' x4 <- rnorm( 1000, 200, 30 )
#' x3[ seq( from = 1, to = 1000, 50 ) ] <- NA
#' x4[ seq( from = 1, to = 1000, 71 ) ] <- NA
#' 
#' dat_01 <- data.frame( x1, x2, x3, x4 )
#' 
#' colnames( dat_01 ) <- c( "F9_08_REV_PROG_TOT_TOT", "F9_01_REV_PROG_TOT_CY",
#'                          "F9_09_EXP_TOT_TOT", "F9_01_EXP_TOT_CY" )
#' 
#' d <- get_ssr( dat_01 )
#' 
#' # specify only PC variables
#' d <- get_ssr( dat_01, prog.serv.rev = "F9_08_REV_PROG_TOT_TOT", total.expense = "F9_09_EXP_TOT_TOT" )
#' 
#' # coerce one column to factor
#' dat_01$F9_08_REV_PROG_TOT_TOT <- as.factor( dat_01$F9_08_REV_PROG_TOT_TOT )
#' 
#' d <- get_ssr( dat_01 )
#' 
#' # winsorize at 0.025 and 0.975 percentiles instead of 0.01 and 0.99
#' d <- get_ssr( df = dat, prog.serv.rev = "x1", total.expense = "x2", winsorize = 0.95 )
#' 
#' d <- get_ssr( dat_01, winsorize = 0.95 )
#' 
#' # assume only one PC variable for the numerator or denominator is present in the dataset and we run with default parameters
#' dat_02 <- dat_01
#' 
#' colnames( dat_02 ) <- c( "F9_08_REV_PROG_TOT_TOT", "F9_01_REV_PROG_TOT_CY",
#'                          "x", "F9_01_EXP_TOT_CY" )
#' 
#' 
#' d <- get_ssr( dat_02, winsorize = 0.95 )
#' 
#' colnames( dat_02 ) <- c( "F9_08_REV_PROG_TOT_TOT", "x",
#'                          "F9_09_EXP_TOT_TOT", "F9_01_EXP_TOT_CY" )
#' 
#' d <- get_ssr( dat_02, winsorize = 0.95 )
#' 
#' 
#' # using 990 data
#' data( part010810 )
#' d <- get_ssr( df = part010810 )
#' 
#' # now coerce one of the variables to numeric
#' part010810$F9_01_EXP_TOT_CY <- as.character( part010810$F9_01_EXP_TOT_CY )
#' 
#' d <- get_ssr( df = part010810 )
#' 
#' \dontrun{
#' ## Errors ##
#' 
#' # numerator not specified
#' d <- get_ssr( df = dat, prog.serv.rev = NULL, total.expense = "x2" )
#' 
#' # denominator not specified
#' d <- get_ssr( df = dat, prog.serv.rev = "x1", total.expense = NULL )
#' 
#' # neither numerator nor denominator specified
#' d <- get_ssr( df = dat, prog.serv.rev = NULL, total.expense = NULL )
#' 
#' # column names vector not of correct length
#' d <- get_ssr( df = dat, prog.serv.rev = c("e","b","c"), total.expense = "e")
#' 
#' # column names vector not of correct length
#' d <- get_ssr( df = dat, prog.serv.rev = "e", total.expense = c( "e", "b", "c") )
#' }
#' 
#' @export
get_ssr <- function( df, prog.serv.rev = c( "F9_08_REV_PROG_TOT_TOT", "F9_01_REV_PROG_TOT_CY"), 
                     total.expense = c( "F9_09_EXP_TOT_TOT", "F9_01_EXP_TOT_CY"), 
                     winsorize = 0.98 )
{

  # function checks
  if( winsorize > 1 | winsorize < 0 )
  { stop( "winsorize argument must be 0 < w < 1" ) }
  
  if( is.null( prog.serv.rev )==T & is.null( total.expense )==F )
  { stop( "The numerator has been incorrectly specified. Ensure you are passing the correct data field to the correct argument." ) }
  
  if( is.null( prog.serv.rev )==F & is.null( total.expense )==T )
  { stop( "The denominator has been incorrectly specified. Ensure you are passing the correct data field to the correct argument." ) }
  
  if( is.null( prog.serv.rev )==T & is.null( total.expense )==T )
  { stop( "The argument fields are empty. Please supply column names for each argument or execute the function with default inputs." ) }
  
  if( length( prog.serv.rev ) > 2 | length( prog.serv.rev ) < 1 )
  { stop( "`prog.serv.rev` must be a single quoted string or a vector with a minimum length of one and maximum length of two." ) }
  
  if( length( total.expense ) > 2 | length( total.expense ) < 1 )
  { stop( "`total.expense` must be a single quoted string or a vector with a minimum length of one and maximum length of two." ) }
  
  
  # copy data
  dat <- df
  
  ## ensure variable classes are numeric ##
  
  # run coerce_numeric and loop through all variables required and that matched by the two input arguments
  v <- c( colnames( dat )[which( colnames( dat ) %in% total.expense ) ], colnames( dat )[which( colnames( dat ) %in% prog.serv.rev )] )
  dat <- coerce_numeric( d = dat, vars = v )
  
  
  # check to ensure both sets of variable names are included in input data when not specifying column names.
  # edge cases
  
  # BEGIN first outer conditional
  # BEGIN first outer conditional
  if ( sum( prog.serv.rev %in% c( "F9_08_REV_PROG_TOT_TOT", "F9_01_REV_PROG_TOT_CY") )==2 & sum( total.expense %in% c( "F9_09_EXP_TOT_TOT", "F9_01_EXP_TOT_CY") )==2 ) {
    
    # BEGIN series of nested conditionals 
    
    
    # if at least one of the columns in missing from the input dataset, use only the column that is present
    if ( length( which( colnames( dat ) %in% prog.serv.rev ) )==1 | length( which( colnames( dat ) %in% total.expense ) )==1 ) {
      
      if ( length( which(colnames( dat ) %in% prog.serv.rev ) )==2 ){
        
        # create a column that concatenates two numerator variables into single column if both columns for numerator are present
        dat[ which( is.na( dat[ prog.serv.rev[2] ] )==F ), "p"] <- dat[ which( is.na( dat[ prog.serv.rev[2] ] )==F ), prog.serv.rev[2] ] 
        dat[ which( is.na( dat[ prog.serv.rev[1] ] )==F ), "p"] <- dat[ which( is.na( dat[ prog.serv.rev[1] ] )==F ), prog.serv.rev[1] ] 
      }
      # if at least one of the numerator columns in missing from the input dataset, use only the column that is present
      if ( length( which( colnames( dat ) %in% prog.serv.rev ) )==1 ){
        
        this <- which( colnames( dat ) %in% prog.serv.rev )
        
        dat[ , "p"] <- dat[ , this ]
      }
      
      
      if ( length( which( colnames( dat ) %in% total.expense ) )==2 ){
        # create a column that concatenates two numerator variables into single column if both columns for denominator are present
        dat[ which( is.na( dat[ total.expense[2] ] )==F ), "e"] <- dat[ which( is.na( dat[ total.expense[2] ] )==F ), total.expense[2] ] 
        dat[ which( is.na( dat[ total.expense[1] ] )==F ), "e"] <- dat[ which( is.na( dat[ total.expense[1] ] )==F ), total.expense[1] ] 
      }
      # if at least one of the denominator columns in missing from the input dataset, use only the column that is present
      if ( length( which( colnames( dat ) %in% total.expense ))==1  ){
        
        this <- which( colnames( dat ) %in% total.expense )
        
        dat[ , "e"] <- dat[ , this ]
      }
      # ENp series of nestep conditionals 
      
      # now, assign p anp e, respectively
      p <- dat[[ "p"]]
      e <- dat[[ "e"]]
    }
    
    # if all four columns are present, exit conditional
    if ( length( prog.serv.rev )==2 & length( total.expense )==2 & length(which( colnames( dat ) %in% prog.serv.rev ) )==2 & length(which( colnames( dat ) %in% total.expense ) )==2 ) {
      
      # create a column that concatenates two numerator variables into single column
      dat[ which( is.na( dat[ prog.serv.rev[2] ] )==F ), "p"] <- dat[ which( is.na( dat[ prog.serv.rev[2] ] )==F ), prog.serv.rev[2] ]
      dat[ which( is.na( dat[ prog.serv.rev[1] ] )==F ), "p"] <- dat[ which( is.na( dat[ prog.serv.rev[1] ] )==F ), prog.serv.rev[1] ]
      
      # create a column that concatenates two denominator variables into single column
      dat[ which( is.na( dat[ total.expense[2] ] )==F ), "e"] <- dat[ which( is.na( dat[ total.expense[2] ] )==F ), total.expense[2] ]
      dat[ which( is.na( dat[ total.expense[1] ] )==F ), "e"] <- dat[ which( is.na( dat[ total.expense[1] ] )==F ), total.expense[1] ]
      
      
      p <- dat[[ "p"]]
      e <- dat[[ "e"]]
    }
  }
  # END first outer conditional
  
  
  
  # all other cases are nestep in the following conditionals
  
  if ( sum( prog.serv.rev %in% c( "F9_08_REV_PROG_TOT_TOT", "F9_01_REV_PROG_TOT_CY") )!=2 & sum( total.expense %in% c( "F9_09_EXP_TOT_TOT", "F9_01_EXP_TOT_CY") )!=2 ){          
    
    if ( length( prog.serv.rev )==2 & length( total.expense )==2 ) {
      
      # create a column that concatenates two numerator variables into single column
      dat[ which( is.na( dat[ prog.serv.rev[2] ] )==F ), "p"] <- dat[ which( is.na( dat[ prog.serv.rev[2] ] )==F ), prog.serv.rev[2] ]
      dat[ which( is.na( dat[ prog.serv.rev[1] ] )==F ), "p"] <- dat[ which( is.na( dat[ prog.serv.rev[1] ] )==F ), prog.serv.rev[1] ]
      
      # create a column that concatenates two denominator variables into single column
      dat[ which( is.na( dat[ total.expense[2] ] )==F ), "e"] <- dat[ which( is.na( dat[ total.expense[2] ] )==F ), total.expense[2] ]
      dat[ which( is.na( dat[ total.expense[1] ] )==F ), "e"] <- dat[ which( is.na( dat[ total.expense[1] ] )==F ), total.expense[1] ]
      
      
      p <- dat[[ "p"]]
      e <- dat[[ "e"]]
    }
    
    else if ( length( prog.serv.rev )==2 & length( total.expense )==1 ) {
      
      # create a column that concatenates two denominator variables into single column
      dat[ which( is.na( dat[ prog.serv.rev[2] ] )==F ), "p"] <- dat[ which( is.na( dat[ prog.serv.rev[2] ] )==F ), prog.serv.rev[2] ]
      dat[ which( is.na( dat[ prog.serv.rev[1] ] )==F ), "p"] <- dat[ which( is.na( dat[ prog.serv.rev[1] ] )==F ), prog.serv.rev[1] ]
      
      
      p <- dat[[ "p"]]
      e <- dat[[ total.expense ]]
    }
    
    else if ( length( prog.serv.rev )==1 & length( total.expense )==2 ) {
      
      # create a column that concatenates two numerator variables into single column
      dat[ which( is.na( dat[ total.expense[2] ] )==F ), "e"] <- dat[ which( is.na( dat[ total.expense[2] ] )==F ), total.expense[2] ]
      dat[ which( is.na( dat[ total.expense[1] ] )==F ), "e"] <- dat[ which( is.na( dat[ total.expense[1] ] )==F ), total.expense[1] ]
      
      
      p <- dat[[ prog.serv.rev ]]
      e <- dat[[ "e"]]
    }
    
    else if ( length( prog.serv.rev )==1 & length( total.expense )==1 ) {
      
      p <- dat[[ prog.serv.rev ]]
      e <- dat[[ total.expense ]]
    }
    
    
  }  
  
  # can't divide by zero
  print( paste0( "Total expenses cannot be equal to zero: ", sum( e==0, na.rm = T ), " cases have been replaced with NA." ) )
  e[ e == 0 ] <- NA 
  
  ssr <- p / e
  
  top.p    <- 1 - (1-winsorize)/2
  bottom.p <- 0 + (1-winsorize)/2
  top      <- quantile( ssr, top.p, na.rm=T )
  bottom   <- quantile( ssr, bottom.p, na.rm=T )
  ssr.w    <- ssr
  ssr.w[ ssr.w > top    ] <- top
  ssr.w[ ssr.w < bottom ] <- bottom
  
  ssr.n <- scale( ssr.w )
  
  ssr.p <- dplyr::ntile( ssr, 100 )
  
  SSR <- data.frame( ssr, ssr.w, ssr.n, ssr.p )
  
  print( summary( SSR ) )
  
  par( mfrow=c(2,2) )
  plot( density(ssr,   na.rm=T), main="Self Sufficiency Ratio (SSR)" )
  plot( density(ssr.w, na.rm=T), main="SSR Winsorized" )
  plot( density(ssr.n, na.rm=T), main="SSR Standardized as Z" )
  plot( density(ssr.p, na.rm=T), main="SSR as Percentile" )
  
  df.ssr <- data.frame( cbind( df, SSR ) )
  return( df.ssr )
}



  




