###---------------------------------------------------
###   INVESTMENT INCOME DEPENDENCE RATIO
###---------------------------------------------------

#' @title
#' Investment Income Dependence Ratio
#'
#' @description
#' Calculate the investment income dependence ratio and append it to the dataframe. 
#'
#' @param df A \code{data.frame} containing the required field for computing the metric. The metric will be appended to this dataset.
#' @param invest.income A character string indicating the column name for investment income (On 990: Part VIII, Line 3(A); On EZ: Not Available).
#' @param bond.proceeds A character string indicating the column name for investment income from tax-exempt bond proceeds (On 990: Part VIII, Line 4(A); On EZ: Not Available).
#' @param rent.income A character string indicating the column name for gross rent income (On 990: Part VIII, Line 6(A); On EZ: Not Available).
#' @param other.income A character string indicating the column name for gross income from sales of assets other than inventory  (On 990: Part VIII, Line 7(A); On EZ: Not Available). 
#' @param total.revenue A character string indicating the column name for total revenue (On 990: (Part VIII, Line 12A); On EZ: Part I, Line 9).
#' 
#' @usage get_iidr( df, 
#' invest.income = "F9_08_REV_OTH_INVEST_INCOME_TOT", 
#' bond.proceeds = "F9_08_REV_OTH_INVEST_BOND_TOT", 
#' rent.income = "F9_08_REV_OTH_RENT_GRO_PERS", 
#' other.income = "F9_08_REV_OTH_SALE_ASSET_OTH", 
#' total.revenue = "F9_08_REV_TOT_TOT", 
#' numerator = NULL,
#' denominator = NULL,
#' winsorize = 0.98 )
#' 
#' @return Object of class \code{data.frame}: the original dataframe appended with the investment income dependence ratio (`iidr`), 
#'  a winsorized version (`iidr.w`), a standardized z-score version (`iidr.z`), 
#'  and a percentile version (`iidr.p`).   
#'
#' @details This metric shows how much of an organization’s total revenues come from investment income 
#' (interest, dividends, gains/losses on sales of securities or other assets). High levels in this metric 
#' indicate an organization is more dependent on investment income (and thus vulnerable to market downturns), 
#' and low values indicate their income comes more from donations or earned income. Note: computation of this metric is 
#' available to only 990 filers and not for 990-EZ filers. The default inputs use column names for variables available 
#' only to 990 filers.
#' 
#' @examples
#' x1 <- rnorm( 1000,100,30 )
#' x2 <- rnorm( 1000,200,30 )
#' x3 <- rnorm( 1000,200,30 )
#' x4 <- rnorm( 1000,100,30 )
#' x5 <- rnorm( 1000,200,30 )
#' dat <- data.frame( x1, x2, x3, x4, x5 )
#' 
#' 
#' # specify own column names
#' d <- get_iidr( df=dat, invest.income='x1', bond.proceeds='x2', rent.income='x3', other.income='x4', total.revenue='x5', winsorize=0.98 )
#' 
#' head( d )
#' 
#' # run with default column names
#' dat_01 <- dat
#' 
#' colnames( dat_01 ) <- c( "F9_08_REV_OTH_INVEST_INCOME_TOT", "F9_08_REV_OTH_INVEST_BOND_TOT", "F9_08_REV_OTH_RENT_GRO_PERS",
#'                          "F9_08_REV_OTH_SALE_ASSET_OTH", "F9_08_REV_TOT_TOT" )
#' 
#' 
#' d <- get_iidr( dat_01 )
#' 
#' # coerce one column to factor
#' dat_01$F9_08_REV_OTH_INVEST_BOND_TOT <- as.factor( dat_01$F9_08_REV_OTH_INVEST_BOND_TOT )
#' 
#' d <- get_iidr( dat_01 )
#' 
#' # winsorize at 0.025 and 0.975 percentiles instead of 0.01 and 0.99
#' d <- get_iidr( df=dat, invest.income='x1', bond.proceeds='x2', rent.income='x3', other.income='x4', total.revenue='x5', winsorize = 0.95 )
#' 
#' d <- get_iidr( dat_01, winsorize = 0.95 )
#' 
#' # aggregate variables into single numerator and denominator
#' x.den <- x5 
#' x.num <- x1 + x2 + x3 + x4
#' 
#' dat_02 <- cbind( dat, x.den, x.num)
#' 
#' d <- get_iidr( dat_02, numerator = "x.num", denominator = "x.den" )
#' 
#' # using 990 data
#' load( '/Volumes/My Passport for Mac/Urban Institute/Summer Projects/Fiscal/fiscal/R/sysdata.rda' )
#' d <- get_iidr( df = part010810 )
#' 
#' # now coerce one of the variables to numeric
#' part010810$F9_08_REV_OTH_INVEST_BOND_TOT <- as.character( part010810$F9_08_REV_OTH_INVEST_BOND_TOT )
#' 
#' d <- get_iidr( df = part010810 )
#' 
#' # incorrectly specify denominator
#' get_iidr( df = dat_02, invest.income = 'x1', bond.proceeds = 'x2', rent.income = 'x3', other.income = 'x4', 
#'           total.revenue = NULL, numerator = NULL, denominator = NULL, winsorize=0.98 )
#' 
#' # incorrectly specify numerator with conflicting arguments
#' get_iidr( df = dat_02, invest.income = 'x1', bond.proceeds = 'x2', rent.income = 'x3', other.income = 'x4', 
#'           total.revenue = NULL, numerator = 'x.num', denominator = 'x.den' , winsorize=0.98 )
#' 
#' 
#' # incorrectly specify denominator with conflicting arguments
#' get_iidr( df = dat_02, invest.income = 'x1', bond.proceeds = 'x2', rent.income = 'x3', other.income = 'x4', 
#'           total.revenue = 'x5', numerator = NULL, denominator = 'x.den' , winsorize=0.98 )  
#' 
#' # supplying no arguments for the numerator
#' get_iidr( df = dat_02, invest.income = NULL, bond.proceeds = NULL, rent.income = NULL, other.income = NULL, 
#'           total.revenue = 'x5', numerator = NULL, denominator = NULL , winsorize=0.98 )  
#' 
#' # supplying no arguments for the denominator
#' get_iidr( df = dat_02, invest.income = 'x1', bond.proceeds = 'x2', rent.income = 'x3', other.income = 'x4', 
#'           total.revenue = NULL, numerator = NULL, denominator = NULL , winsorize=0.98 )  
#' 
#' # supplying argument for one of the parameters in the numerator that is greater than length 1
#' get_iidr( df = dat_02, invest.income = 'x1', bond.proceeds = 'x2', rent.income = 'x3', other.income = 'x4', 
#'           total.revenue = c( 'x5', 'x6' ), numerator = NULL, denominator = NULL, winsorize=0.98 )
#' 
#' get_iidr( df = dat_02, invest.income = NULL, bond.proceeds = NULL, rent.income = NULL, other.income = NULL, 
#'           total.revenue = NULL, numerator = c( 'x5', 'x6' ), denominator = 'x.den' , winsorize=0.98 )  
#' 
#' @export
get_iidr <- function( df, 
                      invest.income = "F9_08_REV_OTH_INVEST_INCOME_TOT", 
                      bond.proceeds = "F9_08_REV_OTH_INVEST_BOND_TOT", 
                      rent.income = "F9_08_REV_OTH_RENT_GRO_PERS", 
                      other.income = "F9_08_REV_OTH_SALE_ASSET_OTH", 
                      total.revenue = "F9_08_REV_TOT_TOT", 
                      numerator = NULL,
                      denominator = NULL,
                      winsorize = 0.98 )
{
  
  # checks
  if( winsorize > 1 | winsorize < 0 )
  { stop( "winsorize argument must be 0 < w < 1" ) }
  
  if( is.null( invest.income )==F & is.null( bond.proceeds )==F & is.null( rent.income )==F & is.null( other.income )==F &
      is.null( total.revenue )==F ) {
    
    if( invest.income == "F9_08_REV_OTH_INVEST_INCOME_TOT" & bond.proceeds == "F9_08_REV_OTH_INVEST_BOND_TOT" &
        rent.income == "F9_08_REV_OTH_RENT_GRO_PERS" & other.income == "F9_08_REV_OTH_SALE_ASSET_OTH" & total.revenue == "F9_08_REV_TOT_TOT" &
        is.null( numerator )==F & is.null( denominator )==F ){
      warning( "Default argument inputs overridden with specified numerator and denominator arguments" ) 
      
      invest.income <- NULL
      bond.proceeds <- NULL
      rent.income <- NULL
      other.income <- NULL
      total.revenue <- NULL
      
    }
    
  } 
  
  if ( ( ( length( c( invest.income, bond.proceeds, rent.income, other.income ) ) < 4 )==F | is.null( numerator )==F ) &
       ( ( is.null( total.revenue )==T ) &
         is.null( denominator )==T ) )
  { stop( "The denominator has been incorrectly specified. Ensure you are passing the correct data field to the correct argument." ) }
  
  if ( ( ( length( c( total.revenue ) ) < 1 )==F | is.null( denominator )==F ) &
       ( ( is.null( invest.income )==T | is.null( bond.proceeds )==T | 
           is.null( rent.income )==T | is.null( other.income )==T ) &
         is.null( numerator )==T ) )
  { stop( "The numerator has been incorrectly specified. Ensure you are passing the correct data field to the correct argument." ) }
  
  
  if ( ( length( c( invest.income, bond.proceeds, rent.income, other.income ) ) <= 4 ) &
       ( length( c( invest.income, bond.proceeds, rent.income, other.income ) ) >= 1 ) & 
       ( is.null( numerator )==F ) )
  { stop( "The numerator has been incorrectly specified with conflicting arguments. Ensure you are passing the correct data field to the correct argument." ) }
  
  if ( ( length( c( total.revenue ) ) >= 1 ) & 
       ( is.null( denominator )==F ) )
  { stop( "The denominator has been incorrectly specified with conflicting arguments. Ensure you are passing the correct data field to the correct argument." ) }
  
  if( ( length( invest.income ) > 1 | length( invest.income ) < 1 ) & is.null( numerator ) == T ) 
  { stop( "`invest.income` must be a single quoted string or a vector with a maximum length of one." ) }
  
  if( ( length( bond.proceeds ) > 1 | length( bond.proceeds ) < 1 ) & is.null( numerator ) == T ) 
  { stop( "`bond.proceeds` must be a single quoted string or a vector with a maximum length of one." ) }
  
  if( ( length( rent.income ) > 1 | length( rent.income ) < 1 ) & is.null( numerator ) == T ) 
  { stop( "`rent.income` must be a single quoted string or a vector with a maximum length of one." ) }
  
  if( ( length( other.income ) > 1 | length( other.income ) < 1 ) & is.null( numerator ) == T ) 
  { stop( "`other.income` must be a single quoted string or a vector with a maximum length of one." ) }
  
  if( ( length( total.revenue ) > 1 | length( total.revenue ) < 1 ) & is.null( denominator ) == T ) 
  { stop( "`total.revenue` must be a single quoted string or a vector with a maximum length of one." ) }
  
  if( ( length( numerator ) > 1 & length( c( invest.income, bond.proceeds, rent.income, other.income ) ) == 0 ) )
  { stop( "`numerator` must be a single quoted string or a vector with a maximum length of one." ) }
  
  if( ( length( denominator ) > 1 & length( c( total.revenue ) ) == 0 ) )
  { stop( "`denominator` must be a single quoted string or a vector with a maximum length of one." ) }
  
  
  
  # copy data
  dat <- df
  
  ## ensure variable classes are numeric ##
  
  # run coerce_numeric and loop through all variables required and that matched by the two input arguments
  v <- c( colnames( dat )[which( colnames( dat ) %in% invest.income ) ], colnames( dat )[which( colnames( dat ) %in% bond.proceeds )],
          colnames( dat )[which( colnames( dat ) %in% other.income )], colnames( dat )[which( colnames( dat ) %in% rent.income )], 
          colnames( dat )[which( colnames( dat ) %in% total.revenue )], colnames( dat )[which( colnames( dat ) %in% numerator )],
          colnames( dat )[which( colnames( dat ) %in% denominator )] )
  
  dat <- coerce_numeric( d = dat, vars = v )
  
  if( is.null( numerator) == T & is.null( denominator ) == T ){
    
    num <- dat[[ invest.income ]] + dat[[ bond.proceeds ]] + dat[[ rent.income ]] + dat[[ other.income ]]
    den <- dat[[ total.revenue ]] 
    
  }
  
  if( is.null( numerator) == F & is.null( denominator ) == T ){
    
    num <- dat[[ numerator ]]
    den <- dat[[ total.revenue ]] 
    
  }
  
  if( is.null( numerator) == T & is.null( denominator ) == F ){
    
    num <- dat[[ invest.income ]] + dat[[ bond.proceeds ]] + dat[[ rent.income ]] + dat[[ other.income ]]
    den <- dat[[ denominator ]] 
    
  }
  
  if( is.null( numerator) == F & is.null( denominator ) == F ){
    
    num <- dat[[ numerator ]]
    den <- dat[[ denominator ]] 
    
  }
  
  # can't divide by zero
  print( paste0( "Total revenue cannot be zero: ", sum( den==0, na.rm = T ), " cases have been replaced with NA." ) )
  den[ den == 0 ] <- NA 
  
  iidr <- num / den
  
  top.p    <- 1 - (1-winsorize)/2
  bottom.p <- 0 + (1-winsorize)/2
  top      <- quantile( iidr, top.p, na.rm=T )
  bottom   <- quantile( iidr, bottom.p, na.rm=T )
  iidr.w    <- iidr
  iidr.w[ iidr.w > top    ] <- top
  iidr.w[ iidr.w < bottom ] <- bottom
  
  iidr.n <- scale( iidr.w )
  
  iidr.p <- dplyr::ntile( iidr, 100 )
  
  IIDR <- data.frame( iidr, iidr.w, iidr.n, iidr.p )
  
  print( summary( IIDR ) )
  
  par( mfrow=c(2,2) )
  plot( density( iidr,   na.rm=T ), main="Investment Income Dependence Ratio (IIDR)" )
  plot( density( iidr.w, na.rm=T ), main="IIDR Winsorized" )
  plot( density( iidr.n, na.rm=T ), main="IIDR Standardized as Z" )
  plot( density( iidr.p, na.rm=T ), main="IIDR as Percentile" )
  
  df.iidr <- data.frame( cbind( df, IIDR ) )
  return( df.iidr )
}
