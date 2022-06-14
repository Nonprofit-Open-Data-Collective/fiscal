###---------------------------------------------------
###   ADD PIPE OPERATOR TO THE PACKAGE
###---------------------------------------------------
#' @importFrom magrittr "%>%"



###---------------------------------------------------
###   DAYS OF OPERATING CASH ON HAND
###---------------------------------------------------

#' @title
#' Days of Operating Cash on Hand 
#'
#' @description
#' Calculate the days of operating cash on hand and append it to the dataframe. 
#'
#' @param f.cash Cash (On 990: (Part X, line 1B)); On EZ:Part I, line 22 (cash and short-term investments only)).
#' @param f.si Short-term investments (On 990: (Part X, line 2B).
#' @param f.pr Pledges and grant receivables (On 990: (Part X, line 3B).
#' @param f.ar Accounts receivables (On 990: (Part X, line 4B).
#' @param f.tfe Total functional expenses (On 990: Part IX, line 25A).
#' @param f.dda Depreciation, depletion, and amortization (On 990: Part IX, line 22A).
#' @param ez.csi Cash, savings, and investment (On EZ:Part I, line 22 (cash and short-term investments only)).
#' @param ez.toe Total operating expenses (On EZ: Part I, line 17 (operating expenses only)).
#' @param winsorize The winsorization value (between 0 and 1), defaults to 0.98 which winsorizes at 99th and 1st percentile values.   
#' 
#' @return The original dataframe appended with the debt to asset ratio (`doch`), 
#'  a winsorized version (`doch.w`), a standardized z-score version (`doch.z`), 
#'  and a percentile version (`doch.p`).   
#'
#' @details Days cash on hand is the number of days that an organization can continue to pay 
#' its operating expenses, given the amount of cash available. This metric is good to review at startup 
#' of organization, during periods of low revenues, and prior to undertaking new major activity. The metric 
#' is limited though in the following ways. First, it is based on an average daily cash outflow, which is 
#' not really the case. Instead, cash tends to be spent in a lumpy manner, such as when rent or payroll 
#' are paid. Also, management tends to take drastic action to reduce expenses as cash reserves decline, 
#' so that the actual days of operation tend to be longer than indicated by this ratio. Thus, it is better 
#' to use a detailed cash flow analysis to determine the precise duration of the available cash, with regular 
#' updates.
#' 
#' @examples
#' x1 <- rnorm(1000,100,30)
#' x2 <- rnorm(1000,200,30)
#' x3 <- rnorm(1000,200,30)
#' x4 <- rnorm(1000,200,30)
#' x5 <- rnorm(1000,200,30)
#' x6 <- rnorm(1000,200,30)
#' dat<-data.frame(x1,x2,x3,x4,x5,x6)
#' a <- get_doch(df=dat, f.cash='x1', f.si = 'x2',
#'              f.pr='x3', f.ar='x4', f.tfe='x5',f.dda='x6')
#' 
# incorrectly specified arguments
#' b <- get_doch(df=dat, f.cash='x1', f.si = 'x2',
#'               f.pr='x3', f.ar='x4', ez.toe='x5',f.dda='x6')
#' 
# zero in the denominator
#' x5[c(1:10)]<-0
#' x6[c(1:10)]<-0
#' dat<-data.frame(x1,x2,x3,x4,x5,x6)
#' 
#' c <- get_doch(df=dat, f.cash='x1', f.si = 'x2',
#'               f.pr='x3', f.ar='x4', f.tfe='x5',f.dda='x6')
#' 
#' # winsorize at 0.025 and 0.975 percentiles instead of 0.01 and 0.99
#' d <- get_doch(df=dat, f.cash='x1', f.si = 'x2',
#'               f.pr='x3', f.ar='x4', f.tfe='x5',f.dda='x6', winsorize=0.95 )
#'
#' # winsorize at 0.025 and 0.975 percentiles instead of 0.01 and 0.99
#' d <- get_doch( df=dat, debt="x1", assets="x2", winsorize=0.95 )
#' 
#' @export
get_doch <- function( df, f.cash=NULL, f.si=NULL, f.pr=NULL, f.ar=NULL, f.tfe=NULL, f.dda=NULL, ez.csi=NULL, ez.toe=NULL,winsorize=0.98 )
{
  
  # checks
  if( winsorize > 1 | winsorize < 0 )
  { stop( "winsorize argument must be 0 < w < 1" ) }
  
  # check that data is coming from either F990 or the F990EZ, but not both
  if( (is.null(f.cash)==F | is.null(f.si)==F | is.null(f.pr)==F | is.null(f.ar)==F |
       is.null(f.tfe)==F | is.null(f.dda)==F) & (is.null(ez.csi)==F | is.null(ez.toe)==F) )
    { stop( "Data fields must come from one of: i. F990 or ii. F990EZ, but not both. Ensure you have accurately passed the data field to the correct arguments." ) }

  if ( (length( c(f.cash, f.si, f.pr, f.ar, f.tfe, f.dda) ) < 6)==T & (is.null(ez.csi)==T | is.null(ez.toe)==T) )
  { stop( "Missing at least one data field from the F990 data. Ensure you are passing the correct data field to the correct argument." ) }

  if (length( c(ez.toe, ez.csi) ) < 2 & (is.null(f.cash)==T | is.null(f.si)==T | is.null(f.pr)==T | is.null(f.ar)==T |
                                         is.null(f.tfe)==T | is.null(f.dda)==T) )
  { stop( "Missing at least one data field from the F990EZ data. Ensure you are passing the correct data field to the correct argument." ) }
  
  # pass with F990 form
  if( (is.null(f.cash)==F & is.null(f.si)==F & is.null(f.pr)==F & is.null(f.ar)==F &
      is.null(f.tfe)==F & is.null(f.dda)==F) )
  {
    n <- df[[ f.cash ]] + df[[ f.si ]] + df[[ f.pr ]] + df[[f.ar]]
    d <- ( df[[ f.tfe ]] + df[[ f.dda ]] ) / 365    
  }
  
  # pass with 990-EZ
  else if( (is.null(ez.csi)==F | is.null(ez.toe)==F) )
  {
    n <- df[[ ez.csi ]]
    d <- ( df[[ ez.toe ]] ) / 365    
  }
  
  
  # can't divide by zero
  print( paste0( "Assets cannot be zero: ", sum(d==0), " cases have been replaced with NA." ) )
  d[ d == 0 ] <- NA 

  doch <- n / d

  top.p    <- 1 - (1-winsorize)/2
  bottom.p <- 0 + (1-winsorize)/2
  top      <- quantile( doch, top.p, na.rm=T )
  bottom   <- quantile( doch, bottom.p, na.rm=T )
  doch.w    <- doch
  doch.w[ doch.w > top    ] <- top
  doch.w[ doch.w < bottom ] <- bottom

  doch.n <- scale( doch.w )

  doch.p <- dplyr::ntile( doch, 100 )

  DOCH <- data.frame( doch, doch.w, doch.n, doch.p )

  print( summary( DOCH ) )

  par( mfrow=c(2,2) )
  plot( density(doch,   na.rm=T), main="Days of Operating Cash on Hand (DOCH)" )
  plot( density(doch.w, na.rm=T), main="DOCH Winsorized" )
  plot( density(doch.n, na.rm=T), main="DOCH Standardized as Z" )
  plot( density(doch.p, na.rm=T), main="DOCH as Percentile" )

  df.doch <- cbind( df, DOCH )
  return( df.doch )
}




 # x1 <- rnorm(1000,100,30)
 # x2 <- rnorm(1000,200,30)
 # x3 <- rnorm(1000,200,30)
 # x4 <- rnorm(1000,200,30)
 # x5 <- rnorm(1000,200,30)
 # x6 <- rnorm(1000,200,30)
 # dat<-data.frame(x1,x2,x3,x4,x5,x6)
 # a <- get_doch(df=dat, f.cash='x1', f.si = 'x2',
 #          f.pr='x3', f.ar='x4', f.tfe='x5',f.dda='x6')
 # 
 # # incorrectly specified arguments
 # b <- get_doch(df=dat, f.cash='x1', f.si = 'x2',
 #           f.pr='x3', f.ar='x4', ez.toe='x5',f.dda='x6')
 # 
 # # zero in the denominator
 # x5[c(1:10)]<-0
 # x6[c(1:10)]<-0
 # dat<-data.frame(x1,x2,x3,x4,x5,x6)
 # 
 # c <- get_doch(df=dat, f.cash='x1', f.si = 'x2',
 #          f.pr='x3', f.ar='x4', f.tfe='x5',f.dda='x6')
 # 
 # # winsorize at 0.025 and 0.975 percentiles instead of 0.01 and 0.99
 # d <- get_doch(df=dat, f.cash='x1', f.si = 'x2',
 #                          f.pr='x3', f.ar='x4', f.tfe='x5',f.dda='x6', winsorize=0.95 )

                 