###---------------------------------------------------
###   ADD PIPE OPERATOR TO THE PACKAGE
###---------------------------------------------------
#' @importFrom magrittr "%>%"



###---------------------------------------------------
###   DEBT TO ASSET RATIO
###---------------------------------------------------

#' @title
#' Debt to Asset Ratio 
#'
#' @description
#' Calculate the debt to asset ratio and append it to the dataframe. 
#'
#' @param df A dataframe containing the required field for computing the metric. The metric will be appended to this dataset.
#' @param debt Total liabilities (On 990: Part X, line 26B; On EZ: Part II, line 26B).
#' @param assets Total assets, EOY (On 990: Part X, line 16B; On EZ: Part II, line 25B).
#' @param winsorize The winsorization value (between 0 and 1), defaults to 0.98 which winsorizes at 99th and 1st percentile values.   
#' 
#' @return The original dataframe appended with the debt to asset ratio (`dar`), 
#'  a winsorized version (`dar.w`), a standardized z-score version (`dar.z`), 
#'  and a percentile version (`dar.p`).   
#'
#' @details Total-debt-to-total-assets is a leverage ratio that defines the total 
#'  amount of debt relative to assets owned by a company. Using this metric, 
#'  analysts can compare one company's leverage with that of other companies 
#'  in the same industry. This information can reflect how financially stable 
#'  a company is. The higher the ratio, the higher the degree of leverage (DoL) and, 
#'  consequently, the higher the risk (\href{Investopedia}{https://www.investopedia.com/terms/t/totaldebttototalassets.asp}).
#' 
#' 
#' @examples
#' #' x1 <- rnorm( 1000,100,30 )
#' x2 <- rnorm( 1000,200,30 )
#' x2[ c(15,300,600) ] <- 0
#' dat <- data.frame( x1,x2 )
#' d <- get_der( df=dat, debt="x1", equity="x2" )
#' head( d )
#'
#' # winsorize at 0.025 and 0.975 percentiles instead of 0.01 and 0.99
#' d <- get_der( df=dat, debt="x1", assets="x2", winsorize=0.95 )
#' 
#' @export
get_dar <- function( df, debt, assets, winsorize=0.98 )
{

  d <- df[[ debt ]]
  a <- df[[ assets ]]

  if( winsorize > 1 | winsorize < 0 )
  { stop( "winsorize argument must be 0 < w < 1" ) }

  # can't divide by zero
  print( paste0( "Assets cannot be zero: ", sum( a==0 ), " cases have been replaced with NA." ) )
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

  df.dar <- cbind( df, DAR )
  return( df.dar )
}


# x1 <- rnorm(1000,100,30)
# x2 <- rnorm(1000,200,30)
# x2[ c(15,300,600) ] <- 0
# d <- get_dar( debt=x1, assets=x2 )







core <-
  core %>% 
  dplyr::rename( 
          ORG_EIN = ein,
          RETURN_TYPE = tax.form, 
          TAX_YEAR = taxyr,
          F9_00_TAX_PERIOD_END_DATE = tax_pd,  
          F9_10_LIAB_ACC_PAYABLE_EOY = accntspayableend,
          F9_10_ASSET_ACC_NET_EOY = accntsrcvblend,
          F9_09_EXP_DEPREC_TOT = deprcatndepletn,
          F9_01_EXP_GRANT_SIMILAR_CY = grntspayableend,
          F9_10_ASSET_INV_SALE_EOY = invntriesalesend,
          F9_10_ASSET_LAND_BLDG_EOY = lndbldgsequipend,
          F9_10_ASSET_CASH_EOY = nonintcashend,
          F9_10_ASSET_PLEDGE_NET_EOY = pldgegrntrcvblend,
          F9_10_ASSET_EXP_PREPAID_EOY = prepaidexpnsend,
          F9_10_ASSET_CASH_SAVING_EOY = svngstempinvend,
          F9_10_ASSET_TOT_EOY = totassetsend,
          F9_01_EXP_TOT_CY = totfuncexpns,
          F9_10_LIAB_TOT_EOY = totliabend,
          F9_10_NAFB_TOT_EOY = totnetassetend,
          F9_08_REV_PROG_TOT_TOT = totprgmrevnue,
          F9_01_REV_TOT_CY = totrevenue,
          F9_10_NAFB_UNRESTRICT_EOY = unrstrctnetasstsend,
          F9_08_REV_CONTR_TOT = totcntrbgfts,
          F9_08_REV_OTH_FUNDR_NET_TOT = netincfndrsng,
          F9_08_REV_OTH_INVEST_INCOME_TOT = invstmntinc, 
          F9_08_REV_OTH_INVEST_BOND_TOT = txexmptbndsproceeds, 
          F9_08_REV_OTH_RENT_NET_TOT = netrntlinc, 
          F9_08_REV_OTH_SALE_GAIN_NET_TOT = netgnls, 
          F9_08_REV_OTH_ROY_TOT = royaltsinc, 
          F9_08_REV_OTH_INV_NET_TOT = netincsales, 
          F9_08_REV_MISC_TOT_TOT = miscrevtot11e  )



### operating margin

change in net assets <- ( unrestricted.net.assets.cy - unrestricted.net.assets.py ) / unrestricted.net.assets.py


                   
### months of cash on hand

operating_cash <- ( core$nonintcashend + core$svngstempinvend + 
                         core$pldgegrntrcvblend + core$accntsrcvblend )
                         
      
### current ratio


liquidity <- ( core$nonintcashend + core$svngstempinvend + 
                         core$pldgegrntrcvblend + core$accntsrcvblend + 
                         core$invntriesalesend + core$prepaidexpnsend )

### can't divide by zero
payables <- ( core$accntspayableend + core$grntspayableend )
payables[ payables == 0 ] <- NA

core$currentratio <-  liquidity / payables


### quick ratio

# TEMPORARY VARIABLES 
quick_assets  <- (core$nonintcashend+core$svngstempinvend+core$pldgegrntrcvblend+core$accntsrcvblend)
payables <- ( core$accntspayableend+core$grntspayableend)

# can't divide by zero
payables[ payables == 0 ] <- NA

# SAVE RESULTS 
core$quick_ratio <-  quick_assets / payables


### income reliance ratio



### debt to asset ratio



### Management & general expense ratio
### government grants ratio
### debt equity ratio
### assets to revenue ratio
### self-sufficiency ratio
### debt management ratio



### land to assets ratio 

# TEMPORARY VARIABLES 
landvalue  <- ( core$lndbldgsequipend)
assets <- ( core$totassetsend)

# can't divide by zero
assets[ assets == 0 ] <- NA

# SAVE RESULTS 
core$lb_to_assets <-  landvalue / assets


###---------------------------------------------------
###   LAND TO ASSET RATIO
###---------------------------------------------------

#' @title
#' Debt to Asset Ratio 
#'
#' @description
#' Calculate the debt to asset ratio and append it to the dataframe. 
#'
#' @param debt Total liabilities (On 990: Part X, line 26B; On EZ: Part II, line 26B).
#' @param assets Total assets, EOY (On 990: Part X, line 16B; On EZ: Part II, line 25B).
#' @param winsorize The winsorization value (between 0 and 1), defaults to 0.98 which winsorizes at 99th and 1st percentile values.   
#' 
#' @return The original dataframe appended with the debt to asset ratio (`dar`), 
#'  a winsorized version (`dar.w`), a standardized z-score version (`dar.z`), 
#'  and a percentile version (`dar.p`).   
#'
#' @details Total-debt-to-total-assets is a leverage ratio that defines the total 
#'  amount of debt relative to assets owned by a company. Using this metric, 
#'  analysts can compare one company's leverage with that of other companies 
#'  in the same industry. This information can reflect how financially stable 
#'  a company is. The higher the ratio, the higher the degree of leverage (DoL) and, 
#'  consequently, the higher the risk (\href{Investopedia}{https://www.investopedia.com/terms/t/totaldebttototalassets.asp}).
#' 
#' 
#' @examples
#' x1 <- rnorm(1000,100,30)
#' x2 <- rnorm(1000,200,30)
#' x2[ c(15,300,600) ] <- 0
#' dat <- data.frame(x1,x2)
#' d <- get_dar( df=dat, debt="x1", assets="x2" )
#' head( d )
#'
#' # winsorize at 0.025 and 0.975 percentiles instead of 0.01 and 0.99
#' d <- get_dar( df=dat, debt="x1", assets="x2", winsorize=0.95 )
#' 
#' @export
get_lar <- function( df, land, assets, winsorize=0.98 )
{

  d <- df[[ land ]]
  a <- df[[ assets ]]

  if( winsorize > 1 | winsorize < 0 )
  { stop( "winsorize argument must be 0 < w < 1" ) }

  # can't divide by zero
  print( paste0( "Assets cannot be zero: ", sum(a==0), " cases have been replaced with NA." ) )
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

  df.dar <- cbind( df, DAR )
  return( df.dar )
}

