# fiscal 

An R package for calculating nonprofit fiscal health accounting metrics from IRS 990 data. 

<br>

---------------------

<br>

## Install the package

```r

devtools::install_github( 'nonprofit-open-data-collective/fiscal' )

```

<br>
<hr>
<br>

## Example Ratio: DAR

```r
library( fiscal )
help( get_dar )  # function documentation 
```

**Description**
Calculate the debt to asset ratio and append it to the dataframe.

```
dar = ( short term debt + long term debt ) / total assets 
```

**Usage**
```
  get_dar( df, 
           debt   = c( "F9_10_LIAB_TOT_EOY", "F9_01_NAFB_LIAB_TOT_EOY" ),
           assets = c( "F9_10_ASSET_TOT_EOY", "F9_01_NAFB_ASSET_TOT_EOY" ),
           winsorize = 0.98,
           sanitize  = TRUE,
           summarize = FALSE )
```

**Arguments**

* `df`: A data.frame containing the required fields for computing the metric. The metric will be appended to this dataset.
* `debt`: Column name(s) for total liabilities. (On 990: Part X, line 26B; `F9_10_LIAB_TOT_EOY`. On EZ: Part II, line 26B; `F9_01_NAFB_LIAB_TOT_EOY`.)
* `assets`: Column name(s) for total assets, EOY. (On 990: Part X, line 16B; `F9_10_ASSET_TOT_EOY`. On EZ: Part II, line 25B; `F9_01_NAFB_ASSET_TOT_EOY`.)
* `winsorize`: The winsorization value (between 0 and 1), defaults to 0.98 which winsorizes at the 1st and 99th percentile values.
* `sanitize`: If `TRUE` (default), imputes zero for NA values in financial fields before computing the ratio, respecting form scope.
* `summarize`: If `TRUE`, prints summary statistics and plots density curves for all four output columns.

```r
df <- dat10k   # sample of 10,000 rows from efile financials

# test the debt-to-asset ratio function
df <- get_dar( df )

# [1] "Assets equal to zero: 3 cases have been replaced with NA."
#
#       dar               dar_w             dar_z              dar_p       
#  Min.   :-0.08372   Min.   :0.09009   Min.   :-2.36716   Min.   :  1.00  
#  1st Qu.: 0.39247   1st Qu.:0.39247   1st Qu.:-0.67281   1st Qu.: 25.00  
#  Median : 0.50623   Median :0.50623   Median :-0.03536   Median : 50.00  
#  Mean   : 0.51315   Mean   :0.51254   Mean   : 0.00000   Mean   : 50.35  
#  3rd Qu.: 0.62853   3rd Qu.:0.62853   3rd Qu.: 0.64995   3rd Qu.: 75.00  
#  Max.   : 1.24623   Max.   :1.00150   Max.   : 2.73988   Max.   :100.00  
#  NA's   :3          NA's   :3         NA's   :3          NA's   :3

# fiscal health metrics have been appended to the original dataframe
head( df ) 

# defaults to efile variable names
# but you can override with your own column names
df <- get_dar( df, debt = "my_liabilities", assets = "my_assets" )

# the function is pipe-enabled 
df <- 
  df %>% 
  get_dar()
  
# compute all ratio metrics at once
df <- compute_all( df, metrics = c("ratio","w","z","p") )
```

The `metrics` argument controls which versions are returned:

| Value | Description |
|-------|-------------|
| `"ratio"` | Raw financial ratio |
| `"w"` | Winsorized version |
| `"z"` | Z-score (standardized) version |
| `"p"` | Percentile rank (1–100) |

The `append_to_df` argument controls the output shape:

| Value | Returns |
|-------|---------|
| `TRUE` (default) | Original data frame with metric columns appended |
| `FALSE` | Efile identifier columns + metric columns only |

<br>
<hr>
<br>

## Results 

The functions are designed to create multiple versions of the fiscal health metric, print summary statistics, and visualize the density distribution. The four versions are added back to the original dataset. 

For example, the Debt to Asset Ratio function **get_dar()** creates the following: 

* `dar`   — the raw debt-to-asset ratio (DAR) 
* `dar_w` — the winsorized version of DAR 
* `dar_z` — DAR standardized as a z-score 
* `dar_p` — DAR expressed as a percentile rank  

```r
df <- get_dar( df = dat10k, summarize = TRUE )

# [1] "Assets equal to zero: 3 cases have been replaced with NA."
#
#       dar               dar_w             dar_z              dar_p       
#  Min.   :-0.08372   Min.   :0.09009   Min.   :-2.36716   Min.   :  1.00  
#  1st Qu.: 0.39247   1st Qu.:0.39247   1st Qu.:-0.67281   1st Qu.: 25.00  
#  Median : 0.50623   Median :0.50623   Median :-0.03536   Median : 50.00  
#  Mean   : 0.51315   Mean   :0.51254   Mean   : 0.00000   Mean   : 50.35  
#  3rd Qu.: 0.62853   3rd Qu.:0.62853   3rd Qu.: 0.64995   3rd Qu.: 75.00  
#  Max.   : 1.24623   Max.   :1.00150   Max.   : 2.73988   Max.   :100.00  
#  NA's   :3          NA's   :3         NA's   :3          NA's   :3
```

![](assets/dar.png)


<br>
<hr>
<br>


**ADD ALL METRICS TO DATASET**

```r
library( fiscal )

# retrieve efile data directly from NCCS
df <- retrieve_efile_data( year = 2021 )

# compute all metrics at once
df <- compute_all( df )

# or add individual metrics
df <- get_aer( df )     # Administrative Overhead Ratio
df <- get_arr( df )     # Asset Revenue Ratio
df <- get_brr( df )     # Burn Rate Ratio
df <- get_casr( df )    # Cash Ratio
df <- get_coh( df )     # Cash on Hand
df <- get_cr( df )      # Current Ratio
df <- get_dar( df )     # Debt to Asset Ratio
df <- get_der( df )     # Debt to Equity Ratio
df <- get_dgdr( df )    # Donation/Grant Dependence Ratio
df <- get_dmr( df )     # Debt to Net Assets Ratio
df <- get_doch( df )    # Days of Cash on Hand
df <- get_doci( df )    # Days of Cash and Investments
df <- get_eidr( df )    # Earned Income Dependency Ratio
df <- get_er( df )      # Equity Ratio
df <- get_fer( df )     # Fundraising Efficiency Ratio
df <- get_ggr( df )     # Government Grant Ratio
df <- get_iidr( df )    # Investment Income Dependency Ratio
df <- get_lar( df )     # Land Asset Ratio
df <- get_luna( df )    # Liquid Unrestricted Net Assets
df <- get_moch( df )    # Months of Cash on Hand
df <- get_nacr( df )    # Net Assets Composition Ratio
df <- get_or( df )      # Operating Ratio
df <- get_orr( df )     # Operating Reserve Ratio
df <- get_per( df )     # Program Expense Ratio
df <- get_podpm( df )   # Post-Depreciation Profit Margin
df <- get_predpm( df )  # Pre-Depreciation Profit Margin
df <- get_qr( df )      # Quick Ratio
df <- get_roa( df )     # Return on Assets
df <- get_rona( df )    # Return on Net Assets
df <- get_sm( df )      # Surplus Margin
df <- get_ssr( df )     # Self Sufficiency Ratio
df <- get_stdr( df )    # Short Term Debt Ratio
```

<br>
<hr>
<br>

# Metrics Reference

---

## get_aer()

**Ratio:** Administrative Overhead Ratio

**Definition:** Share of total expenses devoted to management and general administration.

**Formula:**
```
aer = administrative_expenses / total_expenses
```

| Argument | efile Variable | Description |
|----------|---------------|-------------|
| `mgmt_expenses` | `F9_09_EXP_TOT_MGMT` | Management and general expenses |
| `total_expenses` | `F9_09_EXP_TOT_TOT` | Total functional expenses |

**Scope:** 990 filers only

---

## get_arr()

**Ratio:** Asset Revenue Ratio

**Definition:** Measures revenue generated per dollar of assets.

**Formula:**
```
arr = total_assets_eoy / total_revenue
```

| Argument | efile Variable | Description |
|----------|---------------|-------------|
| `total_assets` | `F9_10_ASSET_TOT_EOY` | Total assets, end of year (990) |
| `total_revenue` | `F9_08_REV_TOT_TOT` | Total revenue from Part VIII (990) |
| `total_revenue` | `F9_01_REV_TOT_CY` | Total revenue from Part I (990EZ fallback) |

**Scope:** 990 + 990EZ filers

---

## get_brr()

**Ratio:** Burn Rate Ratio

**Definition:** Rate at which the organization depletes its cash reserves relative to a prior period.

**Formula:**
```
brr = cash_eoy / cash_boy
```

| Argument | efile Variable | Description |
|----------|---------------|-------------|
| `cash_eoy` | `F9_10_ASSET_CASH_EOY` | Cash, end of year |
| `cash_boy` | `F9_10_ASSET_CASH_BOY` | Cash, beginning of year |

**Scope:** 990 filers only

---

## get_casr()

**Ratio:** Cash Ratio

**Definition:** Measures the organization's ability to cover current liabilities using only cash and savings.

**Formula:**
```
casr = ( cash + savings ) / ( accounts_payable + grants_payable )
```

| Argument | efile Variable | Description |
|----------|---------------|-------------|
| `cash` | `F9_10_ASSET_CASH_EOY` | Cash on hand, EOY |
| `savings` | `F9_10_ASSET_SAVING_EOY` | Savings and temporary cash investments |
| `accounts_payable` | `F9_10_LIAB_ACC_PAYABLE_EOY` | Accounts payable and accrued expenses |
| `grants_payable` | `F9_10_LIAB_GRANT_PAYABLE_EOY` | Grants and similar amounts payable |

**Scope:** 990 filers only

---

## get_coh()

**Ratio:** Cash on Hand

**Definition:** Absolute dollar amount of liquid cash holdings (cash plus savings).

**Formula:**
```
coh = cash + savings
```

| Argument | efile Variable | Description |
|----------|---------------|-------------|
| `cash` | `F9_10_ASSET_CASH_EOY` | Cash on hand, EOY |
| `savings` | `F9_10_ASSET_SAVING_EOY` | Savings and temporary cash investments |

**Scope:** 990 filers only

---

## get_cr()

**Ratio:** Current Ratio

**Definition:** Measures short-term liquidity by comparing current assets to current liabilities.

**Formula:**
```
cr = current_assets / current_liabilities

current_assets      = cash + savings + pledges_receivable + accounts_receivable
                      + investment_sales + prepaid_expenses
current_liabilities = accounts_payable + grants_payable
```

| Argument | efile Variable | Description |
|----------|---------------|-------------|
| `cash` | `F9_10_ASSET_CASH_EOY` | Cash on hand, EOY |
| `savings` | `F9_10_ASSET_SAVING_EOY` | Savings and temporary cash investments |
| `pledges_receivable` | `F9_10_ASSET_PLEDGE_NET_EOY` | Net pledges receivable |
| `accounts_receivable` | `F9_10_ASSET_ACC_NET_EOY` | Accounts receivable, net |
| `investment_sales` | `F9_10_ASSET_INV_SALE_EOY` | Investments held for sale |
| `prepaid_expenses` | `F9_10_ASSET_EXP_PREPAID_EOY` | Prepaid expenses and deferred charges |
| `accounts_payable` | `F9_10_LIAB_ACC_PAYABLE_EOY` | Accounts payable and accrued expenses |
| `grants_payable` | `F9_10_LIAB_GRANT_PAYABLE_EOY` | Grants and similar amounts payable |

**Scope:** 990 filers only

---

## get_dar()

**Ratio:** Debt to Asset Ratio

**Definition:** Proportion of total assets financed through liabilities.

**Formula:**
```
dar = total_liabilities / total_assets
```

| Argument | efile Variable | Description |
|----------|---------------|-------------|
| `debt` | `F9_10_LIAB_TOT_EOY` | Total liabilities, EOY (990) |
| `debt` | `F9_01_NAFB_LIAB_TOT_EOY` | Total liabilities from Part I (990EZ fallback) |
| `assets` | `F9_10_ASSET_TOT_EOY` | Total assets, EOY (990) |
| `assets` | `F9_01_NAFB_ASSET_TOT_EOY` | Total assets from Part I (990EZ fallback) |

**Scope:** 990 + 990EZ filers

---

## get_der()

**Ratio:** Debt to Equity Ratio

**Definition:** Compares total liabilities to unrestricted net assets.

**Formula:**
```
der = total_liabilities / unrestricted_net_assets
```

| Argument | efile Variable | Description |
|----------|---------------|-------------|
| `debt` | `F9_10_LIAB_TOT_EOY` | Total liabilities, EOY |
| `equity` | `F9_10_NAFB_UNRESTRICT_EOY` | Unrestricted net assets, EOY |

**Scope:** 990 filers only

---

## get_dgdr()

**Ratio:** Donation and Grant Dependence Ratio

**Definition:** Measures reliance on contributions and fundraising as a share of total revenue.

**Formula:**
```
dgdr = ( contributions + fundraising_revenue ) / total_revenue
```

| Argument | efile Variable | Description |
|----------|---------------|-------------|
| `contributions` | `F9_08_REV_CONTR_TOT` | Total contributions |
| `fundraising_revenue` | `F9_08_REV_OTH_FUNDR_NET_TOT` | Net fundraising event revenue |
| `total_revenue` | `F9_08_REV_TOT_TOT` | Total revenue |

**Scope:** 990 filers only

---

## get_dmr()

**Ratio:** Debt to Net Assets Ratio

**Definition:** Compares total liabilities to unrestricted net assets.

**Formula:**
```
dmr = total_liabilities / unrestricted_net_assets
```

| Argument | efile Variable | Description |
|----------|---------------|-------------|
| `liabilities` | `F9_10_LIAB_TOT_EOY` | Total liabilities, EOY (990) |
| `liabilities` | `F9_01_NAFB_LIAB_TOT_EOY` | Total liabilities from Part I (990EZ fallback) |
| `net_assets` | `F9_10_NAFB_UNRESTRICT_EOY` | Unrestricted net assets, EOY |

**Scope:** 990 + 990EZ filers

---

## get_doch()

**Ratio:** Days of Cash on Hand

**Definition:** Number of days an organization can operate using available liquid assets.

**Formula:**
```
doch = liquid_assets / daily_expenses

liquid_assets  = cash + savings + pledges_receivable + accounts_receivable
daily_expenses = ( total_expenses - depreciation ) / 365
```

| Argument | efile Variable | Description |
|----------|---------------|-------------|
| `cash` | `F9_10_ASSET_CASH_EOY` | Cash on hand |
| `savings` | `F9_10_ASSET_SAVING_EOY` | Savings |
| `pledges_receivable` | `F9_10_ASSET_PLEDGE_NET_EOY` | Net pledges receivable |
| `accounts_receivable` | `F9_10_ASSET_ACC_NET_EOY` | Accounts receivable |
| `total_expenses` | `F9_09_EXP_TOT_TOT` | Total functional expenses |
| `depreciation` | `F9_09_EXP_DEPREC_TOT` | Depreciation and amortization |

**Scope:** 990 filers only

---

## get_doci()

**Ratio:** Days of Cash and Investments

**Definition:** Days of operating coverage including investment assets net of related debt.

**Formula:**
```
doci = liquid_and_investment_assets / daily_expenses

liquid_and_investment_assets = unrestricted_net_assets + investments
                               - ( land_buildings - mortgages_payable )
daily_expenses               = ( total_expenses - depreciation ) / 365
```

| Argument | efile Variable | Description |
|----------|---------------|-------------|
| `net_assets` | `F9_10_NAFB_UNRESTRICT_EOY` | Unrestricted net assets, EOY |
| `investments` | `F9_10_ASSET_INV_SALE_EOY` | Investments held for sale |
| `land_buildings` | `F9_10_ASSET_LAND_BLDG_NET_EOY` | Net land, buildings, and equipment |
| `mortgages_payable` | `F9_10_LIAB_MTG_NOTE_EOY` | Mortgages and notes payable |
| `total_expenses` | `F9_09_EXP_TOT_TOT` | Total functional expenses |
| `depreciation` | `F9_09_EXP_DEPREC_TOT` | Depreciation and amortization |

**Scope:** 990 filers only

---

## get_eidr()

**Ratio:** Earned Income Dependency Ratio

**Definition:** Share of total revenue from earned program and commercial income sources.

**Formula:**
```
eidr = earned_revenue / total_revenue

earned_revenue = program_service_rev + membership_dues + royalties + other_revenue
```

| Argument | efile Variable | Description |
|----------|---------------|-------------|
| `program_service_rev` | `F9_08_REV_PROG_TOT_TOT` | Program service revenue |
| `membership_dues` | `F9_08_REV_CONTR_MEMBSHIP_DUE` | Membership dues |
| `royalties` | `F9_08_REV_OTH_ROY_TOT` | Royalties |
| `other_revenue` | `F9_08_REV_MISC_OTH_TOT` | Other miscellaneous revenue |
| `total_revenue` | `F9_08_REV_TOT_TOT` | Total revenue |

**Scope:** 990 filers only

---

## get_er()

**Ratio:** Equity Ratio

**Definition:** Share of total assets financed through net assets (organizational equity).

**Formula:**
```
er = net_assets / total_assets
```

| Argument | efile Variable | Description |
|----------|---------------|-------------|
| `net_assets` | `F9_10_NAFB_TOT_EOY` | Total net assets, EOY |
| `total_assets` | `F9_10_ASSET_TOT_EOY` | Total assets, EOY |

**Scope:** 990 + 990EZ filers

---

## get_fer()

**Ratio:** Fundraising Efficiency Ratio

**Definition:** Cost of raising one dollar of contributions; lower values indicate more efficient fundraising.

**Formula:**
```
fer = fundraising_expenses / total_contributions
```

| Argument | efile Variable | Description |
|----------|---------------|-------------|
| `fundraising_expenses` | `F9_09_EXP_TOT_FUNDR` | Total fundraising expenses |
| `total_contributions` | `F9_08_REV_CONTR_TOT` | Total contributions received |

**Scope:** 990 filers only

---

## get_ggr()

**Ratio:** Government Grant Ratio

**Definition:** Share of total revenue originating from government grants.

**Formula:**
```
ggr = government_grants / total_revenue
```

| Argument | efile Variable | Description |
|----------|---------------|-------------|
| `government_grants` | `F9_08_REV_CONTR_GOVT_GRANT` | Government grants and contributions |
| `total_revenue` | `F9_08_REV_TOT_TOT` | Total revenue |

**Scope:** 990 filers only

---

## get_iidr()

**Ratio:** Investment Income Dependency Ratio

**Definition:** Measures reliance on investment-related revenue as a share of total revenue.

**Formula:**
```
iidr = investment_income / total_revenue

investment_income = invest_income + bond_proceeds + rent_income + asset_sale_income
```

| Argument | efile Variable | Description |
|----------|---------------|-------------|
| `invest_income` | `F9_08_REV_OTH_INVEST_INCOME_TOT` | Investment income |
| `bond_proceeds` | `F9_08_REV_OTH_INVEST_BOND_TOT` | Income from bond proceeds |
| `rent_income` | `F9_08_REV_OTH_RENT_GRO_PERS` | Gross rental income |
| `asset_sale_income` | `F9_08_REV_OTH_SALE_ASSET_OTH` | Net gain from asset sales |
| `total_revenue` | `F9_08_REV_TOT_TOT` | Total revenue |

**Scope:** 990 filers only

---

## get_lar()

**Ratio:** Land Asset Ratio

**Definition:** Share of total assets invested in land, buildings, and equipment.

**Formula:**
```
lar = land_buildings / total_assets
```

| Argument | efile Variable | Description |
|----------|---------------|-------------|
| `land_buildings` | `F9_10_ASSET_LAND_BLDG_DEPREC` | Land, buildings, and equipment (net of depreciation) |
| `total_assets` | `F9_10_ASSET_TOT_EOY` | Total assets, EOY |

**Scope:** 990 filers only

---

## get_luna()

**Ratio:** Liquid Unrestricted Net Assets

**Definition:** Unrestricted net assets available for operations, net of illiquid fixed assets and related debt, expressed as months of expenses.

**Formula:**
```
luna = ( unrestricted_net_assets - net_fixed_assets + mortgages_payable )
       / monthly_expenses

monthly_expenses = total_expenses / 12
```

| Argument | efile Variable | Description |
|----------|---------------|-------------|
| `unrestricted_net_assets` | `F9_10_NAFB_UNRESTRICT_EOY` | Unrestricted net assets, EOY |
| `net_fixed_assets` | `F9_10_ASSET_LAND_BLDG_NET_EOY` | Net land, buildings, and equipment |
| `mortgages_payable` | `F9_10_LIAB_MTG_NOTE_EOY` | Mortgages and notes payable |
| `total_expenses` | `F9_09_EXP_TOT_TOT` | Total functional expenses |

**Scope:** 990 filers only

---

## get_moch()

**Ratio:** Months of Cash on Hand

**Definition:** Number of months the organization can operate using available liquid assets.

**Formula:**
```
moch = liquid_assets / monthly_expenses

liquid_assets    = cash + savings + pledges_receivable + accounts_receivable
monthly_expenses = ( total_expenses - depreciation ) / 12
```

| Argument | efile Variable | Description |
|----------|---------------|-------------|
| `cash` | `F9_10_ASSET_CASH_EOY` | Cash on hand |
| `savings` | `F9_10_ASSET_SAVING_EOY` | Savings |
| `pledges_receivable` | `F9_10_ASSET_PLEDGE_NET_EOY` | Net pledges receivable |
| `accounts_receivable` | `F9_10_ASSET_ACC_NET_EOY` | Accounts receivable |
| `total_expenses` | `F9_09_EXP_TOT_TOT` | Total functional expenses |
| `depreciation` | `F9_09_EXP_DEPREC_TOT` | Depreciation and amortization |

**Scope:** 990 filers only

---

## get_nacr()

**Ratio:** Net Assets Composition Ratio

**Definition:** Share of total net assets that are unrestricted and available for general operations.

**Formula:**
```
nacr = unrestricted_net_assets / total_net_assets
```

| Argument | efile Variable | Description |
|----------|---------------|-------------|
| `unrestricted_net_assets` | `F9_10_NAFB_UNRESTRICT_EOY` | Unrestricted net assets, EOY |
| `total_net_assets` | `F9_10_NAFB_TOT_EOY` | Total net assets, EOY |

**Scope:** 990 filers only

---

## get_or()

**Ratio:** Operating Ratio

**Definition:** Year-over-year change in net assets as a proportion of beginning-of-year net assets.

**Formula:**
```
or = ( net_assets_eoy - net_assets_boy ) / net_assets_boy
```

| Argument | efile Variable | Description |
|----------|---------------|-------------|
| `net_assets_eoy` | `F9_10_NAFB_TOT_EOY` | Total net assets, EOY (990) |
| `net_assets_eoy` | `F9_01_NAFB_TOT_EOY` | Net assets from Part I, EOY (990EZ fallback) |
| `net_assets_boy` | `F9_10_NAFB_TOT_BOY` | Total net assets, BOY (990) |
| `net_assets_boy` | `F9_01_NAFB_TOT_BOY` | Net assets from Part I, BOY (990EZ fallback) |

**Scope:** 990 + 990EZ filers

---

## get_orr()

**Ratio:** Operating Reserve Ratio

**Definition:** Months of operating expenses covered by liquid unrestricted net assets.

**Formula:**
```
orr = ( unrestricted_net_assets - net_fixed_assets ) / monthly_expenses

monthly_expenses = total_expenses / 12
```

| Argument | efile Variable | Description |
|----------|---------------|-------------|
| `unrestricted_net_assets` | `F9_10_NAFB_UNRESTRICT_EOY` | Unrestricted net assets, EOY |
| `net_fixed_assets` | `F9_10_ASSET_LAND_BLDG_NET_EOY` | Net land, buildings, and equipment |
| `total_expenses` | `F9_09_EXP_TOT_TOT` | Total functional expenses |

**Scope:** 990 filers only

---

## get_per()

**Ratio:** Program Expense Ratio

**Definition:** Share of total expenses devoted to program services.

**Formula:**
```
per = program_expenses / total_expenses
```

| Argument | efile Variable | Description |
|----------|---------------|-------------|
| `program_expenses` | `F9_09_EXP_TOT_PROG` | Program service expenses (990) |
| `total_expenses` | `F9_09_EXP_TOT_TOT` | Total functional expenses (990) |
| `total_expenses` | `F9_01_EXP_TOT_CY` | Total expenses from Part I (990EZ fallback) |

**Scope:** 990 + 990EZ filers

---

## get_podpm()

**Ratio:** Post-Depreciation Profit Margin

**Definition:** Operating surplus or deficit as a share of total revenue, after depreciation.

**Formula:**
```
podpm = ( total_revenue - total_expenses ) / total_revenue
```

| Argument | efile Variable | Description |
|----------|---------------|-------------|
| `expenses` | `F9_09_EXP_TOT_TOT` | Total functional expenses |
| `revenue` | `F9_08_REV_TOT_TOT` | Total revenue |

**Scope:** 990 filers only

---

## get_predpm()

**Ratio:** Pre-Depreciation Profit Margin

**Definition:** Operating surplus or deficit as a share of total revenue, before depreciation.

**Formula:**
```
predpm = ( total_revenue - ( total_expenses - depreciation ) ) / total_revenue
```

| Argument | efile Variable | Description |
|----------|---------------|-------------|
| `revenue` | `F9_08_REV_TOT_TOT` | Total revenue |
| `expenses` | `F9_09_EXP_TOT_TOT` | Total functional expenses |
| `depreciation` | `F9_09_EXP_DEPREC_TOT` | Depreciation and amortization |

**Scope:** 990 filers only

---

## get_qr()

**Ratio:** Quick Ratio

**Definition:** Measures liquidity using only the most liquid assets, excluding inventory and prepaid expenses.

**Formula:**
```
qr = quick_assets / current_liabilities

quick_assets        = cash + savings + pledges_receivable + accounts_receivable
current_liabilities = accounts_payable + grants_payable
```

| Argument | efile Variable | Description |
|----------|---------------|-------------|
| `cash` | `F9_10_ASSET_CASH_EOY` | Cash on hand |
| `savings` | `F9_10_ASSET_SAVING_EOY` | Savings |
| `pledges_receivable` | `F9_10_ASSET_PLEDGE_NET_EOY` | Net pledges receivable |
| `accounts_receivable` | `F9_10_ASSET_ACC_NET_EOY` | Accounts receivable |
| `accounts_payable` | `F9_10_LIAB_ACC_PAYABLE_EOY` | Accounts payable and accrued expenses |
| `grants_payable` | `F9_10_LIAB_GRANT_PAYABLE_EOY` | Grants and similar amounts payable |

**Scope:** 990 filers only

---

## get_roa()

**Ratio:** Return on Assets

**Definition:** Net surplus or deficit as a share of total assets.

**Formula:**
```
roa = revenues_less_expenses / total_assets
```

| Argument | efile Variable | Description |
|----------|---------------|-------------|
| `revenues_less_expenses` | `F9_01_EXP_REV_LESS_EXP_CY` | Revenues less expenses, current year |
| `total_assets` | `F9_10_ASSET_TOT_EOY` | Total assets, EOY (990) |
| `total_assets` | `F9_01_NAFB_ASSET_TOT_EOY` | Total assets from Part I (990EZ fallback) |

**Scope:** 990 + 990EZ filers

---

## get_rona()

**Ratio:** Return on Net Assets

**Definition:** Net surplus or deficit as a share of beginning-of-year net assets.

**Formula:**
```
rona = revenues_less_expenses / net_assets_boy
```

| Argument | efile Variable | Description |
|----------|---------------|-------------|
| `revenues_less_expenses` | `F9_01_EXP_REV_LESS_EXP_CY` | Revenues less expenses, current year |
| `net_assets_boy` | `F9_10_NAFB_TOT_BOY` | Total net assets, BOY (990) |
| `net_assets_boy` | `F9_01_NAFB_TOT_BOY` | Net assets from Part I, BOY (990EZ fallback) |

**Scope:** 990 + 990EZ filers

---

## get_sm()

**Ratio:** Surplus Margin

**Definition:** Net surplus or deficit as a share of total revenue.

**Formula:**
```
sm = revenues_less_expenses / total_revenue
```

| Argument | efile Variable | Description |
|----------|---------------|-------------|
| `revenues_less_expenses` | `F9_01_EXP_REV_LESS_EXP_CY` | Revenues less expenses, current year |
| `total_revenue` | `F9_08_REV_TOT_TOT` | Total revenue from Part VIII (990) |
| `total_revenue` | `F9_01_REV_TOT_CY` | Total revenue from Part I (990EZ fallback) |

**Scope:** 990 + 990EZ filers

---

## get_ssr()

**Ratio:** Self Sufficiency Ratio

**Definition:** Measures whether program revenue covers total expenses without relying on donations.

**Formula:**
```
ssr = program_service_revenue / total_expenses
```

| Argument | efile Variable | Description |
|----------|---------------|-------------|
| `program_service_rev` | `F9_08_REV_PROG_TOT_TOT` | Program service revenue (990) |
| `program_service_rev` | `F9_01_REV_PROG_TOT_CY` | Program revenue from Part I (990EZ fallback) |
| `total_expenses` | `F9_09_EXP_TOT_TOT` | Total functional expenses (990) |
| `total_expenses` | `F9_01_EXP_TOT_CY` | Total expenses from Part I (990EZ fallback) |

**Scope:** 990 + 990EZ filers

---

## get_stdr()

**Ratio:** Short Term Debt Ratio

**Definition:** Share of short-term liabilities relative to total net assets.

**Formula:**
```
stdr = short_term_liabilities / net_assets

short_term_liabilities = accounts_payable + grants_payable
```

| Argument | efile Variable | Description |
|----------|---------------|-------------|
| `accounts_payable` | `F9_10_LIAB_ACC_PAYABLE_EOY` | Accounts payable and accrued expenses |
| `grants_payable` | `F9_10_LIAB_GRANT_PAYABLE_EOY` | Grants and similar amounts payable |
| `net_assets` | `F9_10_NAFB_TOT_EOY` | Total net assets, EOY |

**Scope:** 990 filers only

---
