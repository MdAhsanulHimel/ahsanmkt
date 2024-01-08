
<!-- README.md is generated from README.Rmd. Please edit that file -->

**This package provides some useful functions for performing data
analysis on large market survey data sets.**

# ahsanmkt

<!-- badges: start -->
<!-- badges: end -->

This is currently in development stage. Please contact before using it.

## Installation

You can install the development version of ahsanmkt from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("MdAhsanulHimel/ahsanmkt")
```

## Usage

The following code imports data from `market_data_202312.xlsx` file and
stores in the object named `data_raw`. It reads data from all the sheets
sequentially and stores them in a single data frame. Note: This function
cannot import data from xlsb files. So before using it, convert the
files from xlsx to xlsb.

``` r
data_raw <- ahsanmkt::from_excel_sheets("F:/Data/market_data_202312.xlsx")
```

To send YM from rows to columns and send attributes/facts from columns
to rows, use the following code. Run
`?ahsanmkt::transform_date_rows_to_cols` in the console to know more
about the function `transform_date_rows_to_cols()`.

``` r
data_transformed <- ahsanmkt::transform_date_rows_to_cols(
  data,
  decimals = 7,
  date_col = "YM",
  div_100 = c("WDPERC","NDPERC","SAHPERC","ND_OOS","WD_OOS","MSVAL","MSVOL","FMCGND"),
  div_1000 = c("SALVAL", "SALVOL", "PURVOL", "STKVOL"),
  mult_1000 = c("PDOVAL"),
  dup_attribute = "WDPERC",
  dup_market = "MARKET",
  dup_market_level = "ALL BGD",
  dup_item_desc = "ITEMDESC"
)
```

Add custom periods using functions whose initials start with `cp_`. Run
`?ahsanmkt::cp_half_yearly` in the console to know more about the
function `cp_half_yearly()`.

Add quarterly:

``` r
data_q_added <- ahsanmkt::cp_quarter(
  data_transformed,
  years = c(21:23),
  decimals = 7,
  att_column = "ATTRIBUTE",
  att_average = c("PDOVAL", "STR", "ND_OOS", "WD_OOS", "OOS_STORNO", "MSVAL", "MSVOL"),
  att_sum = c("SALVAL", "SALVOL", "SALQTY", "PURVOL", "PURQTY", "STKVOL", "STKQTY"),
  att_end_month = c("WDPERC", "NDPERC", "SAHPERC", "STORENO")
)
```

Add half-yearly:

``` r
data_h_added <- ahsanmkt::cp_half_yearly(
  data_q_added,
  years = c(21:23),
  decimals = 7,
  att_column = "ATTRIBUTE",
  att_average = c("PDOVAL", "STR", "ND_OOS", "WD_OOS", "OOS_STORNO", "MSVAL", "MSVOL"),
  att_sum = c("SALVAL", "SALVOL", "SALQTY", "PURVOL", "PURQTY", "STKVOL", "STKQTY"),
  att_end_month = c("WDPERC", "NDPERC", "SAHPERC", "STORENO")
)
```

Notice: Use the latest processed data so that new columns are added at
the end of the previous one.

Add last n months and MAT:

``` r
data_LNM_added <- ahsanmkt::cp_last_n_month(
  data_h_added,
  monthyear,
  n = 3, # 3 for last 3 months, n = 12 will calculate MAT
  decimals = 7,
  att_column = "ATTRIBUTE",
  att_average = c("PDOVAL", "STR", "ND_OOS", "WD_OOS", "OOS_STORNO", "MSVAL", "MSVOL"),
  att_sum = c("SALVAL", "SALVOL", "SALQTY", "PURVOL", "PURQTY", "STKVOL", "STKQTY"),
  att_end_month = c("WDPERC", "NDPERC", "SAHPERC", "STORENO")
)
```

Add n before n months (PNM):

``` r
data_PNM_added <- ahsanmkt::cp_n_before_n(
  data_LNM_added,
  monthyear,
  n = 3, # 3 for 3 months before last 3 months
  decimals = 7,
  att_column = "ATTRIBUTE",
  att_average = c("PDOVAL", "STR", "ND_OOS", "WD_OOS", "OOS_STORNO", "MSVAL", "MSVOL"),
  att_sum = c("SALVAL", "SALVOL", "SALQTY", "PURVOL", "PURQTY", "STKVOL", "STKQTY"),
  att_end_month = c("WDPERC", "NDPERC", "SAHPERC", "STORENO")
)
```

Add YTD:

``` r
data_YTD_added <- ahsanmkt::cp_jan_to_latest(
  data_PNM_added,
  monthyear = "Dec'23",
  decimals = 7,
  att_column = "ATTRIBUTE",
  att_average = c("PDOVAL", "STR", "ND_OOS", "WD_OOS", "OOS_STORNO", "MSVAL", "MSVOL"),
  att_sum = c("SALVAL", "SALVOL", "SALQTY", "PURVOL", "PURQTY", "STKVOL", "STKQTY"),
  att_end_month = c("WDPERC", "NDPERC", "SAHPERC", "STORENO")
)
```

Add MAT:

``` r
data_YTD_added <- ahsanmkt::cp_jan_to_latest(
  data_PNM_added,
  monthyear = "Dec'23",
  decimals = 7,
  att_column = "ATTRIBUTE",
  att_average = c("PDOVAL", "STR", "ND_OOS", "WD_OOS", "OOS_STORNO", "MSVAL", "MSVOL"),
  att_sum = c("SALVAL", "SALVOL", "SALQTY", "PURVOL", "PURQTY", "STKVOL", "STKQTY"),
  att_end_month = c("WDPERC", "NDPERC", "SAHPERC", "STORENO")
)
```

Add FY:

``` r
data_FY_added <- ahsanmkt::cp_full_year(
  data_YTD_added,
  Year = 23,
  decimals = 7,
  att_column = "ATTRIBUTE",
  att_average = c("PDOVAL", "STR", "ND_OOS", "WD_OOS", "OOS_STORNO", "MSVAL", "MSVOL"),
  att_sum = c("SALVAL", "SALVOL", "SALQTY", "PURVOL", "PURQTY", "STKVOL", "STKQTY"),
  att_end_month = c("WDPERC", "NDPERC", "SAHPERC", "STORENO")
)
```

The following code exports the data frame named `data_transformed` from
R to xlsx. If the number of rows exceed row limit in Excel, then the
excess data is exported to the next sheets.

``` r
ahsanmkt::to_excel_sheets(data_FY_added, "F:/Data/market_data_202312_transformed.xlsx")
```
