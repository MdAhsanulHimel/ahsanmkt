
#' Transform Data - Rows to Columns
#'
#' @param data A data frame. The first column should contain date in the format 202312, i.e., YEARMONTH
#' @param div_100 Columns that should be divided by 100.
#' @param div_1000 Columns that should be divided by 1000.
#' @param mult_1000 Columns that should be multiplied by 1000.
#' @param decimals Number of decimal places to retain.
#' @param date_col Column name that refer to the column YEARMONTH, e.g., 202312.
#' @param dup_attribute Name of any attribute level in the dataset. Just for calculating duplicates.
#' @param dup_market Column name that contains market information.
#' @param dup_market_level Name of the largest market, usually ALL BGD.
#' @param dup_item_desc Column name that contains item description.
#'
#' @return A transformed data frame.
#' @import dplyr
#' @import tidyr
#' @import beepr
#' @export
transform_date_rows_to_cols <- function
(
  data,
  decimals = 7,
  date_col = "YM",
  div_100 = c("WDPERC","NDPERC","SAHPERC","ND_OOS","WD_OOS","MSVAL","MSVOL","FMCGND"),
  div_1000 = c("SALVAL","SALVOL","PURVOL","STKVOL"),
  mult_1000 = c("PDOVAL"),
  dup_attribute = "WDPERC",
  dup_market = "MARKET",
  dup_market_level = "ALL BGD",
  dup_item_desc = "ITEMDESC"){

  time_start()
  colnames(data) <- toupper(colnames(data)) # make sure all columns are capitalized for consistency

  data_cal <- data %>%
    mutate(across(any_of(div_100), ~ .x / 100),
           across(any_of(div_1000), ~ .x / 1000),
           across(any_of(mult_1000), ~ .x * 1000))

  data_YM_edited <- data_cal %>%
    mutate(YM = paste0(!!dplyr::sym(date_col), "01")
           %>% as.Date(format = "%Y%m%d") %>%
             format(format = "%b'%y"))

  message("Sending Attributes to rows")
  data_long <- data_YM_edited %>%
    pivot_longer(cols = names(select_if(data_YM_edited, is.numeric)),
                 # cols are only the numeric columns which are actually WDPERC to FMCGND
                 # to avoid error, all other columns are recommended to be set to factor / character type
                 values_to = 'VALUE', names_to = 'ATTRIBUTE')

  message("Sending YearMonth to columns")
  data_wide <- tryCatch({

    data_long %>%
      pivot_wider(names_from = all_of(date_col), values_from = "VALUE", values_fill = 0)

  },
  error = function(e) {

    message("Duplicates found!"); beepr::beep(10)

    # dup_ arguments are used in the following part only
    duplicates <- data_cal %>%
      dplyr::select(!!dplyr::sym(date_col):!!dplyr::sym(dup_attribute)) %>%
      tidyr::pivot_longer(cols = any_of(dup_attribute),
                          values_to = 'VALUE',
                          names_to = 'ATTRIBUTE') %>%
      dplyr::group_by(pick(!!dplyr::sym(date_col):ATTRIBUTE)) %>%
      dplyr::filter(!!dplyr::sym(dup_market) == dup_market_level) %>%
      dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
      dplyr::filter(n > 1L) %>%
      dplyr::select(!!dplyr::sym(dup_item_desc), !!dplyr::sym(date_col))

    # showing duplicates on the console
    for(i in 1:nrow(duplicates)){
      message(duplicates[i,1],"     ", duplicates[i,2])
    }

    data_long %>%
      pivot_wider(names_from = date_col, values_from = "VALUE",
                  values_fn = ~ round(sum(.x, na.rm = TRUE), decimals),
                  values_fill = 0)
  })

  time_stop()

  data_wide <- data_wide %>%
  mutate(across(.cols = names(data_wide)[1:which(names(data_wide)=='ATTRIBUTE')], .fns = factor))

  return(data_wide)

}

