

#' Title
#'
#' @param data
#' @param val
#' @param vol
#' @param val_share
#' @param vol_share
#' @param top_level
#' @param market
#' @param product_level
#' @param decimals
#' @param progress_full
#'
#' @return A data frame with market share calculated.
#' @export
#'
#' @examples
market_share <- function(data,
                         val = "Sales Offtake Value (mil)",
                         vol = "Sales Offtake Volume (ton)",
                         val_share = "Market Share (Value)",
                         vol_share = "Market Share (Volume)",
                         top_level = "1_CATEGORY_TOTAL",
                         market = "MARKET",
                         product_level = "PROD_LEVEL",
                         decimals = 7, progress_full = FALSE){

  data2 <- data %>%
    filter(!(Attribute %in% c(val_share, vol_share))) %>%
    mutate_if(is.character, as.factor)

  data_share <- data2[1,]

  i <- 0
  for (m in unique(data2[[market]])) {
    i <- i+1
    message("\n",i," Calculating Market Share in ", m)

    deno_val <- data2 %>%   # deno means Denominator
      dplyr::filter(MARKET == m, PROD_LEVEL == top_level, Attribute == val)

    deno_vol <- data2 %>%
      dplyr::filter(MARKET == m, PROD_LEVEL == top_level, Attribute == vol)

    for (p in unique(data[[product_level]])) {
      if(progress_full == TRUE) {message("---Product Level: ", p)}

      # Calculating Market Share Value
      nume_val <- data2 %>%  # nume means Numerator
        filter(MARKET == m, PROD_LEVEL == p, Attribute == val)

      deno_val_extended <- deno_val[rep(1, each = nrow(nume_val)),]

      share_val <- data.frame(nume_val[, sapply(nume_val, is.factor)],
                              nume_val[, sapply(nume_val, is.numeric)] / deno_val_extended[, sapply(nume_val, is.numeric)],
                              check.names = F) %>%
        mutate(
          across(where(is.numeric), ~ round(., decimals)),
          Attribute = case_when(Attribute == val ~ val_share)
        )

      # Calculating Market Share Volume
      nume_vol <- data2 %>%
        filter(MARKET == m, PROD_LEVEL == p, Attribute == vol)

      deno_vol_extended <- deno_vol[rep(1, each = nrow(nume_vol)),]

      share_vol <- data.frame(nume_vol[, sapply(nume_vol, is.factor)],
                              nume_vol[, sapply(nume_vol, is.numeric)] / deno_vol_extended[, sapply(nume_vol, is.numeric)],
                              check.names = F) %>%
        mutate(
          across(where(is.numeric), ~ round(., decimals)),
          Attribute = case_when(Attribute == vol ~ vol_share)
        )


      data_share <- rbind(data_share, share_val, share_vol)

    }
  }
  data_share <- data_share[-1,]
  return(rbind(data2, data_share))
}
