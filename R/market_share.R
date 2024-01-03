
#' Market Share Calculation
#'
#' @param data Name of the data frame object.
#' @param market Column name that contains the markets.
#' @param product_level Column name that contains the product levels.
#' @param base_level Product level to use as denominator when calculating market share.
#' @param attribute Column name that contains the attribute.
#' @param val Level among attribute levels that contains sales value.
#' @param vol Level among attribute levels that contains sales volume.
#' @param val_share Level among attribute levels that contains sales volume market share.
#' @param vol_share Level among attribute levels that contains sales value market share.
#' @param decimals Numeric value that specifies how many digits to retain after decimal.
#' @param progress_full Logical. TRUE to show full process as the program loops through market and product levels.
#' @import dplyr
#' @import rlang
#'
#' @return A data frame
#' @export
market_share <- function(data,
                         market = "MARKET",
                         product_level = "PROD_LEVEL",
                         base_level = "1_CATEGORY_TOTAL",
                         attribute = "ATTRIBUTE",
                         val = "Sales Offtake Value (mil)",
                         vol = "Sales Offtake Volume (ton)",
                         val_share = "Market Share (Value)",
                         vol_share = "Market Share (Volume)",
                         decimals = 7,
                         progress_full = FALSE){

  stopifnot("Attribute column not found" = attribute %in% colnames(data),
            "Product level column not found" = product_level %in% colnames(data),
            "Market column not found" = market %in% colnames(data))

  stopifnot("Passed argument in base_level not found in product level" =
              base_level %in% unique(data[[product_level]]),
            "Passed argument in val not found in attributes" =
              val %in% unique(data[[attribute]]),
            "Passed argument in vol not found in attributes" =
              vol %in% unique(data[[attribute]]),
            "Passed argument in val_share not found in attributes" =
              val_share %in% unique(data[[attribute]]),
            "Passed argument in vol_share not found in attributes" =
              vol_share %in% unique(data[[attribute]]))


  data2 <- data %>%
    filter(!(data[[attribute]] %in% c(val_share, vol_share))) %>%
    mutate(across(.cols = names(data)[1:which(names(data)==attribute)], .fns = factor))

  data_share <- data2[1,]

  i <- 0
  for (m in unique(data2[[market]])) {
    i <- i+1
    message("\n",i," Calculating Market Share in ", m)

    deno_val <- data2 %>%   # deno means Denominator
      dplyr::filter(!!dplyr::sym(market) == m,
                    !!dplyr::sym(product_level) == base_level,
                    !!dplyr::sym(attribute) == val)

    deno_vol <- data2 %>%
      dplyr::filter(!!dplyr::sym(market) == m,
                    !!dplyr::sym(product_level) == base_level,
                    !!dplyr::sym(attribute) == vol)

    for (p in unique(data[[product_level]])) {
      if(progress_full == TRUE) {message("---Product Level: ", p)}

      # Calculating Market Share Value
      nume_val <- data2 %>%  # nume means Numerator
        filter(!!dplyr::sym(market) == m,
               !!dplyr::sym(product_level) == p,
               !!dplyr::sym(attribute) == val)

      deno_val_extended <- deno_val[rep(1, each = nrow(nume_val)),]

      share_val <- data.frame(nume_val[, sapply(nume_val, is.factor)],
                              nume_val[, sapply(nume_val, is.numeric)] / deno_val_extended[, sapply(nume_val, is.numeric)],
                              check.names = F) %>%
        mutate(
          across(where(is.numeric), ~ round(., decimals)),
          !!attribute := case_when(!!dplyr::sym(attribute) == val ~ val_share)
        )

      # Calculating Market Share Volume
      nume_vol <- data2 %>%
        filter(!!dplyr::sym(market) == m,
               !!dplyr::sym(product_level) == p,
               !!dplyr::sym(attribute) == vol)

      deno_vol_extended <- deno_vol[rep(1, each = nrow(nume_vol)),]

      share_vol <- data.frame(nume_vol[, sapply(nume_vol, is.factor)],
                              nume_vol[, sapply(nume_vol, is.numeric)] / deno_vol_extended[, sapply(nume_vol, is.numeric)],
                              check.names = F) %>%
        mutate(
          across(where(is.numeric), ~ round(., decimals)),
          !!attribute := case_when(!!dplyr::sym(attribute) == vol ~ vol_share)
        )

      data_share <- rbind(data_share, share_val, share_vol)

    }
  }

  data_share <- data_share[-1,]
  return(rbind(data2, data_share))
}









#' Market Share Calculation - Single Fact
#'
#' @param data Name of the data frame object.
#' @param market Column name that contains the markets.
#' @param product_level Column name that contains the product levels.
#' @param base_level Product level to use as denominator when calculating market share.
#' @param fact Column name that contains the facts
#' @param fact_amount Level among fact levels that contains amount.
#' @param fact_share Level among fact levels that contains market share.
#' @param decimals Numeric value that specifies how many digits to retain after decimal.
#' @param progress_full Logical. TRUE to show full process as the program loops through market and product levels.
#' @import dplyr
#' @import rlang
#'
#' @return A data frame
#' @export
market_share_single <- function(data,
                                market = "Market",
                                product_level = "Product",
                                base_level = "1_CATEGORY_TOTAL",
                                fact = "Fact",
                                fact_amount = "Value (Taka)",
                                fact_share = "Value Share (%)",
                                decimals = 7,
                                progress_full = FALSE){

  stopifnot("Attribute column not found" = fact %in% colnames(data),
            "Product level column not found" = product_level %in% colnames(data),
            "Market column not found" = market %in% colnames(data))

  stopifnot("Passed argument in base_level not found in product level" =
              base_level %in% unique(data[[product_level]]),
            "Passed argument in fact_amount not found in attributes" =
              fact_amount %in% unique(data[[fact]]),
            "Passed argument in fact_share not found in attributes" =
              fact_share %in% unique(data[[fact]]))


  data2 <- data %>%
    filter(!(data[[fact]] %in% fact_share)) %>%
    mutate(across(.cols = names(data)[1:which(names(data)==fact)], .fns = factor))

  data_share <- data2[1,]

  i <- 0
  for (m in unique(data2[[market]])) {
    i <- i+1
    message("\n",i," Calculating Market Share in ", m)

    deno_val <- data2 %>%   # deno means Denominator
      dplyr::filter(!!dplyr::sym(market) == m,
                    !!dplyr::sym(product_level) == base_level,
                    !!dplyr::sym(fact) == fact_amount)

    for (p in unique(data[[product_level]])) {
      if(progress_full == TRUE) {message("---Product Level: ", p)}

      # Calculating Market Share Value
      numerator <- data2 %>%  # nume means Numerator
        filter(!!dplyr::sym(market) == m,
               !!dplyr::sym(product_level) == p,
               !!dplyr::sym(fact) == fact_amount)

      denominator <- deno_val[rep(1, each = nrow(numerator)),]

      share <- data.frame(numerator[, sapply(numerator, is.factor)],
                          numerator[, sapply(numerator, is.numeric)] / denominator[, sapply(numerator, is.numeric)],
                          check.names = F) %>%
        mutate(
          across(where(is.numeric), ~ round(., decimals)),
          !!fact := case_when(!!dplyr::sym(fact) == fact_amount ~ fact_share)
        )

      data_share <- rbind(data_share, share)

    }
  }

  data_share <- data_share[-1,]
  return(rbind(data2, data_share))
}
