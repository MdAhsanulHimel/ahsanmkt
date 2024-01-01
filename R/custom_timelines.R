

#' Custom Period Calculation - Quarters
#'
#' @param data Transformed data frame where months are in the columns.
#' @param years Years when quarters need to be calculated.
#' @param decimals Number of decimal places to retain.
#' @param att_column Column name in the data frame that contains attributes.
#' @param att_average Attribute levels that need to be calculated by taking average.
#' @param att_sum Attribute levels that need to be calculated by taking sum.
#' @param att_end_month Attribute levels that need to be calculated by taking end_month.
#'
#' @return A data frame with quarters added.
#' @export
cp_quarter <- function(data,
                       years = c(21:23), decimals = 7,
                       att_column = "ATTRIBUTE",
                       att_average = c("PDOVAL", "STR", "ND_OOS", "WD_OOS","OOS_STORNO","MSVAL","MSVOL"),
                       att_sum = c("SALVAL","SALVOL", "SALQTY","PURVOL", "PURQTY","STKVOL", "STKQTY"),
                       att_end_month = c("WDPERC","NDPERC","SAHPERC","STORENO")){

  stopifnot("At least one of the attributes to average is not found in data" = unique(att_average %in% unique(data[[att_column]])),
            "At least one of the attributes to sum is not found in data" = unique(att_sum %in% unique(data[[att_column]])),
            "At least one of the attributes to end_month is not found in data" = unique(att_end_month %in% unique(data[[att_column]]))
            )

  data_average <- data %>% filter(!!dplyr::sym(att_column) %in% att_average) %>% droplevels()
  data_sum <- data %>% filter(!!dplyr::sym(att_column) %in% att_sum) %>% droplevels()
  data_end_month <- data %>% filter(!!dplyr::sym(att_column) %in% att_end_month) %>% droplevels()

  for(y in years){
    for(q in 1:4){

      if(q == 1){
        pattern <- paste0("^(",paste(month.abb[1:3], collapse = "|"), ")'(", paste(y, collapse = "|"), ")$")
      }else if(q == 2){
        pattern <- paste0("^(",paste(month.abb[4:6], collapse = "|"), ")'(", paste(y, collapse = "|"), ")$")
      }else if(q == 3){
        pattern <- paste0("^(",paste(month.abb[7:9], collapse = "|"), ")'(", paste(y, collapse = "|"), ")$")
      }else if(q == 4){
        pattern <- paste0("^(",paste(month.abb[10:12], collapse = "|"), ")'(", paste(y, collapse = "|"), ")$")
      }

      columns <- grep(pattern, names(data), value = TRUE)

      if(length(columns) == 3){
        colname <- paste0("Q",q,"'", y); message(colname)

        data_average[colname] <- round(rowSums(data_average[,columns])/3, decimals)
        data_sum[colname] <- round(rowSums(data_sum[,columns]), decimals)
        data_end_month[colname] <- data_end_month[, columns[length(columns)]] %>% as.vector()
      }

    }
  }

  data_q_added <- rbind(data_average, data_sum, data_end_month)
  data_average <- data_sum <- data_end_month <- NULL;

  return(data_q_added)
}








#' Custom Period Calculation - Half Yearly
#'
#' @param data Transformed data frame where months are in the columns.
#' @param years Years when half years need to be calculated.
#' @param decimals Number of decimal places to retain.
#' @param att_column Column name in the data frame that contains attributes.
#' @param att_average Attribute levels that need to be calculated by taking average.
#' @param att_sum Attribute levels that need to be calculated by taking sum.
#' @param att_end_month Attribute levels that need to be calculated by taking end_month.
#'
#' @return A data frame with half year custom periods added.
#' @export
cp_half_yearly <- function(data,
                           years = c(21:23),
                           decimals = 7,
                           att_column = "ATTRIBUTE",
                           att_average = c("PDOVAL", "STR", "ND_OOS", "WD_OOS", "OOS_STORNO", "MSVAL", "MSVOL"),
                           att_sum = c("SALVAL", "SALVOL", "SALQTY", "PURVOL", "PURQTY", "STKVOL", "STKQTY"),
                           att_end_month = c("WDPERC", "NDPERC", "SAHPERC", "STORENO")){

  stopifnot("At least one of the attributes to average is not found in data" = unique(att_average %in% unique(data[[att_column]])),
            "At least one of the attributes to sum is not found in data" = unique(att_sum %in% unique(data[[att_column]])),
            "At least one of the attributes to end_month is not found in data" = unique(att_end_month %in% unique(data[[att_column]]))
  )

  data_average <- data %>% filter(!!dplyr::sym(att_column) %in% att_average) %>% droplevels()
  data_sum <- data %>% filter(!!dplyr::sym(att_column) %in% att_sum) %>% droplevels()
  data_end_month <- data %>% filter(!!dplyr::sym(att_column) %in% att_end_month) %>% droplevels()

  for(y in years){
    for(h in 1:2){

      if(h == 1){
        pattern <- paste0("^(",paste(month.abb[1:6], collapse = "|"), ")'(", paste(y, collapse = "|"), ")$")
      }else if(h == 2){
        pattern <- paste0("^(",paste(month.abb[7:12], collapse = "|"), ")'(", paste(y, collapse = "|"), ")$")
      }

      columns <- grep(pattern, names(data), value = TRUE)

      if(length(columns) == 6){
        colname <- paste0("H",h,"'", y); message(colname)

        data_average[colname] <- round(rowSums(data_average[,columns])/6, decimals)
        data_sum[colname] <- round(rowSums(data_sum[,columns]), decimals)
        data_end_month[colname] <- data_end_month[, columns[length(columns)]] %>% as.vector()
      }

    }
  }

  data_h_added <- rbind(data_average, data_sum, data_end_month)
  data_average <- data_sum <- data_end_month <- NULL;

  return(data_h_added)
}






#' Custom Period Calculation - Half Yearly
#'
#' @param data Transformed data frame where months are in the columns.
#' @param monthyear Years when Last N Months need to be calculated.
#' @param n Number of months to use for calculation.
#' @param decimals Number of decimal places to retain.
#' @param att_column Column name in the data frame that contains attributes.
#' @param att_average Attribute levels that need to be calculated by taking average.
#' @param att_sum Attribute levels that need to be calculated by taking sum.
#' @param att_end_month Attribute levels that need to be calculated by taking end_month.
#'
#' @return A data frame with Last N Months calculation added.
#' @export
cp_last_n_month <- function(data, monthyear, n, decimals = 7,
                            att_column = "ATTRIBUTE",
                            att_average = c("PDOVAL", "STR", "ND_OOS", "WD_OOS", "OOS_STORNO", "MSVAL", "MSVOL"),
                            att_sum = c("SALVAL", "SALVOL", "SALQTY", "PURVOL", "PURQTY", "STKVOL", "STKQTY"),
                            att_end_month = c("WDPERC", "NDPERC", "SAHPERC", "STORENO")){

  data_average <- data %>% filter(!!dplyr::sym(att_column) %in% att_average) %>% droplevels()
  data_sum <- data %>% filter(!!dplyr::sym(att_column) %in% att_sum) %>% droplevels()
  data_end_month <- data %>% filter(!!dplyr::sym(att_column) %in% att_end_month) %>% droplevels()

  column_names <- colnames(data)   # all column names of data frame

  stopifnot("Passed monthyear not found in columns." = any(column_names %in% monthyear))

  col_number <- which(column_names %in% monthyear)
  columns <- column_names[c(col_number-n+1):col_number]

  colname <- paste0("L",n,"M'",substr(monthyear, 5,6)); message(colname)

  data_average[colname] <- round(rowSums(data_average[,columns])/n, decimals)
  data_sum[colname] <- round(rowSums(data_sum[,columns]), decimals)
  data_end_month[colname] <- data_end_month[, columns[length(columns)]] %>% as.vector()

  data_LNM_added <- rbind(data_average, data_sum, data_end_month)
  data_average <- data_sum <- data_end_month <- NULL;

  return(data_LNM_added)
}
