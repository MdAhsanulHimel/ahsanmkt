
#' Import from Successive Excel Sheets
#'
#' @param path File path with file's name and extension.
#' @import openxlsx
#' @import readxl
#'
#' @return A data frame
#' @export

from_excel_sheets <- function(path){
  data <- data.frame()
  time_start()
  for(sheet in openxlsx::getSheetNames(path)){
    message("Reading ",sheet," from ",basename(path),".")

    if(nrow(data) == 0){
      data <- readxl::read_excel(path, sheet = sheet, guess_max = 1048576)
    }else{
      data <- rbind(data, readxl::read_excel(path, sheet = sheet, guess_max = 1048576))
    }
  }
  time_stop()
  return(data)
}


#' Export to Successive Excel Sheets
#'
#' @param data A data frame/tibble.
#' @param path File path and file name with extension. For example: "./myfolder/mydata.xlsx"
#' @param split_col Character specifying which column to use if considering to export to multiple sheets based on column levels.
#' @param delete Logical. TRUE to delete the column being used to split, FALSE otherwise.
#' @rawNamespace import(writexl)
#' @export
to_excel_sheets <- function(data, path, split_col = "SL", delete = TRUE){
  time_start()
  if(nrow(data) >= 1048570){
    data$SL <- rep(paste0("Sheet", 1:ceiling(nrow(data)/1048570)),
                   each = 1048570)[1:nrow(data)]
    data <- split_by_column_labels(data, split_col = split_col, delete = delete)
  }
  writexl::write_xlsx(data, path = path)
  time_stop()
}


#' split_by_column_labels
#'
#' @param data Name of the data frame.
#' @param split_col Name of the column which should be used to split.
#' @param delete Logical. TRUE to delete the column which is used to split, FALSE otherwise.
#' @rawNamespace import(dplyr)
#' @return A list with data frames split by the specified column's levels.
#' @export
split_by_column_labels <- function(data, split_col, delete = TRUE) {

  data[[split_col]] <- factor(data[[split_col]])

  splitted_data <- split(data, f = data[[split_col]])

  if(delete == TRUE){
    for (value in unique(data[[split_col]])) {
      splitted_data[[value]] <- splitted_data[[value]] %>% dplyr::select(!all_of(split_col))
    }
  }

  return(splitted_data)
}




