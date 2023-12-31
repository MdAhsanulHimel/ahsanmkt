% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/file_handling.R
\name{to_excel_sheets}
\alias{to_excel_sheets}
\title{Export to Successive Excel Sheets}
\usage{
to_excel_sheets(data, path, split_col = "SL", delete = TRUE)
}
\arguments{
\item{data}{A data frame/tibble.}

\item{path}{File path and file name with extension. For example: "./myfolder/mydata.xlsx"}

\item{split_col}{Character specifying which column to use if considering to export to multiple sheets based on column levels.}

\item{delete}{Logical. TRUE to delete the column being used to split, FALSE otherwise.}
}
\description{
Export to Successive Excel Sheets
}
