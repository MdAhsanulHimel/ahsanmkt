#' Seconds to Time String Conversion
#'
#' Converts a numeric value to time string
#' @param seconds numeric value containing seconds
#' @return seconds in time string
#' @examples
#' seconds_to_time_string(67)
#' seconds_to_time_string(5*60*60+9*60+58)
#' seconds_to_time_string(45*24*60*60+13*60*60+15*60+25)
#' @export
seconds_to_time_string <- function(seconds) {

  days <- floor(seconds %/% (60*60*24))
  hours <- floor((seconds / 3600) %% 24)
  minutes <- floor((seconds %% 3600)/60)
  remaining_seconds <- seconds %% 60
  time_string <- character(0L)
  if(days != 0){
    time_string <- paste0(days, " Day",ifelse(days != 1, "s", ""),
                          " ", hours, " Hour", ifelse(hours != 1, "s", ""),
                          " ", minutes, " Minute", ifelse(minutes != 1, "s", ""),
                          " ", remaining_seconds, " Second", ifelse(remaining_seconds != 1, "s", ""))

  }else if(days == 0 & hours != 0){
    time_string <- paste0(hours, " Hour", ifelse(hours != 1, "s", ""),
                          " ", minutes, " Minute", ifelse(minutes != 1, "s", ""),
                          " ", remaining_seconds, " Second", ifelse(remaining_seconds != 1, "s", ""))

  }else if(days == 0 & hours == 0 & minutes != 0){
    time_string <- paste0(minutes, " Minute", ifelse(minutes != 1, "s", ""),
                          " ", remaining_seconds, " Second", ifelse(remaining_seconds != 1, "s", ""))

  }else if(days == 0 & hours == 0 & minutes == 0 & remaining_seconds != 0){
    time_string <- paste0(remaining_seconds, " Second", ifelse(remaining_seconds != 1, "s", ""))
  }

  return(time_string)
}

#' Record Start Time of a Program
#'
#' `time_start()` starts running the stopwatch and `time_stop()` stops the stopwatch to record run time.
#' @return Several objects loaded to the global environment
#' \itemize{
#'   \item begin_time - Starting time of the program
#'   \item end_time - Ending time of the program
#'   \item runtime - Numeric value containing elapsed seconds
#' }
#' @examples
#' time_start()
#' Sys.sleep(10)
#' time_stop()
#' @export
time_start <- function (){
  timeStart <<- Sys.time()
  message("Program started: ", timeStart)
}


#' Record Stop Time of a Program
#'
#' `time_start()` starts running the stopwatch and `time_stop()` stops the stopwatch to record run time.
#' @return Several objects loaded to the global environment
#' \itemize{
#'   \item begin_time - Starting time of the program
#'   \item end_time - Ending time of the program
#'   \item runtime - Numeric value containing elapsed seconds
#' }
#' @examples
#' time_start()
#' Sys.sleep(10)
#' time_stop()
#' @export
time_stop <- function (){
  timeStop <<- Sys.time()
  runtime <<- as.numeric(format(timeStop, "%s")) - as.numeric(format(timeStart, "%s"))
  message("Program end: ", timeStop)
  message("Runtime: ", seconds_to_time_string(runtime))
}
