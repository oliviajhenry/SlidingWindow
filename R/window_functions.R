## WINDOW FUNCTIONS##

#' @title Threshold filter
#' @param window_data The data to be parsed through the function
#' @param threshold The threshold value to compare against the data
#' @return Iterates through a window to check if values are greater than a provided threshold, if so the value of highest difference to the reference value is returned
#' @export

threshold_filter <- function(window_data, threshold){
  result <- window_data[[1]]
  comp_value <- window_data[[1]] #the reference value to compare with the threshold value is the first value of the window

  #--------------------
  # AVG METHOD
  window_mean <- mean(window_data)
  difference <- abs(window_mean - comp_value)
  if(difference >= threshold){
    result <- window_mean
  }
  #--------------------

  #--------------------
  # MAX DIFFERENCE METHOD
  # difference <- 0
  # max_difference <- 0
  # for(i in 1:length(window_data)){
  #   difference <- abs(comp_value - window_data[[i]])
  #   if(difference > max_difference){
  #     max_difference <- difference
  #     if(max_difference >threshold){
  #       result <- window_data[[i]]
  #     }
  #   }
  # }
  #--------------------


  return(result)
}

#' @title Binary filter
#' @param window_data The data to be parsed through the function
#' @returns Iterates through a window to returning a +1/-1 depending on whether the value increases or decreases from the beginning to the end of the window
#' @export
#'

binary_filter <- function(window_data, dummy_variable=0){
  comp_value <- window_data[[1]] #by default the first value of the window will be used as the reference point to compare against

  #-------------------
  # AVG METHOD
  window_mean <- mean(window_data)
  difference <- window_mean - comp_value
  if(difference >= 0){ #i.e. if the difference is greater than the comp value
    value <- 1
  }
  else{
    value <- -1
  }
  #-----------------

  #---------------------
  # DIFFERENCE BTW END AND START OF WINDOW METHOD
  # end_window_value <- window_data[[length(window_data)]]
  # if (end_window_value - comp_value >= 0){
  #   value <- 1
  # }
  # else{
  #   value <- -1
  # }
  #-------------------

  return( value )
}
