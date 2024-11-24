#' @title Calculating euclidean norm
#'
#' @param data_file_path path to load data file
#' @param dimensions vector containing the dimensions (i.e. # of columns)
#'
#' @return The euclidean norm of the input sequential data
#'

euclidean_norm <- function(data_file_path, dimensions) {
  data <- read.csv(data_file_path)
  euclidean_data <- 0
  class(data$time) <- "POSIXct" #change class of 2nd col
  for(i in 1:nrow(data)) {

    datai_sum <- 0
    for(j in 1:length(dimensions)) {
      data_dim <- data[i, dimensions[[j]] ]
      datai_sum <- datai_sum + data_dim^2

    }

    euclidean_data[[i]] <- sqrt(datai_sum)
  }

  data$euclidean_data <- euclidean_data

  return(data)

}

#' @title Threshold filter
#' @param window_data The data to be parsed through the function
#' @param threshold The threshold value to compare against the data
#' @return Iterates through a window to check if values are greater than a provided threshold, if so the value of highest difference to the reference value is returned

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
#'
#'

binary_filter <- function(window_data, dummy_variable=0){
  comp_value <- window_data[[1]]

  #-------------------
  # AVG METHOD
  window_mean <- mean(window_data)
  difference <- window_mean - comp_value
  if(difference >= 0){
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

#' @title Apply sliding window
#' @param window_function the function which will be iteratively applied across each window
#' @param data_frame the raw data
#' @param window_size the size (length) of the window
#' @return Iterates over time series data and applies the windows functions for every data point

apply_sliding_window <- function(window_function, data_frame, window_size, threshold = 0){
  final_results <- c()
  for(i in 1:nrow(data_frame)){ #iterate through the raw data
    window_end <- i+(window_size -1) #defines the last index of the window, the window starts at index i
    if((window_end) >  nrow(data_frame)){ #if the last index of the window is > the last index in the data frame
      window_end <- nrow(data_frame) #trim the window until you reach the last index of the data frame
    }
    window_data <- data_frame[i:window_end,1] #assuming we don't need a comma
    #apply the window_function to the data points in window then store in a vector
    result <- window_function(window_data, threshold)
    final_results[[i]] <- result
  }
  return(final_results)
}

#' @title ggplot2 wrapper function
#' @param data data frame with a time column and the list of columns in column names
#' @param column_names other columns from the data frame to be included in the graph
#' @return returns plots of input columns
#' @example plot_wrapper(test, c('euclidean_data', 'mean'))

 plot_wrapper <- function(data, column_names){

   colors = c("black", "red", "blue")
  for( ii in 1:length(column_names)) {
    if (ii == 1){
      plot(data$time, data[[column_names[[ii]]]], type="l", col=colors[[ii]])
    }
    else{
      lines(data$time, data[[column_names[[ii]]]], col=colors[[ii]])
    }
  }

 }
