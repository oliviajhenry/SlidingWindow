#' @title Apply sliding window
#' @param window_function the function which will be iteratively applied across each window
#' @param data_frame the raw data
#' @param window_size the size (length) of the window
#' @param threshold The threshold value to compare against the data (when threshold is applicable)
#' @return Iterates over time series data and applies the windows functions for every data point
#' @export
#'
apply_sliding_window <- function(window_function, data_frame, window_size, threshold = 0){
  final_results <- numeric(nrow(data_frame)) #changed from c() to store results in a vector
  for(i in 1:nrow(data_frame)){ #iterate through the raw data
    window_end <- i+(window_size -1) #defines the last index of the window, the window starts at index i
    #need to deal with the end of the data frame, when the window will be shorter to prevent code from breaking:
    if((window_end) >  nrow(data_frame)){ #if the last index of the window is > the last index in the data frame
      window_end <- nrow(data_frame) #trim the window until you reach the last index of the data frame
    }
    window_data <- data_frame[i:window_end,1]
    #apply the window_function to the data points in window then store in a vector
    result <- window_function(window_data, threshold)
    final_results[i] <- result #changed from final_results[[i]] to store in a vector not list
  }
  return(final_results)
}

