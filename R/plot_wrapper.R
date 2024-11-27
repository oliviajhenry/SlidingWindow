#' @title ggplot2 wrapper function
#' @param data data frame with a time column and the list of columns in column_names
#' @param column_names columns from the data frame to be included in the graph
#' @return returns plots of input columns
#' @example plot_wrapper(test, c('euclidean_data', 'mean'))
#' @export

##Updated version with ggplot2 and corrected time display
plot_wrapper <- function(data, column_names) {

  # change to long format for ggplot2
  data_long <- data %>%
    dplyr::select(time, all_of(column_names)) %>%  #select only the time column and columsn specified in column_names
    pivot_longer(cols = -time, names_to = "variable", values_to = "value") #gather all columns except time to long format: names of old cols to names variable and corresponding values to new value col

  # Create ggplot
  ggplot(data_long, aes(x = time, y = value, color = variable)) +
    geom_line() +
    theme_minimal() +
    labs(x = "Time", y = "Value", color = "Variable") +
    scale_x_datetime()  # To handle the time variable display

}

##OLD version with plot()
# plot_wrapper <- function(data, column_names){
#
#   colors = c("black", "red", "blue")
#  for( ii in 1:length(column_names)) {
#    if (ii == 1){
#      plot(data$time, data[[column_names[[ii]]]], type="l", col=colors[[ii]])
#    }
#    else{
#      lines(data$time, data[[column_names[[ii]]]], col=colors[[ii]])
#    }
#  }
#
# }
