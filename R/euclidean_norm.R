#' @title Calculating euclidean norm
#' @param data_file_path path to load data file
#' @param dimensions vector containing the dimensions (i.e. # of columns)
#' @return The euclidean norm of the input sequential data
#' @export

euclidean_norm <- function(data_file_path, dimensions) {
  data <- read.csv(data_file_path)
  euclidean_data <- 0
  data$time <- as.POSIXct(data$time, format="%H:%M:%S") #change date to class POSIXct and only keep the time input
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
