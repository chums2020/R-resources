library(dplyr)

# load & combine all .RData files in a directory
data.files <- list.files(path="C:/Users/yingting/data", pattern="*.RData", full.names=T, recursive=FALSE)
data.list <- lapply(data.files,load,.GlobalEnv) #load all .RData files in the list "data.files"
data.combined <-do.call(rbind, lapply(data.list, get)) # combine all .RData files in the list data.list into one data frame

# load & combine all .csv files in a directory
data.files <- list.files(path="C:/Users/yingting/data", pattern="*.csv", full.names=T, recursive=FALSE)
data.list <- lapply(data.files, function(x) {
  data <- read.csv(x, header= TRUE, sep=",", encoding = "UTF-8", stringsAsFactors = FALSE)
  colnames(data) <-c("a", "b", "c", "d") # force the column names for consistency to avoid error in the do.call(rbind, .) process
  data <- data[,1:4]
})
data.combined<-do.call(rbind, data.list)



