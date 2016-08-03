data.files <- list.files(path="C:/Users/yingting/data", pattern="*.RData", full.names=T, recursive=FALSE)
data.list <- lapply(data.files,load,.GlobalEnv) #load all .RData files in the list "data.files"
data.combined <-do.call(rbind, lapply(data.list, get)) # combine all .RData files in the list data.list into one data frame




