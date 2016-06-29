#define a date transformation function "dateRF"
#the original date format is %d-%m月-%y, e.g., '1-9月-91' stands for September 1st, 1991, '22-11月-02' stands for November 22, 2002
#the output date format is yyyy-mm-dd
dateRF <- function (x){
  x<-gsub(" ", "", x, fixed = TRUE)
  
  year <- ifelse(is.na(x), NA, paste0(ifelse(substr(x, nchar(x)-1, nchar(x))>16,19,20),substr(x, nchar(x)-1, nchar(x))))
  month <- ifelse(is.na(x), NA, substr(x, 4, nchar(x)-4))
  month <- ifelse(is.na(x), NA, ifelse(nchar(month)==1, paste0(0,month), month)) #if month only has one character, eg 9, then return 09
  day <- ifelse(is.na(x), NA, substr(x, 1, 2))
  
  return(ifelse(is.na(x), NA, paste(year,month,day, sep = "-")))
}
