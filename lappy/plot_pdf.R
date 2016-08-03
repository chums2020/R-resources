# Read all data files from a directory (each file contains the data for one product)
# Build one model for each product: use quantity to predict prices
# Read the old model files from another directory (one file for each product)
# Plot the predictive price from the new model against the old model using ggplot (one graph for each product)
# Output all graphs in a single PDF file

#set directory and file pattern
files <- list.files(path="C:/yingting/raw_data", pattern="*.csv", full.names=T, recursive=FALSE)

#direct output to a file
pdf("myplot.pdf")

#lapply in base package
lapply(files, function(x) {
  t <- read.csv(x) # load file
  
  #delete observations with missing values
  t <- na.omit(t)
  
  #count by code
  count_by_group <- aggregate(t, by = list(t$code), FUN = length)
  colnames(count_by_group)[1:2] <- c("code","count")
  
  #count by number of data points 
  aggregate(count_by_group, by = list(count_by_group$count), FUN = length)
  
  #model: t, log
  t_log <- merge(t, count_by_group[,1:2], by = "code") 

  #model using plyr package#
  model <- dlply(t_log, "code", function(df) # code is unique to each product 
    lm(log(price) ~ quantity, data = df)) # predict price using quantity
  model_coeff <- ldply(model, coef)
  
  colnames(model_coeff) <- c("code","intercept","coef_exp")
  
  #save the .csv file to a new directory
  x_concat <- unlist(strsplit(x, split='.', fixed=TRUE))[1] 
  x_concat <- unlist(strsplit(x_concat, split='/', fixed=TRUE))[5]
  write.csv(model_coeff, file.path("C:/yingting/output", 
                                   paste(x_concat, "_coeff.csv", sep=""))
            , row.names=FALSE)
  
  ##prediction
  model_coeff$pred_price_1 <- exp(model_coeff$intercept + model_coeff$coef_exp*1)
  model_coeff$pred_price_2 <- exp(model_coeff$intercept + model_coeff$coef_exp*2)
  model_coeff$pred_price_3 <- exp(model_coeff$intercept + model_coeff$coef_exp*3)

  model_coeff_long <- melt(model_coeff, id=c("code","intercept","coef_exp"))
  model_coeff_long <- model_coeff_long[order(model_coeff_long$code),]
  levels(model_coeff_long$variable) <- 0:15
  names(model_coeff_long)[names(model_coeff_long) == 'variable'] <- 'quantity'
  
  t <- t[(t$code %in% model_coeff_long$code),]
  length(unique(t$code)) == length(unique(model_coeff_long$code))
  
  # plot --------------------------------------------------------------------
  
  #add "id"
  #Goal: each graph in the output panel shows the name of product
  
  file_name <- paste("C:/yingting/old_model",x_concat,". MODEL EQUATIONS.xlsx", sep = "")
  testData <- read_excel(file_name)
  
  # Clean duplicate id -------------------------------------------------
  
  
  #if id is not unique! (maybe duplicate code for the same car model)
  if (length(unique(testData$id)) != length(unique(testData$code))){
    
    n_occur <- data.frame(table(testData$id))
    
    #list products with different codes but the same id
    n_occur[n_occur$Freq > 1,]
    
    ###keep only those observations with unique id###
    testData <- testData[(testData$id %in% n_occur$Var1[n_occur$Freq == 1]),]
  }
  
  
  # merge -------------------------------------------------------------------
  
  
  #By default the data frames are merged on the columns with names they both have
  merge_model_coeff_long <- merge(model_coeff_long, testData, by = "code") 
  
  merge_model_t <- merge(t, testData, by = "code")
  
  # Lattice Plot ------------------------------------------------------------
  
  
  #log-transformed model
  
  xyplot_model <- xyplot(value ~ quantity |id, 
                         par.strip.text=list(cex=0.5), 
                         merge_model_coeff_long, 
                         t= 'l', col = 'blue', layout=c(3,3), xlim=c(0:15))
  
  #log-transformed model (mileage-adjusted)
  xyplot_model_adjusted <- xyplot(value_adjusted ~ quantity |id, 
                                          par.strip.text=list(cex=0.5), 
                                          merge_model_coeff_long, 
                                          t= 'l', col = 'green', layout=c(3,3), xlim=c(0:15))  
  
  
  foo_key <- list(text = list(lab = c("data","log-transformed price", "log-transformed price (adjusted)")),
                  lines = list(type = c("p", "l", "l"), col = c("red","blue", "green"),
                               pch = c("o", "", "")), 
                  title = x_concat)
  
  colnames(merge_model_t) <- c("code","quantity","value","id")
  
  #original data points
  xyplot_data <- xyplot(value ~ quantity |id, par.strip.text=list(cex=0.5), merge_model_t, 
                        key = foo_key,
                        col = 'red', layout=c(3,3), xlim=c(0:15)
  )
  xyplot_data + as.layer(xyplot_model) + as.layer(xyplot_model_adjusted) 
  
})
dev.off()
