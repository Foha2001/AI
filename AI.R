#--------Title:  Predicting energy commodity prices amidst -------------
#-------worldwide energy transitions using deep learning models --

library(readxl)
library(xts)
library(writexl)
library(xgboost)
library(nnet)
library(stats)
library(dplyr)
library(tidyr)
library(ggplot2)
library(fdesc)
library(mltsp)
library(e1071)
library(flexsurv)
library(keras)
library(neuralnet)
library(zoo)
library(caret)
library(NeuralNetTools)
#************import data********************-----
excel_file <- "C:/Users/Foued Azuz 14/OneDrive/AI energy/dataset.xlsx"
sheet_names <- excel_sheets(excel_file)
all_dataframes <- lapply(sheet_names, function(sheet) {
    read_excel(excel_file, sheet = sheet)
  })

all_dataframes <- lapply(all_dataframes, as.data.frame)
  
for (i in 1:22) {
    
    all_dataframes[[i]][["date"]] <- as.Date(all_dataframes[[i]][["date"]])

}
merged_df <- all_dataframes[[1]]
  for (i in 2:length(all_dataframes)) {
    df <- all_dataframes[[i]]
      merged_df <- merge(merged_df, df, by = 'date', all = TRUE)
  }
  
#merged_df$date <- as.Date(merged_df$date)
#merged_df <- merged_df[!is.na(merged_df$date), ]
xts_obj <- xts(merged_df[, -1], order.by = merged_df$date)
write_xlsx(merged_df, "file.xlsx")
#---------------------plot data--------------------------

ecp <-na.omit(merged_df[,c(1,2,3,4)])  #energy commodities prices
colnames(ecp)<- c("Date","OIL","GAS","Coal")


df <- ecp %>%
  gather(key = "Variable", value = "value", -Date)
ggplot(df, aes(x = Date, y = value, linetype = Variable)) + 
  geom_line(aes(color = Variable), size = 1) +
  scale_color_manual(values = c("#59504E", "#59504E", "#222222")) +
  theme(legend.position = c(0.1, 0.80))+
  labs(color = "Commodity", linetype = "Commodity")+
theme(panel.background = element_blank())+
  theme(axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
        axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'))




#------------prepare the dataset correct missing value--------------####
column_names <- colnames(merged_df)
start_date <- as.Date("2006-10-16")
end_date <- as.Date("2023-07-24")
mask <- merged_df$date >= start_date & merged_df$date <= end_date
subset_df <- merged_df[mask, ]
subset_df_xts<-as.xts(subset_df)

#Replace missing value#####


#replace missing values with the values in the end of year  repeate for every column
zoogeothermal<-zoo(subset_df[, "geothermal"],
                              order.by = subset_df$date)
filled_zoogeothermal <- na.locf(zoogeothermal,
                                           fromLast = TRUE)
filled_df <- data.frame(Date = index(filled_zoogeothermal), 
                        geothermal = 
                          coredata(filled_zoogeothermal))
colnames(filled_df)<-c("date","geothermal")

subset_df <- merge(subset_df, filled_df, by = "date", all.x = TRUE)
subset_df$geothermal.x<- NULL
subset_df$geothermal<-subset_df$geothermal.y
subset_df$geothermal.y<- NULL

copysubset_df<-subset_df
#end
#replace for missing month

subset_df <- subset_df %>%
  mutate(date = as.Date(date, format = "%Y-%m-%d")) %>%
  group_by(year = format(date, "%Y"), month = format(date, "%m")) %>%
  mutate(UMP = ifelse(is.na(UMP), first(UMP, na_rm = TRUE), UMP)) %>%
  ungroup()

subset_df<-subset_df[,-37,-38]


################function to replace missing value with mean value

#end

################


my_function <- function(column) {
  column <- ifelse(is.na(column), mean(column, na.rm = TRUE), column)
  return(column)
}

subset_df_filled <- lapply(subset_df, function(x) {
  if(is.numeric(x)) {
    # Apply the function only to numeric columns
    return(my_function(x))
  } else {
    # For non-numeric columns, return them as is
    return(x)
  }
})
subset_df_filled <- as.data.frame(subset_df_filled)
subset_df_filled_xts <- as.xts(subset_df_filled)
mytable <- desc(subset_df_filled_xts)
export(mytable)


#define functions ------------------------------
RMSE <- function (true_values,predictions) {
  squared_diff <- (true_values - predictions)^2
  
  mean_squared_diff <- mean(squared_diff)
  
  rmse <- sqrt(mean_squared_diff)
  
  cat("Root Mean Squared Error (RMSE):", rmse, "\n")
  
}



Rsquared <- function(true_values,predictions) {
  correlation <- cor(true_values, predictions)
  rsquared <- correlation^2
  
  cat("R-squared (R²):", rsquared, "\n")
  
}



MAE <- function (true_values,predictions) {
  
  abs_diff <- abs(true_values - predictions)
  mad <- mean(abs_diff)
  cat("Mean Absolute Deviation (MAD):", mad, "\n") 
  
}

# NARX model using neuralnet for WTI####
exoge <-subset_df_filled_xts[,-1]
lag1_target <- lag(subset_df_filled_xts$wti, 1)
colnames(lag1_target) <- "wti1"
lag2_target <- lag(subset_df_filled_xts$wti, 2)
colnames(lag2_target) <- "wti2"
time <- 1:6126
wti<-subset_df_filled_xts$wti
data <- data.frame(wti, exoge, lag1_target, lag2_target)
data <- na.omit(data)
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

maxmindata <- as.data.frame(lapply(data, normalize))

## Split the data into training and testing sets
n<-6124
train_frac <- 0.8
train_idx <- 1:round(train_frac * n)
test_idx <- (round(train_frac * n) + 1):n
training_data <- maxmindata[train_idx,]
testing_data <- maxmindata[test_idx,]
## Define the NARX formula#

formula <- wti ~ wti1 + wti2 +GP+coal+ TB+LTY+SRV+GOP+GIPIO+M2+IPI+UMP+EEPH+#macro factors
  TEMP+co2_per_capita+#envt factors
  solarpv+solarthermal+solarpvthermalhybrid+wind+hydropower+marineandtidal+bioenergy+geothermal+#technological factors
  bioenergyLCOE+geothermalLCOE+ offshorewindLCOE+solarphotovoltaicLCOE+concentratedsolarLCOE+
  hydropowerLCOE+onshorewindLCOE+AC_WIND_E+AC_SOLAR_E+AC_HYD_E+#renewable factors
  GR+EPI# political factors


formula <- wti ~ wti1+wti2+TB+LTY+I+SRV+GOP+GIPIO+M2+IPI+UMP+ EEPH+#macro factors
 GP+coal 

formula <- wti ~ wti1 + wti2+TEMP+co2_per_capita+#envt factors
  solarpv+solarthermal+solarpvthermalhybrid+wind+hydropower+marineandtidal+bioenergy+geothermal+#technological factors
  bioenergyLCOE+geothermalLCOE+ offshorewindLCOE+solarphotovoltaicLCOE+concentratedsolarLCOE+
  hydropowerLCOE+onshorewindLCOE+AC_WIND_E+AC_SOLAR_E+AC_HYD_E+#renewable factors
  GR+EPI# political factors



temp_test <- testing_data[,-1] # change this according to dependent variable


### Choose the best model##

grid <-  expand.grid(layer1 = c(1, 2,3),
                     layer2 = c(1, 2,3),
                     layer3 = c(1,2,3))
###with all variables##
nn <- train(formula, 
            data = training_data, 
            method = "neuralnet", 
            tuneGrid = grid,
            metric = "RMSE",
            preProc = c("center", "scale", "nzv"), #good idea to do this with neural nets - your error is due to non scaled data
            trControl = trainControl(
              method = "cv",
              number = 5,
              verboseIter = TRUE)
)


### with macro variables###
nn1 <- train(formula, 
             data = training_data, 
             method = "neuralnet", 
             tuneGrid = grid,
             metric = "RMSE",
             preProc = c("center", "scale", "nzv"), #good idea to do this with neural nets - your error is due to non scaled data
             trControl = trainControl(
               method = "cv",
               number = 5,
               verboseIter = TRUE)
)

### with without macro economic factors ###

nn3 <- train(formula, 
             data = training_data, 
             method = "neuralnet", 
             tuneGrid = grid,
             metric = "RMSE",
             preProc = c("center", "scale", "nzv"), #good idea to do this with neural nets - your error is due to non scaled data
             trControl = trainControl(
               method = "cv",
               number = 5,
               verboseIter = TRUE)
)


### plot the model ###

plot(nn)

tiff("nn.jpg",width = 10, height = 10, units = 'in', res = 350) #for high resolution
plotnet(nn$finalModel,cex_val =0.5,"","WTI")
title("Oil wti")
dev.off()


#repeat for each model 
predictions <-  predict(nn3, newdata = testing_data)
tiff("pred_without_mac.jpg",width = 10, height = 5, units = 'in', res = 350) #for high resolution
plot(testing_data$wti, predictions, 
     main = "Actual vs. Predicted",
     xlab = "Actual",
     ylab = "Predicted")
abline(0, 1, col = "red") 
dev.off()







# NARX model using neuralnet for OIL####
exoge <-subset_df_filled_xts[,-2]
lag1_target <- lag(subset_df_filled_xts$GP, 1)
colnames(lag1_target) <- "gp1"
lag2_target <- lag(subset_df_filled_xts$GP, 2)
colnames(lag2_target) <- "gp2"
time <- 1:6126
GP<-subset_df_filled_xts$GP
data <- data.frame(GP, exoge, lag1_target, lag2_target)
data <- na.omit(data)
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

maxmindata <- as.data.frame(lapply(data, normalize))

## Split the data into training and testing sets
n<-6124
train_frac <- 0.8
train_idx <- 1:round(train_frac * n)
test_idx <- (round(train_frac * n) + 1):n
training_data <- maxmindata[train_idx,]
testing_data <- maxmindata[test_idx,]
## Define the NARX formula#
formula <- GP ~ gp1 + gp2 +wti+coal+ TB+LTY+SRV+GOP+GIPIO+M2+IPI+UMP+EEPH+#macro factors
  TEMP+co2_per_capita+#envt factors
  solarpv+solarthermal+solarpvthermalhybrid+wind+hydropower+marineandtidal+bioenergy+geothermal+#technological factors
  bioenergyLCOE+geothermalLCOE+ offshorewindLCOE+solarphotovoltaicLCOE+concentratedsolarLCOE+
  hydropowerLCOE+onshorewindLCOE+AC_WIND_E+AC_SOLAR_E+AC_HYD_E+#renewable factors
  GR+EPI# political factors



formula <- GP ~ gp1 + gp2+TB+LTY+I+SRV+GOP+GIPIO+M2+IPI+UMP+ EEPH+#macro factors
  GP+coal 

formula <- GP ~ gp1 + gp2+TEMP+co2_per_capita+#envt factors
  solarpv+solarthermal+solarpvthermalhybrid+wind+hydropower+marineandtidal+bioenergy+geothermal+#technological factors
  bioenergyLCOE+geothermalLCOE+ offshorewindLCOE+solarphotovoltaicLCOE+concentratedsolarLCOE+
  hydropowerLCOE+onshorewindLCOE+AC_WIND_E+AC_SOLAR_E+AC_HYD_E+#renewable factors
  GR+EPI# political factors



temp_test <- testing_data[,-1] # change this according to dependent variable


### Choose the best model##

grid <-  expand.grid(layer1 = c(1, 2,3),
                     layer2 = c(1, 2,3),
                     layer3 = c(1,2,3))

###with all variables##
nnGP <- train(formula, 
              data = training_data, 
              method = "neuralnet", 
              tuneGrid = grid,
              metric = "RMSE",
              preProc = c("center", "scale", "nzv"), #good idea to do this with neural nets - your error is due to non scaled data
              trControl = trainControl(
                method = "cv",
                number = 5,
                verboseIter = TRUE)
)

nnGP  # find the best model

### with macro variables###
nn1GP <- train(formula, 
               data = training_data, 
               method = "neuralnet", 
               tuneGrid = grid,
               metric = "RMSE",
               preProc = c("center", "scale", "nzv"), #good idea to do this with neural nets - your error is due to non scaled data
               trControl = trainControl(
                 method = "cv",
                 number = 5,
                 verboseIter = TRUE)
)
nn1GP  # find the best model
### without macro economic factors ###

nn3GP <- train(formula, 
               data = training_data, 
               method = "neuralnet", 
               tuneGrid = grid,
               metric = "RMSE",
               preProc = c("center", "scale", "nzv"), #good idea to do this with neural nets - your error is due to non scaled data
               trControl = trainControl(
                 method = "cv",
                 number = 5,
                 verboseIter = TRUE)
)

nn3GP


### plot the model ###

plot(nnGP)  # for choosing the best model

tiff("nn3GP.jpg",width = 10, height = 10, units = 'in', res = 350) #for high resolution
plotnet(nn3GP$finalModel,cex_val =0.5,"","GAS")
title("GAS")
dev.off()


#repeat for each model 
predictions <-  predict(nn1GP, newdata = testing_data)
tiff("pred_onlymacro.jpg",width = 10, height = 5, units = 'in', res = 350) #for high resolution
plot(testing_data$GP, predictions, 
     main = "Actual vs. Predicted",
     xlab = "Actual",
     ylab = "Predicted")
abline(0, 1, col = "red") 
dev.off()




























#---------------------------------END---------------------------------------------



























 