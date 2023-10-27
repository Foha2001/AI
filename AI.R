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
#import data-----
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


#----------------------------------------------------#

#-------------NARX model using MLTSP-------------------

train_size <- floor(0.8 * nrow(subset_df_filled_xts))
train_data <- subset_df_filled_xts[1:train_size,]
test_data <- subset_df_filled_xts[(train_size + 1):nrow(subset_df_filled_xts),]
ind_test = index(test_data)
wti <- subset_df_filled_xts$wti
gas <- subset_df_filled_xts$GP
coal<- subset_df_filled_xts$coal


factorwithout_RE <- subset_df_filled_xts[,c(1,3,4,5,6,7,8,9,10,12,13,14)]
with_RE_fac <- subset_df_filled_xts[,c(1,3,
                                       11,35,
                                       15,16,
                                       25,26,27,28,29,30,31,32,33,34,
                                       17,18,19,20,21,22,23,24)]
#predict without any variables


#predict wti without RE factors *****###
model_gp_without = narx(train_data$GP,SimpleLM, p = 2,xreg=factorwithout_RE)
pred_gp_without = forecast(model_gp_without,xreg=factorwithout_RE[ind_test])
plot(pred_gp_without$mean)
lines(test_data$wti,col="red")

#predict wti with RE factors *****##
model_gp_with = narx(train_data$GP, SimpleLM, p = 2,xreg=with_RE_fac)
pred_gp_with = forecast(model_gp_with,xreg=with_RE_fac[ind_test])
plot(pred_gp_with$mean)
lines(test_data$wti,col="red")



RMSE(pred_gp_without$mean, test_data$GP)
Rsquared(pred_gp_without$mean, test_data$GP)
MAE(test_data$GP,pred_gp_without$mean)

#-------------NARX model using neuralnet-------------------
exoge <-subset_df_filled_xts[,-1]
lag1_target <- lag(subset_df_filled_xts$wti, 1)
colnames(lag1_target) <- "wti1"
lag2_target <- lag(subset_df_filled_xts$wti, 2)
colnames(lag2_target) <- "wti2"
time <- 1:6126
data <- data.frame(time, wti, exoge, lag1_target, lag2_target)
data <- na.omit(data)
data<-scale(data)


# Split the data into training and testing sets
n<-6124
train_frac <- 0.8
train_idx <- 1:round(train_frac * n)
test_idx <- (round(train_frac * n) + 1):n
training_data <- data[train_idx, ]
testing_data <- data[test_idx, ]
# Define the NARX formula


formula <- wti ~ wti1 + wti2 +GP+coal+ TB+LTY+SRV+GOP+GIPIO+M2+IPI+UMP+EEPH+
  TEMP+co2_per_capita+solarpv+solarthermal+solarpvthermalhybrid+wind+
  hydropower+marineandtidal+bioenergy+geothermal+bioenergyLCOE+geothermalLCOE+
  offshorewindLCOE+solarphotovoltaicLCOE+concentratedsolarLCOE+
  hydropowerLCOE+onshorewindLCOE+AC_WIND_E+AC_SOLAR_E+AC_HYD_E+
  GR+EPI

formula <- wti ~ wti1+wti2+TB+LTY+I+SRV+GOP+GIPIO+M2+IPI+UMP+EEPH+
  TEMP+co2_per_capita+GR+EPI





model <- neuralnet(formula, data = training_data, hidden = c(2,1),
                   linear.output = FALSE,threshold = 0.01,rep = 10)
                   

model$
plot(model)

# Train the model
trained_model <- model

# Make predictions on the test data
predictions <- predict(trained_model,testing_data)
plot(predictions)

RMSE(predictions,testing_data$wti)


#------------------------------------------------------------------------------























# calculate_r_squared <- function(actual_values, predicted_values) {
#   N <- length(actual_values)
#   residuals <- actual_values - predicted_values
#   ss_residuals <- sum(residuals^2)/N^2
#   ss_total <- sum((actual_values - mean(actual_values))^2)/N
#   r_squared <-  1-(((ss_residuals) / ss_total))
#   return(r_squared)
# }




# M_A_D <- function(actual_values,predicted_values) {
#   n <- length(actual_values)
#   mean_abs_dev <- sum(abs(actual_values - predicted_values)) / n
#   return(mean_abs_dev)
# }
# 






 