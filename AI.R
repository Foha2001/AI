#--------Title:  Predicting energy commodity prices amidst -------------
#-------worldwide energy transitions using deep learning models --
##library#####
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
library(tensorflow)
library(reticulate)
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

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

#******************NARX model******************####

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


tiff("nn.jpg",width = 10, height = 6, units = 'in', res = 350) #for high resolution
plot(nn3)
dev.off()



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


tiff("nn3GP.jpg",width = 10, height = 6, units = 'in', res = 350) #for high resolution
plot(nn3GP)
dev.off()


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

# NARX model using neuralnet for Coal####
exoge <-subset_df_filled_xts[,-3]
lag1_target <- lag(subset_df_filled_xts$coal, 1)
colnames(lag1_target) <- "coal1"
lag2_target <- lag(subset_df_filled_xts$coal, 2)
colnames(lag2_target) <- "coal2"
time <- 1:6126
coal<-subset_df_filled_xts$coal
data <- data.frame(coal, exoge, lag1_target, lag2_target)
data <- na.omit(data)

maxmindata <- as.data.frame(lapply(data, normalize))

## Split the data into training and testing sets
n<-6124
train_frac <- 0.8
train_idx <- 1:round(train_frac * n)
test_idx <- (round(train_frac * n) + 1):n
training_data <- maxmindata[train_idx,]
testing_data <- maxmindata[test_idx,]
## Define the NARX formula#
formula <- coal ~ coal1 + coal2 +wti+GP+ TB+LTY+SRV+GOP+GIPIO+M2+IPI+UMP+EEPH+#macro factors
  TEMP+co2_per_capita+#envt factors
  solarpv+solarthermal+solarpvthermalhybrid+wind+hydropower+marineandtidal+bioenergy+geothermal+#technological factors
  bioenergyLCOE+geothermalLCOE+ offshorewindLCOE+solarphotovoltaicLCOE+concentratedsolarLCOE+
  hydropowerLCOE+onshorewindLCOE+AC_WIND_E+AC_SOLAR_E+AC_HYD_E+#renewable factors
  GR+EPI# political factors



formula <- coal ~ coal1 + coal2+TB+LTY+I+SRV+GOP+GIPIO+M2+IPI+UMP+ EEPH+#macro factors
  GP+coal 

formula <- coal ~ coal1 + coal2+TEMP+co2_per_capita+#envt factors
  solarpv+solarthermal+solarpvthermalhybrid+wind+hydropower+marineandtidal+bioenergy+geothermal+#technological factors
  bioenergyLCOE+geothermalLCOE+ offshorewindLCOE+solarphotovoltaicLCOE+concentratedsolarLCOE+
  hydropowerLCOE+onshorewindLCOE+AC_WIND_E+AC_SOLAR_E+AC_HYD_E+#renewable factors
  GR+EPI# political factors



### Choose the best model##

grid <-  expand.grid(layer1 = c(1, 2,3),
                     layer2 = c(1, 2,3),
                     layer3 = c(1,2,3))

###with all variables##
nncoal <- train(formula, 
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

nncoal  # find the best model



### with macro variables###
nn1coal <- train(formula, 
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
nn1coal  # find the best model
### without macro economic factors ###
nn3coal <- train(formula, 
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

nn3coal

### plot the model ###


tiff("nn3coal.jpg",width = 10, height = 6, units = 'in', res = 350) #for high resolution
plot(nn3coal)
dev.off()

tiff("nn3coal.jpg",width = 10, height = 10, units = 'in', res = 350) #for high resolution
plotnet(nn3coal$finalModel,cex_val =0.5,"","Coal")
title("Coal")
dev.off()


#repeat for each model 
predictions <-  predict(nn1coal, newdata = testing_data)
tiff("pred.jpg",width = 10, height = 5, units = 'in', res = 350) #for high resolution
plot(testing_data$coal, predictions, 
     main = "Actual vs. Predicted",
     xlab = "Actual",
     ylab = "Predicted")
abline(0, 1, col = "red") 
dev.off()

#**********************ANN*****************************####
#*
# ANN model using neuralnet for WTI####
wti <- subset_df_filled_xts[,1]
lagged_data <- embed(wti, 5)
input_data <- lagged_data[, -5 ]
output_data <- lagged_data[, 5]
ann_data <- data.frame(input_data, output_data)
ann_datascale <- as.data.frame(lapply(ann_data, normalize))

#train the model
n<-6122
train_frac <- 0.8
train_idx <- 1:round(train_frac * n)
test_idx <- (round(train_frac * n) + 1):n
training_data <- ann_datascale[train_idx,]
testing_data <- ann_datascale[test_idx,]

grid <-  expand.grid(layer1 = c(1, 2,3),
                     layer2 = c(1, 2,3),
                     layer3 = c(1,2,3))


formula <- output_data ~ X1+X2+X3+X4
  

ann <- train(formula, 
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
ann



# ANN model using neuralnet for oil####
gp <- subset_df_filled_xts[,2]
lagged_data <- embed(gp, 5)
input_data <- lagged_data[, -5 ]
output_data <- lagged_data[, 5]
ann_data <- data.frame(input_data, output_data)
ann_datascale <- as.data.frame(lapply(ann_data, normalize))

#train the model
n<-6122
train_frac <- 0.8
train_idx <- 1:round(train_frac * n)
test_idx <- (round(train_frac * n) + 1):n
training_data <- ann_datascale[train_idx,]
testing_data <- ann_datascale[test_idx,]

grid <-  expand.grid(layer1 = c(1, 2,3),
                     layer2 = c(1, 2,3),
                     layer3 = c(1,2,3))


formula <- output_data ~ X1+X2+X3+X4

#ann for oil
ann1 <- train(formula, 
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
ann1


# ANN model using neuralnet for coal####
coal <- subset_df_filled_xts[,3]
lagged_data <- embed(coal, 5)
input_data <- lagged_data[, -5 ]
output_data <- lagged_data[, 5]
ann_data <- data.frame(input_data, output_data)
ann_datascale <- as.data.frame(lapply(ann_data, normalize))

#train the model
n<-6122
train_frac <- 0.8
train_idx <- 1:round(train_frac * n)
test_idx <- (round(train_frac * n) + 1):n
training_data <- ann_datascale[train_idx,]
testing_data <- ann_datascale[test_idx,]

grid <-  expand.grid(layer1 = c(1, 2,3),
                     layer2 = c(1, 2,3),
                     layer3 = c(1,2,3))


formula <- output_data ~ X1+X2+X3+X4

#ann for coal
ann2 <- train(formula, 
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
ann2


#**********************LSTM********************************####
#LSTM for WTI, Gp and coal  change each name accordingly ####
wti_diff =as.numeric(diff(wti, differences = 1))
gp_diff <- as.numeric(diff(gp, differences = 1))
coal_diff <- as.numeric(diff(coal, differences = 1))


lag_transform <- function(x, k= 1){
  
  lagged =  c(rep(NA, k), x[1:(length(x)-k)])
  DF = as.data.frame(cbind(lagged, x))
  colnames(DF) <- c( paste0('x-', k), 'x')
  DF[is.na(DF)] <- 0
  return(DF)
}
supervised = lag_transform(coal_diff, 1)
N = nrow(supervised)
n = round(N *0.8, digits = 0)
train = supervised[1:n, ]
test  = supervised[(n+1):N,  ]
scale_data = function(train, test, feature_range = c(0, 1)) {
  x = train
  fr_min = feature_range[1]
  fr_max = feature_range[2]
  std_train = ((x - min(x) ) / (max(x) - min(x)  ))
  std_test  = ((test - min(x) ) / (max(x) - min(x)  ))
  
  scaled_train = std_train *(fr_max -fr_min) + fr_min
  scaled_test = std_test *(fr_max -fr_min) + fr_min
  
  return( list(scaled_train = as.vector(scaled_train), scaled_test = as.vector(scaled_test) ,scaler= c(min =min(x), max = max(x))) )
  
}

Scaled = scale_data(train, test, c(-1, 1))

y_train = Scaled[["scaled_train"]][["x"]]
x_train = Scaled[["scaled_train"]][["x-1"]]

y_test =Scaled[["scaled_test"]][["x"]]
x_test =Scaled[["scaled_test"]][["x-1"]]

invert_scaling = function(scaled, scaler, feature_range = c(0, 1)){
  min = scaler[1]
  max = scaler[2]
  t = length(scaled)
  mins = feature_range[1]
  maxs = feature_range[2]
  inverted_dfs = numeric(t)
  
  for( i in 1:t){
    X = (scaled[i]- mins)/(maxs - mins)
    rawValues = X *(max - min) + min
    inverted_dfs[i] <- rawValues
  }
  return(inverted_dfs)
}
dim(x_train) <- c(length(x_train), 1, 1)

X_shape2 = dim(x_train)[2]
X_shape3 = dim(x_train)[3]
batch_size = 1                # must be a common factor of both the train and test samples
units = 1     
model <- keras_model_sequential()

model%>%
  layer_lstm(units, batch_input_shape = c(batch_size, X_shape2, X_shape3), stateful= TRUE)%>%
  layer_dense(units = 1)

model %>% compile(
  loss = 'mean_squared_error',
  optimizer = optimizer_adam(),  
  metrics = c('accuracy')
)
summary(model)

Epochs = 50   
for(i in 1:Epochs ){
  model %>% fit(x_train, y_train, epochs=1, batch_size=batch_size, verbose=1, shuffle=FALSE)
  model %>% reset_states()
}

L = length(x_test)
scaler = Scaled$scaler
predictions = numeric(L)

for(i in 1:L){
  X = x_test[i]
  dim(X) = c(1,1,1)
  yhat = model %>% predict(X, batch_size=batch_size)
  # invert scaling
  yhat = invert_scaling(yhat, scaler,  c(-1, 1))
  # invert differencing
  yhat  = yhat + coal[(n+i)]
  # store
  predictions[i] <- yhat
}

combin <- cbind(as.numeric(coal),predictions)
colnames(combin) <-c("actual","predictions")
combin <- as.data.frame(combin)

rmse <- sqrt(mean((combin$actual - combin$predictions)^2))
mae <- mean(abs(combin$actual - combin$predictions))
r <- cor(combin$predictions,combin$actual)

#second method for LSTM
library(TSLSTM)
TSLSTM<-ts.lstm(ts=wti,tsLag=2,xregLag = 0,LSTMUnits=5, Epochs=50,CompLoss = "mse")
#*****************xgboost**********************####
wtiB <- data.frame(Date = index(wti), test$wti)
wtiBoost <- wtiB %>%
  mutate(Lag1 = lag(wti),
         Lag2 = lag(wti, 2))
wtiBoost <- na.omit(wtiBoost)

train_size <- 0.8
train_index <- 1:(train_size * nrow(wtiBoost))

train_data <- wtiBoost[train_index, ]
test_data <- wtiBoost[-train_index, ]



# Train the XGBoost Model
params <- list(
  objective = "reg:squarederror",  # Regression task
  eval_metric = "rmse"  # Root Mean Squared Error as the evaluation metric
)

label_column <- as.numeric(train_data$wti)
features <- as.matrix(train_data[, c("Lag1", "Lag2")])
dtrain <- xgb.DMatrix(data = features, label = label_column)

# Train the model
model <- xgboost(data = dtrain, params = params, nrounds = 100)

predictions <- predict(model, xgb.DMatrix(as.matrix(test_data[, c("Lag1", "Lag2")])))

# Evaluate the Model
rmse <- sqrt(mean((predictions - test_data$wti)^2))
cat("Root Mean Squared Error:", rmse, "\n")













#---------------------------------END---------------------------------------------


















 