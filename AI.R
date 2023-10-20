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
  
merged_df$date <- as.Date(merged_df$date)
merged_df <- merged_df[!is.na(merged_df$date), ]
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



#-------------NARX model-------------------

train_size <- floor(0.8 * nrow(subset_df_filled_xts))
train_data <- subset_df_filled_xts[1:train_size,]
test_data <- subset_df_filled_xts[(train_size + 1):nrow(subset_df_filled_xts),]
ind_test = index(test_data)
wti <- subset_df_filled_xts$wti
gas <- subset_df_filled_xts$GP
coal<- subset_df_filled_xts$coal
macro_fact<- subset_df_filled_xts[,c(4,5,6,7,8,9,10,12,13,14)]
with_RE_fac <- subset_df_filled_xts[,c(25,26,27,28,29,30,31,32,33,34)]
with_ENV_fac <- subset_df_filled_xts[,c(15,16)]
with_tech_fac <- subset_df_filled_xts[,c(17,18,19,20,21,22,23,24)]
with_pol_fac <- subset_df_filled_xts[,c(11,35)]

#predict wti without  variables*****####
model_wti = narx(train_data$wti, SimpleLM, p = 2)
pred_wti = forecast(model_wti,h=1126)


model_wti_macr = narx(train_data$wti, SimpleLM, p = 2,xreg=macro_fact)
pred_wti_macr = forecast(model_wti_macr,xreg=macro_fact[ind_test])

#predict wti with RE factors *****####
model_wti_RE = narx(train_data$wti, SimpleLM, p = 2,xreg = with_RE_fac)
pred_wti_RE = forecast(model_wti_RE,xreg=with_RE_fac[ind_test])

#predict wti with ENV factors *****####
model_wti_ENV = narx(train_data$wti, SimpleLM, p = 2,xreg = with_ENV_fac)
pred_wti_ENV = forecast(model_wti_ENV,xreg=with_ENV_fac[ind_test])

#predict wti with TECH factors *****####
model_wti_TECH = narx(train_data$wti, SimpleLM, p = 2,xreg = with_tech_fac)
pred_wti_TECH = forecast(model_wti_TECH,xreg=with_tech_fac[ind_test])


#predict wti with TECH factors *****####
model_wti_pol = narx(train_data$wti, SimpleLM, p = 2,xreg = with_pol_fac)
pred_wti_pol = forecast(model_wti_pol,xreg=with_pol_fac[ind_test])


rmse <- function(x,y) sqrt(mean((x-y)^2))
# sigmoid_value <- function (x) 1 / (1 + exp(-x))

c(Err_without_xreg= rmse(pred_wti$mean, test_data$wti),
  Err_with_xreg_macr= rmse(pred_wti_macr$mean, test_data$wti),
  Err_with_xreg_RE= rmse(pred_wti_RE$mean, test_data$wti),
  Err_with_xreg_ENV= rmse(pred_wti_ENV$mean, test_data$wti),
  Err_with_xreg_TECH= rmse(pred_wti_TECH$mean, test_data$wti),
  Err_with_xreg_POL= rmse(pred_wti_pol$mean, test_data$wti))

























 