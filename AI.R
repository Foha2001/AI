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

train_size <- floor(0.7 * nrow(subset_df_filled_xts))
train_data <- subset_df_filled_xts[1:train_size,]
test_data <- subset_df_filled_xts[(train_size + 1):nrow(subset_df_filled_xts),]
ind_test = index(test_data)
wti <- subset_df_filled_xts$wti
gas <- subset_df_filled_xts$GP
coal<- subset_df_filled_xts$coal
macro_fact<- subset_df_filled_xts[,c(11,1,2,3,4,5,6,7,9,10)]
with_env_fac <- subset_df_filled_xts[,c(11,1,2,3,4,5,6,7,9,10,17,18,19,20,21,22,23,24,25,26,27,28,29,30,
                                    31,32,33,34,15,16)]


#predict wti without tech-envt variables*****####

model_wti = narx(train_data$wti, SimpleLM, p = 2,xreg = macro_fact)
pred_wti = forecast(model_wti,xreg=macro_fact[ind_test])

#predict wti with tech-envt variables*****####
model_wti_env = narx(train_data$wti, SimpleLM, p = 2,xreg = with_env_fac)
pred_wti_env = forecast(model_wti_env,xreg=with_env_fac[ind_test])




rmse <- function(x,y) sqrt(mean((x-y)^2))

c(Err_without_xreg= rmse(pred1$mean, test_data),
  Err_with_xreg= rmse(pred2$mean, x_test),
  Err_with_bad_xreg= rmse(pred3$mean, x_test))

























 