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
train_size <- floor(0.7 * nrow(subset_df))
train_data <- subset_df[1:train_size, -1]
test_data <- subset_df[(train_size + 1):nrow(subset_df),-1]
narx_model <- nnet(wti ~ TB+LTY+I+SRV+GOP+GIPIO+M2+EPI+IPI+UMP+EEPH+TEMP+
                     co2_per_capita+solarpv+solarthermal+solarpvthermalhybrid+
                     wind+hydropower+marineandtidal+bioenergy+geothermal+
                     bioenergyLCOE+geothermalLCOE+offshorewindLCOE+
                     solarphotovoltaicLCOE+concentratedsolarLCOE+hydropowerLCOE+
                     onshorewindLCOE+AC_WIND_E+AC_SOLAR_E+AC_HYD_E+GR
                   , data = train_data, size = 20,
                   na.action = na.omit,linout = TRUE)
predictions <- predict(narx_model, newdata = test_data)

















 