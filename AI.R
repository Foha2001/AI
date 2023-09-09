#--------Title:  Predicting energy commodity prices amidst -------------
#-------worldwide energy transitions using deep learning models --

library(readxl)
library(xts)
library(writexl)
library(xgboost)
library(nnet)
library(stats)
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

#-------------NARX model-------------------
merged_df[is.na(merged_df)] <- 0
train_size <- floor(0.7 * nrow(merged_df))
train_data <- merged_df[1:train_size, ]
test_data <- merged_df[(train_size + 1):nrow(merged_df), ]
narx_model <- nnet(merged_df$wti ~ merged_df$TB + merged_df$LTY, data = train_data, size = 10, linout = TRUE)
predictions <- predict(narx_model, newdata = test_data)

















 