#--------Title:  Predicting energy commodity prices amidst -------------
#-------worldwide energy transitions using deep learning models --
#import data
library(readxl)
library(xts)

excel_file <- "C:/Users/Foued Azuz 14/OneDrive/AI energy/dataset.xlsx"

# Get the sheet names
sheet_names <- excel_sheets(excel_file)
# Read each sheet into a list of dataframes
all_dataframes <- lapply(sheet_names, function(sheet) {
    read_excel(excel_file, sheet = sheet)
  })

all_dataframes <- lapply(all_dataframes, as.data.frame)
  
for (i in 1:22) {
    
    all_dataframes[[i]][["date"]] <- as.Date(all_dataframes[[i]][["date"]])

}


merged_df <- all_dataframes[[1]]
# Loop through the remaining dataframes in the list and merge them
  for (i in 2:length(all_dataframes)) {
    df <- all_dataframes[[i]]
      merged_df <- merge(merged_df, df, by = 'date', all = TRUE)
  }
  
merged_df$date <- as.Date(merged_df$date)
merged_df <- merged_df[!is.na(merged_df$date), ]
xts_obj <- xts(merged_df[, -1], order.by = merged_df$date)

 




















 
  
  -----------------------------------------------------------------------------
  # lst <- lapply(1:16, function(i) read_excel("C:/Users/Foued Azuz 14/OneDrive/AI energy/dataset.xlsx", sheet = i))
  # for (i in 1:16) {
  #   
  #   lst[[i]][["date"]] <- as.Date(lst[[i]][["date"]])
  # 
  # }
  
# 
# library(xts)
# library(zoo)
# wti <- as.data.frame(lst[[1]])
# wti <- xts(wti[, -1], order.by = wti$date)
# colnames(wti) <- "wti"
# 
# names <- c("GP","coal","TB","LTY","I","SRV","GOP","GIPIO","M2","EPI","IPI",
#            "UMP","EEPH","TEMP","co2_per_capita")
# common_dates <- Reduce(intersect, lapply(lst, function(df) df$date))
#   

