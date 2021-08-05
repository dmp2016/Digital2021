library(tidyverse)

C_HOLIDAY <- as.Date(c(
  "2021-01-01",
  "2021-01-02", 
  "2021-01-03", 
  "2021-01-04", 
  "2021-01-05", 
  "2021-01-06",
  "2021-01-07", 
  "2021-01-08", 
  "2021-01-09", 
  "2021-01-10", 
  "2021-02-22", 
  "2021-02-23",
  "2021-03-08",
  "2021-05-03",
  "2021-05-10",
  "2021-06-14"))

C_NOT_HOLIDAY <- as.Date(c("2021-02-20"))


is_holiday <- function(x){
  if (x %in% C_HOLIDAY)
    return(T)
  if (x %in% C_NOT_HOLIDAY)
    return(F)
  if (wday(x) %in% c(1, 7))
    return(T)
  return(F)
}

##############################################################
# Чтение и подготовка данных
##############################################################

df_train <- read_csv2("1/train.csv", 
                      col_types = cols(.default = col_character()))


numeric_cols <- colnames(df_train)[5:length(colnames(df_train))]
predict_cols <- colnames(df_train)[5:length(colnames(df_train))]

for (cl in numeric_cols){
  print(cl)
  df_train[[cl]] <- gsub(",", ".", df_train[[cl]], fixed = T)
  df_train[[cl]] <- gsub(" ", "", df_train[[cl]], fixed = T)
  df_train[[cl]] <- as.double(df_train[[cl]])
}



typeof(df_train$bread)

df_train$date <- as.Date(paste(substr(df_train$date, 7, 10), 
                               "-", 
                               substr(df_train$date, 4, 5),
                               "-",
                               substr(df_train$date, 1, 2),
                               sep = ""))

##############################################################
# Исходный датасет прочитан и подготовлен..
##############################################################

