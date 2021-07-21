library(tidyverse)
library(scorer)


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


start_date <- as.Date("2021-01-15")


df_train %>% filter(date >= start_date)

min_date <- min(df_train_reg$date)
df_train$date_int <- as.integer(difftime(df_train$date, 
                                         min_date, 
                                         units = "days"))


for (col in predict_cols){
  df_train_col
}


df_train_reg <- na.omit(df_train %>% filter(region == "10"))

df_train_reg_part <- df_train_reg %>% select(date, bread, bread_value)


min_date <- min(df_train_reg$date)

max(df_train_reg$date)


start_date <- as.Date("2021-01-15")




ggplot(data = df_train_reg %>% 
         filter(date >= '2021-01-15') %>% 
         arrange(date), aes(x = date, y = bread_value)) +
  geom_point() +
  geom_line() +
  geom_smooth(method = "lm")
  

ggplot(data = df_train_reg %>% 
         filter(date >= '2021-02-01') %>% 
         arrange(date), aes(x = date, y = ai95_value)) +
  geom_point() +
  geom_line() +
  geom_smooth(method = "loess")



df_test <- read_csv("1/test.csv", col_types = cols(.default = col_character()))
df_test %>% group_by(oktmo) %>% 
  summarise(cnt = n()) %>% 
  filter(cnt > 91)
