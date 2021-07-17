library(tidyverse)


df_train <- read_csv2("1/train.csv", col_types = cols(
  region = col_character(),
  oktmo = col_character(),
  okato = col_character(),
  date = col_character()))

df_train$date <- as.Date(paste(substr(df_train$date, 7, 10), 
                         "-", 
                         substr(df_train$date, 4, 5),
                         "-",
                         substr(df_train$date, 1, 2),
                         sep = ""))

df_train_reg <- na.omit(df_train %>% filter(region == "54"))


min(df_train_reg$date)
max(df_train_reg$date)

ggplot(data = df_train_reg %>% arrange(date)) +
  geom_point(aes(x = date, y = bread))


