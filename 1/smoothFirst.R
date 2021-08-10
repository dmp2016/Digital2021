library(tidyverse)
library(Rcpp)
library(optimization)
library(lubridate)
library(randomForest)
library(car)
library(xgboost)
library(RcppRoll)
# install.packages("xgboost")
# install.packages("optimization")
# install.packages('Rcpp')
# install.packages('car')
# install.packages("randomForest")
# install.packages("RcppRoll")


source("1/prepare_data.r", encoding = "UTF-8")


start_date <- as.Date("2021-02-01")

df_train_date <- df_train %>% filter(date >= start_date)

min_date <- min(df_train_date$date)
df_train_date$date_int <- as.integer(difftime(df_train_date$date, 
                                              min_date, 
                                              units = "days"))

oktmo_set <- df_train_date %>% select(oktmo) %>% distinct()


delta_shift <- 14

df_2020 <- df_train %>% 
  mutate(date = date + 366) %>% 
  filter(between(date, 
                 start_date - delta_shift, 
                 as.Date("2021-12-31"))) %>% 
  rename(setNames(predict_cols, paste0(predict_cols, ".2020"))) %>% 
  arrange(date)


df_train_date <- merge(df_train_date, df_2020_fit, by = c("oktmo", "date"))


start_predict <- start_date
finish_predict <- as.Date("2021-06-30")

start_date_int <- as.integer(difftime(start_predict, min_date, units = "days"))
finish_date_int <- as.integer(difftime(finish_predict, min_date, units = "days"))
predict_dates_int = start_date_int:finish_date_int


cur_oktmo <- "30000000000"
# 
col_name <-  "сucumbers_tomatoes"

res_predict <- NULL

exc_cnt <- 0

# prev year only: 0.007461143484185842


for (cur_oktmo in oktmo_set$oktmo){
  print(cur_oktmo)
  df_train_date_reg <- df_train_date %>% 
    filter(oktmo == cur_oktmo) %>% 
    arrange(date_int)
  
  
  df_predicted_reg <- tibble(oktmo = cur_oktmo,
                           date_int = predict_dates_int)
  
  df_lm <- df_train_date_reg
  
  df_2020_fit_reg <- df_2020 %>% 
    filter(oktmo == cur_oktmo) %>% 
    select(-oktmo) %>% 
    arrange(date)
  
  for (col_name in predict_cols){
    col_name2020 <- paste0(col_name, ".2020")
    

    if (sum(df_lm[[col_name]]) != 0)
    {
      
      n_mean <- 15
      n_mean_side <- n_mean %/% 2
      df_lm_part <- df_lm[(n_mean_side + 1):(nrow(df_lm) - n_mean_side), 
                          c("oktmo", "date", "date_int", col_name)]
      
      smooth_2021 <- roll_mean(df_lm[[col_name]], 
                               n = n_mean)
      dev_2021 <- df_lm_part[[col_name]] - smooth_2021
      
      smooth_2020 <- roll_mean(df_2020_fit_reg[[col_name2020]], 
                               n = n_mean)
      dev_2020 <- df_2020_fit_reg[[col_name2020]][(n_mean_side + 1):(nrow(df_2020_fit_reg) - 
                                                                       n_mean_side)] - smooth_2020
      
      shift_opt <- which.min(sapply(1:(delta_shift + 1), function(x){
        sum(abs(dev_2021 - dev_2020[x:(x + length(dev_2021) - 1)])) }))
      
      # x <- shift_opt
      # sum(abs(dev_2021 - dev_2020[x:(x + length(dev_2021) - 1)]))
      # sum(abs(dev_2021 - dev_2020[shift_opt:(shift_opt + length(dev_2021) - 1)]))

      df_col_2020 <- tibble(smooth_2020 = smooth_2020[shift_opt:length(smooth_2020)],
                            dev_2020 = dev_2020[shift_opt:length(dev_2020)])
      
      
      # sum(abs(dev_2021 - df_col_2020$dev_2020[1:(1 + length(dev_2021) - 1)]))
      df_col_2020$date <- seq.Date(start_date + n_mean_side, 
                                   start_date + n_mean_side + nrow(df_col_2020) - 1, 
                                   by = 1)
      
      df_lm_part$smooth_2021 <- smooth_2021
      df_lm_part$dev_2021 <- dev_2021
      df_lm_part <- merge(df_lm_part, 
                          df_col_2020, 
                          by = "date")
      # df_lm_part$smooth_2020 <- smooth_2020[1:nrow(df_lm_part)]
      # df_lm_part$dev_2020 <- dev_2020[1:nrow(df_lm_part)]

      # sum(abs(df_lm_part$dev_2021 - df_lm_part$dev_2020))
      # 
      # ggplot(data = df_lm_part) +
      #   geom_point(aes(x = date, y = smooth_2021), col = "blue") +
      #   geom_point(aes(x = date, y = smooth_2020), col = "green") +
      #   scale_x_date(breaks = "day") +
      #   theme(axis.text.x = element_text(angle = 45, hjust = 1))
      # 
      # ggplot(data = df_lm_part) +
      #   geom_path(aes(x = date, y = dev_2021), col = "blue") +
      #   geom_path(aes(x = date, y = dev_2020), col = "green") +
      #   scale_x_date(breaks = "day") +
      #   theme(axis.text.x = element_text(angle = 45, hjust = 1))
      # 
      # 
      # 
      # ggplot(data = df_lm_part) +
      #   geom_point(aes(x = date, y = smooth_2021), col = "blue") +
      #   geom_point(aes(x = date, y = .data[[col_name]]), col = "red") +
      #   geom_smooth(aes(x = date, y = smooth_2021), col = "blue", method = "lm") +
      #   geom_smooth(aes(x = date, y = .data[[col_name]]), col = "red", method = "lm")
      # 
      # 
      # geom_point(aes(x = date, y = smooth_2020), col = "green") +
      #   scale_x_date(breaks = "day") +
      #   theme(axis.text.x = element_text(angle = 45, hjust = 1))
      # 
      

      fit_smooth <- glm(smooth_2021 ~ date_int, data = df_lm_part)
      # summary(fit_smooth)
      
      df_predict <- tibble(date_int = predict_dates_int, 
                           date = predict_dates_int + min_date)
      df_predict <- merge(df_predict, 
                          df_col_2020, 
                          by = "date", 
                          sort = F)
      
      df_predict$smooth_predict <- predict(fit_smooth, df_predict)
      
      ggplot() +
        geom_point(aes(x = df_predict$date, y = df_predict$smooth_predict), col = "blue") +
        geom_point(aes(x = df_lm_part$date, y = df_lm_part[[col_name]]), col = "red") +
        geom_point(aes(x = df_lm_part$date, y = df_lm_part$smooth_2021), col = "red") +
        geom_point(aes(x = df_predict$date, y = df_predict$smooth_2020), col = "green")


      # ggplot() +
      #   geom_point(aes(x = df_predict$date, y = df_predict$col_predict), col = "blue") +
      #   geom_point(aes(x = df_lm_part$date, y = df_lm_part[[col_name]]), col = "red") +
      #   geom_point(aes(x = df_lm_part$date, y = df_lm_part$smooth_2021), col = "red") +
      #   geom_point(aes(x = df_predict$date, y = df_predict$smooth_2020), col = "green")
      
      
      # df_predict <- df_predict %>% mutate(smooth_predict := predict(fit, df_predict))
      
      if (min(df_lm_part[[col_name]]) > 0){
        rate <- mean(df_predict$smooth_predict[between(df_predict$date, 
                                                       as.Date("2021-06-20"), 
                                                       as.Date("2021-06-30"))]) /
          mean(df_predict$smooth_predict[between(df_predict$date, 
                                                 as.Date("2021-04-01"), 
                                                 as.Date("2021-04-10"))])
        if (rate < 1/2 | rate > 2){
          exc_cnt <- exc_cnt + 1
          print(paste("except", 
                      cur_oktmo, 
                      col_name))
          
          # fit_smooth <- glm(smooth_2021 ~ smooth_2020, data = df_lm_part)
          # summary(fit_smooth)
          fit_smooth <- randomForest(smooth_2021 ~ smooth_2020,
                              data = df_lm_part, 
                              ntree=50)
          
          df_predict$smooth_predict <- predict(fit_smooth, df_predict)
        }
        
      }

      
      fit_dev <- randomForest(dev_2021 ~ dev_2020, 
                              data = df_lm_part,
                              ntree=50)
      df_predict$dev_predict <- predict(fit_dev, df_predict)
      
      df_predict$col_predict <- df_predict$smooth_predict + df_predict$dev_predict
              
      df_predict$col_predict <- ifelse(df_predict$col_predict < 0, 
                                       0, 
                                       df_predict$col_predict)
    }
    else
    {
      print(0)
      df_predict$col_predict <- 0
    }
    df_predicted_reg <- merge(df_predicted_reg, 
                              df_predict %>% 
                                mutate(!!col_name := col_predict) %>% 
                                select(date_int, !!col_name),
                              by = "date_int")
    
    # df_predicted_reg <- df_predicted_reg %>% 
    #   mutate(!!col_name := df_predict$col_predict)
    # sum(df_predicted_reg$date_int != df_predict$date_int)
  }
  if (is.null(res_predict))
    res_predict <- df_predicted_reg
  else
    res_predict <- rbind(
      res_predict,
      df_predicted_reg)
}

print(exc_cnt)
res_predict$date <- res_predict$date_int + min_date

df_test <- read_csv("1/test.csv", 
                    col_types = cols(.default = col_character()))

df_test$date_format <- 
  as.Date(paste(substr(df_test$date, 7, 10), 
                "-", 
                substr(df_test$date, 4, 5),
                "-",
                substr(df_test$date, 1, 2),
                sep = ""))

df_res <- merge(df_test %>% 
                  select(region, okato, oktmo, date, date_format), 
                res_predict %>% 
                  filter(date >= as.Date("2021-04-01")),
                by.x = c("oktmo", "date_format"),
                by.y = c("oktmo", "date"),
                sort = F) %>% 
  select(all_of(colnames(df_test))) %>% 
  select(-date_format)

write_csv(df_res, "1/mytest_ord.csv")

write_csv(res_predict, "1/all_predict.csv")


# Проверка корректности порядка строк и столбцов в результирующем наборе
sum(df_res$date != df_test$date |
      df_res$oktmo != df_test$oktmo)


sum(colnames(df_test %>% select(-date_format))!= colnames(df_res))



######################################################
# Test prediction on graphic
######################################################
cur_oktmo <- "71000000000"
cur_oktmo <- "26000000000"
cur_oktmo <- "64000000000"
cur_oktmo <- "75000000000"
cur_oktmo <- "33000000000"
col_name <- "herring_value"
col_name <- "rice"
col_name <- "сucumbers_tomatoes"
col_name <- "bread_value"
ggplot(data = df_train_date %>% 
         filter(oktmo == cur_oktmo)) +
  geom_point(aes(x = date_int, y = .data[[col_name]]), col = "blue") +
  geom_line(aes(x = date_int, y = .data[[col_name]]), col = "blue") +
  geom_smooth(aes(x = date_int, y = .data[[col_name]]), method = "lm") +
  geom_point(data = res_predict %>% 
               filter(oktmo == cur_oktmo), 
             aes(x = date_int, y = .data[[col_name]]), col = "red")


