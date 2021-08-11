

# install.packages("tseries")
# install.packages("forecast")
library(tseries)
library(forecast)


start_date <- as.Date("2021-02-01")

df_train_date <- df_train %>% filter(date >= start_date)

min_date <- min(df_train_date$date)

df_train_date$date_int <- as.integer(difftime(df_train_date$date, 
                                              min_date, 
                                              units = "days"))

df_train_date$week <- factor(df_train_date$date_int %% 7)
df_train_date$is_holiday <- factor(sapply(df_train_date$date, is_holiday))

days_amount <- difftime("2021-06-30", "2021-02-01", units = "days")

df_2020 <- df_train %>% 
  filter(between(date, 
                 as.Date("2020-02-01") + 2, 
                 as.Date("2020-02-01") + 2 + days_amount)) %>% 
  mutate(date = date + 366 - 2, year = 2020)


df_2020_fit <- df_2020 %>% 
  select(date, oktmo, all_of(predict_cols)) %>% 
  rename(setNames(predict_cols, paste0(predict_cols, ".2020"))) %>% 
  mutate(date_int_1 = as.integer(difftime(date, 
                                          min_date, 
                                          units = "days"))) %>% 
  arrange(date_int_1)


oktmo_set <- df_train_date %>% select(oktmo) %>% distinct()

df_2020_fit$date_int_1 <- NULL


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

data <- ts(df_lm_part[[col_name]], frequency = 7)
plot(decompose(data))
L <- BoxCox.lambda(data, method="loglik")

fit.arima <- auto.arima(data, lambda=L)

fcast.arima <- forecast(fit.arima, 1, lambda=L)


ggplot(data = df_train_date %>% 
         filter(oktmo == cur_oktmo)) +
  geom_point(aes(x = date_int, y = .data[[col_name]]), col = "blue") +
  geom_line(aes(x = date_int, y = .data[[col_name]]), col = "blue") +
  geom_smooth(aes(x = date_int, y = .data[[col_name]]), method = "lm") +
  geom_point(data = res_predict %>% 
               filter(oktmo == cur_oktmo), 
             aes(x = date_int, y = .data[[col_name]]), col = "red")






for (cur_oktmo in oktmo_set$oktmo){
  print(cur_oktmo)
  df_train_date_reg <- df_train_date %>% filter(oktmo == cur_oktmo)
  
  df_predict_reg <- tibble(oktmo = cur_oktmo,
                           date_int = predict_dates_int)
  
  # df_lm <- df_train_date_reg %>% filter(.[[col_name]] > 0)
  
  df_lm <- df_train_date_reg
  
  df_2020_fit_reg <- df_2020_fit %>% 
    filter(oktmo == cur_oktmo) %>% 
    select(-oktmo)
  
  df_predict <- tibble(date_int = predict_dates_int, 
                       week = factor(predict_dates_int %% 7),
                       date = predict_dates_int + min_date,
                       isw1 = as.integer(week == 1),
                       isw2 = as.integer(week == 2),
                       isw3 = as.integer(week == 3),
                       isw4 = as.integer(week == 4),
                       isw5 = as.integer(week == 5),
                       isw6 = as.integer(week == 6))
  df_predict <- merge(df_predict, df_2020_fit_reg,
                      by = "date")
  
  
  for (col_name in predict_cols){
    col_name2020 <- paste0(col_name, ".2020")
    if (length(grep("_value", col_name)) > 0)
      add_col_name <- substr(col_name, 1, length(col_name) - 6)
    else{
      add_col_name <- paste0(col_name, "_value")
      if (!(add_col_name %in% predict_cols))
        add_col_name <- ""
    }
    
    if (add_col_name != "")
      add_col_name <- paste0(add_col_name, ".2020")
    
    df_lm_part <- df_lm %>% 
      arrange(df_lm[[col_name]])
    
    for (n_cut in 10:7){
      if (nrow(df_lm_part[n_cut:(nrow(df_lm_part) - n_cut), ] %>% 
               select(week) %>% 
               distinct()) == 7){
        df_lm_part <- df_lm_part[n_cut:(nrow(df_lm_part) - n_cut), ]
        break
      }
    }
    
    if (nrow(df_lm_part) > 0 & sum(df_lm_part[[col_name]]) != 0)
    {
      
      # if (add_col_name != "")
      #   lm_formula <- paste(col_name, 
      #                       " ~ date_int + week + ", 
      #                       col_name2020,
      #                       " + ",
      #                       add_col_name)
      # else
      #   lm_formula <- paste(col_name, 
      #                       " ~ date_int + week + ", 
      #                       col_name2020)
      
      lm_formula <- paste(col_name,
                          " ~ date_int + week + ",
                          col_name2020)
      
      
      fit <- glm(as.formula(lm_formula),
                 data = df_lm_part)
      
      # df_lm_part[[col_name]][spl]
      # qplot(x = df_lm_part[-spl, "date"], y = df_lm_part[-spl, col_name])
      
      df_predict <- df_predict %>% mutate(!!col_name := predict(fit, df_predict))
      
      # qplot(x = df_predict$date, y = df_predict[[col_name]])
      
      if (min(df_lm_part[[col_name]]) > 0){
        rate <- mean(df_predict[[col_name]][between(df_predict$date, 
                                                    as.Date("2021-06-20"), 
                                                    as.Date("2021-06-30"))]) /
          mean(df_predict[[col_name]][between(df_predict$date, 
                                              as.Date("2021-04-01"), 
                                              as.Date("2021-04-10"))])
        if (rate < 1/2 | rate > 2){
          exc_cnt <- exc_cnt + 1
          print(paste("except", 
                      cur_oktmo, 
                      col_name))
          fit <- randomForest(as.formula(paste(col_name,
                                               " ~ ",
                                               col_name2020)),
                              data = df_lm_part, ntree=50)
          # if (add_col_name != "")
          #   lm_formula_new <- paste(col_name, 
          #                           " ~ ", 
          #                           col_name2020,
          #                           " + ",
          #                           add_col_name)
          # else
          #   lm_formula_new <- paste(col_name, 
          #                           " ~ ", 
          #                           col_name2020)
          
          # fit <- lm(as.formula(lm_formula_new),
          #           data = df_lm_part)
          
          # summary(fit)
          df_predict <- df_predict %>% 
            mutate(!!col_name := predict(fit, df_predict))
        }
        
      }
      df_predict[[col_name]] <- ifelse(df_predict[[col_name]] < 0, 
                                       0, 
                                       df_predict[[col_name]])
    }
    else
    {
      print(0)
      df_predict <- df_predict %>% 
        mutate(!!col_name := 0)
    }
    df_predict_reg <- cbind(df_predict_reg, 
                            df_predict %>% 
                              select(!!col_name))
  }
  if (is.null(res_predict))
    res_predict <- df_predict_reg
  else
    res_predict <- rbind(
      res_predict,
      df_predict_reg)
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
# sum(df_tmp$date != df_test$date | 
#       df_tmp$oktmo != df_test$oktmo)
# 
# 
# sum(colnames(df_tmp) != colnames(df_test))
# sum(colnames(df_test %>% select(-date_format))!= colnames(df_res))



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
ggplot(data = df_train_date %>% 
         filter(oktmo == cur_oktmo)) +
  geom_point(aes(x = date_int, y = .data[[col_name]]), col = "blue") +
  geom_line(aes(x = date_int, y = .data[[col_name]]), col = "blue") +
  geom_smooth(aes(x = date_int, y = .data[[col_name]]), method = "lm") +
  geom_point(data = res_predict %>% 
               filter(oktmo == cur_oktmo), 
             aes(x = date_int, y = .data[[col_name]]), col = "red")


plot(decompose(ts(df_lm_part[[col_name]], frequency = 7)))



