library(tidyverse)
# library(scorer)
library(Rcpp)
library(optimization)
library(lubridate)
library(randomForest)
library(car)
library(RcppRoll)
# install.packages("optimization")
# install.packages('Rcpp')
# install.packages('car')
# install.packages("randomForest")


source("1/utils.R")

C_CUR_YEAR <- "2021"
C_PREV_YEAR <- as.character(as.integer(C_CUR_YEAR) - 1)


source("1/prepare_data.r", encoding = "UTF-8")


start_date <- as.Date(paste0(C_CUR_YEAR, "-02-01"))

df_train_date <- df_train %>% filter(date >= start_date)

min_date <- min(df_train_date$date)

df_train_date$date_int <- as.integer(difftime(df_train_date$date, 
                                              min_date, 
                                              units = "days"))

df_train_date$week <- factor(df_train_date$date_int %% 7)

oktmo_set <- df_train_date %>% select(oktmo) %>% distinct()

df_prev <- df_train %>% 
  filter(between(date, 
                 as.Date(paste0(C_PREV_YEAR, "-02-01")) + 2,
                 as.Date(paste0(C_PREV_YEAR, "-12-31")))) %>% 
  mutate(date = date + 366 - 2)


df_prev_fit <- df_prev %>% 
  select(date, oktmo, all_of(predict_cols)) %>% 
  rename(setNames(predict_cols, paste0(predict_cols, ".prev"))) %>% 
  arrange(date)


df_train_date <- merge(df_train_date, df_prev_fit, by = c("oktmo", "date"))

start_predict <- start_date
finish_predict <- as.Date(paste0(C_CUR_YEAR, "-06-30"))

start_date_int <- as.integer(difftime(start_predict, min_date, units = "days"))
finish_date_int <- as.integer(difftime(finish_predict, min_date, units = "days"))
predict_dates_int = start_date_int:finish_date_int


cur_oktmo <- "98000000000"
# 
col_name <-  "mutton"

res_predict <- NULL

exc_cnt <- 0

except_oktmo <- c()
except_col <- c()
estimates <- c()

# prev year only: 0.007461143484185842

set.seed(42)
for (cur_oktmo in oktmo_set$oktmo){
  print(cur_oktmo)
  df_train_date_reg <- df_train_date %>% 
    filter(oktmo == cur_oktmo & date < as.Date(paste0(C_CUR_YEAR, "-04-01")))
  
  df_predict_reg <- tibble(oktmo = cur_oktmo,
                           date_int = predict_dates_int)
  
  df_lm <- df_train_date_reg
  
  
  df_prev_fit_reg <- df_prev_fit %>% 
    filter(oktmo == cur_oktmo) %>% 
    select(-oktmo) %>% 
    arrange(date)
  
  df_predict <- tibble(date_int = predict_dates_int, 
                       week = factor(predict_dates_int %% 7),
                       date = predict_dates_int + min_date)
  
  ## Для тестирования качества
  # df_predict <- df_predict %>%
  #   inner_join(df_train %>%
  #                filter(oktmo == cur_oktmo),
  #              by = c("date")) %>%
  #   rename(setNames(predict_cols, paste0(predict_cols, ".ans")))
  ## Для тестирования качества
  
  df_predict <- merge(df_predict, df_prev_fit_reg,
                      by = "date")
  
  df_predict <- df_predict %>% 
    arrange(date)
  
  
  for (col_name in predict_cols){
    ident <- paste0(cur_oktmo, ".", col_name)
    col_name_prev <- paste0(col_name, ".prev")
    
    df_lm_part <- df_lm %>% 
      arrange(date)
    
    
    col_prev_data_smooth <- roll_mean(df_prev_fit_reg[[col_name_prev]],
                                      n = 7)
    
    shift_prev_smooth <- which.max(sapply(1:25, function(x){
      cor.test(col_prev_data_smooth[x:(x + nrow(df_lm_part) - 1)],
               df_lm_part[[col_name]])$conf.int[1]}))
    
    if (length(shift_prev_smooth) == 0)
      shift_prev_smooth <- 1
    
    
    col_prev_data <- df_prev_fit_reg[[col_name_prev]]
    
    shift_prev <- which.max(sapply(1:25, function(x){
      cor.test(col_prev_data[x:(x + nrow(df_lm_part) - 1)],
               df_lm_part[[col_name]])$conf.int[1]}))
    
    if (length(shift_prev) == 0)
      shift_prev <- 1
    
    df_lm_part[["shifted"]] <- col_prev_data[shift_prev:(shift_prev + nrow(df_lm_part) - 1)]
    df_predict[["shifted"]] <- col_prev_data[shift_prev:(shift_prev + nrow(df_predict) - 1)]
    
    df_lm_part[["shifted_smooth"]] <- col_prev_data_smooth[shift_prev_smooth:(shift_prev_smooth + nrow(df_lm_part) - 1)]
    df_predict[["shifted_smooth"]] <- col_prev_data_smooth[shift_prev_smooth:(shift_prev_smooth + nrow(df_predict) - 1)]
    
    # ggplot() +
    #   geom_point(data = df_predict,
    #              aes(x = date, 
    #                  y = .data[[col_name2020]]), 
    #              col = "red") +
    #   geom_point(data = df_lm_part,
    #              aes(x = date, 
    #                  y = .data[[col_name]]), 
    #              col = "blue") +
    #   geom_point(data = df_2020_fit %>% 
    #                filter(oktmo == cur_oktmo),
    #              aes(x = date, 
    #                  y = .data[[col_name2020]]), col = "green")
    
    df_lm_part <- df_lm_part %>%
      arrange(df_lm[[col_name]])
    
    if (nrow(df_lm_part[8:(nrow(df_lm_part) - 8), ] %>%
             select(week) %>%
             distinct()) == 7){
      df_lm_part <- df_lm_part[8:(nrow(df_lm_part) - 8), ]
    }
    else
      df_lm_part <- df_lm_part[7:(nrow(df_lm_part) - 7), ]
    
    
    if (nrow(df_lm_part) > 0 & sum(df_lm_part[[col_name]]) != 0)
    {

      lm_formula <- paste(col_name,
                          " ~ date_int + week + ",
                          col_name_prev)
      
      
      fit.glm <- glm(as.formula(lm_formula),
                     data = df_lm_part)
      
      sf <-  summary(fit.glm)
      
      if (last(sf$coefficients[, 1]) < 0)
      {
        lm_formula <- paste(col_name,
                            " ~ date_int + week")
        
        fit.glm <- glm(as.formula(lm_formula),
                       data = df_lm_part)
      }
      
      
      df_predict <- df_predict %>% mutate(!!col_name := predict(fit.glm, df_predict))
      
      
      if (min(df_lm_part[[col_name]]) > 0)
      {
        rate <- mean(df_predict[[col_name]][between(df_predict$date,
                                                    as.Date("2021-06-20"),
                                                    as.Date("2021-06-30"))]) /
          mean(df_predict[[col_name]][between(df_predict$date,
                                              as.Date("2021-04-01"),
                                              as.Date("2021-04-10"))])
        if (rate < 1/1.8 | rate > 1.8)
        {
          exc_cnt <- exc_cnt + 1
          print(paste("except",
                      cur_oktmo,
                      col_name))
          except_oktmo[exc_cnt] <- cur_oktmo
          except_col[exc_cnt] <- col_name
          
          if (col_name != "сucumbers_tomatoes")
          {
            fit.exc <- randomForest(as.formula(paste(col_name,
                                                     " ~ date_int")),
                                    data = df_lm_part, ntree=50)
            
            
            df_predict <- df_predict %>%
              mutate(!!col_name := predict(fit.exc,
                                           df_predict))
            
          }
          else
          {
            fit.exc <- glm(as.formula(paste(col_name,
                                            " ~ shifted")),
                           data = df_lm_part)
            
            df_predict <- df_predict %>%
              mutate(!!col_name := predict(fit.exc,
                                           df_predict))

          }
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
    
    # Оценка качества
    # df_1 <- df_train %>%
    #   filter(between(date,
    #                  as.Date("2020-04-01"),
    #                  as.Date("2020-06-30")))
    # 
    # err <- mean(abs(df_predict[[paste0(col_name, ".ans")]] -  df_predict[[col_name]])) /
    #   mean(abs(df_predict[[paste0(col_name, ".ans")]]))
    # 
    # estimates[paste0(cur_oktmo,
    #                  ".",
    #                  col_name)] <- 1 / (1000 * err)
    # Оценка качества
    
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

df_test$date_format <- convert_input_date(df_test$date)
df_test$date <- convert_date_to_inp(df_test$date_format)

df_res <- merge(df_test %>% 
                  select(region, okato, oktmo, date, date_format), 
                res_predict %>% 
                  filter(date >= as.Date(paste0(C_CUR_YEAR, "-04-01"))),
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
  geom_point(aes(x = date, 
                 y = .data[[col_name]]), 
             col = "blue") +
  geom_line(aes(x = date, 
                y = .data[[col_name]]), 
            col = "blue") +
  geom_smooth(aes(x = date, 
                  y = .data[[col_name]]), 
              method = "lm") +
  geom_point(data = res_predict %>% 
               filter(oktmo == cur_oktmo), 
             aes(x = date, 
                 y = .data[[col_name]]), 
             col = "red") +
  geom_point(data = df_prev_fit %>% 
               filter(oktmo == cur_oktmo),
             aes(x = date, 
                 y = .data[[paste0(col_name,
                                   ".prev")]]), col = "green")



for (ind in 1:exc_cnt){
  col_name <- except_col[ind]
  cur_oktmo <- except_oktmo[ind]
  show(ggplot(data = df_train_date %>% 
                filter(oktmo == cur_oktmo)) +
         geom_point(aes(x = date, 
                        y = .data[[col_name]]), 
                    col = "blue") +
         geom_line(aes(x = date, 
                       y = .data[[col_name]]), 
                   col = "blue") +
         geom_smooth(aes(x = date, 
                         y = .data[[col_name]]), 
                     method = "lm") +
         geom_point(data = res_predict %>% 
                      filter(oktmo == cur_oktmo), 
                    aes(x = date, 
                        y = .data[[col_name]]), 
                    col = "red") +
         geom_point(data = df_prev_fit %>% 
                      filter(oktmo == cur_oktmo),
                    aes(x = date, 
                        y = .data[[paste0(col_name,
                                          ".prev")]]), col = "green") +
         ggtitle(cur_oktmo))
  
}


col_name <- "mutton"

for (cur_oktmo in oktmo_set$oktmo){
  show(ggplot(data = df_train_date %>% 
                filter(oktmo == cur_oktmo)) +
         geom_point(aes(x = date, 
                        y = .data[[col_name]]), 
                    col = "blue") +
         geom_line(aes(x = date, 
                       y = .data[[col_name]]), 
                   col = "blue") +
         geom_smooth(aes(x = date, 
                         y = .data[[col_name]]), 
                     method = "lm") +
         geom_point(data = res_predict %>% 
                      filter(oktmo == cur_oktmo), 
                    aes(x = date, 
                        y = .data[[col_name]]), 
                    col = "red") +
         geom_point(data = df_prev_fit %>% 
                      filter(oktmo == cur_oktmo),
                    aes(x = date, 
                        y = .data[[paste0(col_name,
                                          ".prev")]]), col = "green") +
         ggtitle(cur_oktmo))
  
}


cur_oktmo <- "71000000000"
for (col_name in predict_cols){
  show(ggplot(data = df_train_date %>% 
                filter(oktmo == cur_oktmo)) +
         geom_point(aes(x = date, 
                        y = .data[[col_name]]), 
                    col = "blue") +
         geom_line(aes(x = date, 
                       y = .data[[col_name]]), 
                   col = "blue") +
         geom_smooth(aes(x = date, 
                         y = .data[[col_name]]), 
                     method = "lm") +
         geom_point(data = df_predict %>% 
                      filter(oktmo == cur_oktmo), 
                    aes(x = date, 
                        y = .data[[col_name]]), 
                    col = "red") +
         geom_point(data = df_prev_fit %>% 
                      filter(oktmo == cur_oktmo),
                    aes(x = date, 
                        y = .data[[paste0(col_name,
                                          ".prev")]]), col = "green") +
         ggtitle(cur_oktmo))
  
}

