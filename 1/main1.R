library(tidyverse)
# library(scorer)
library(Rcpp)
library(optimization)
library(lubridate)
library(randomForest)
library(car)
# install.packages("optimization")
# install.packages('Rcpp')
# install.packages('car')
# install.packages("randomForest")

source("1/prepare_data.r", encoding = "UTF-8")


start_date <- as.Date("2021-02-01")

df_train_date <- df_train %>% filter(date >= start_date)

min_date <- min(df_train_date$date)

df_train_date$date_int <- as.integer(difftime(df_train_date$date, 
                                              min_date, 
                                              units = "days"))

df_train_date$week <- factor(df_train_date$date_int %% 7)

oktmo_set <- df_train_date %>% select(oktmo) %>% distinct()

days_amount <- difftime("2021-06-30", "2021-02-01", units = "days")

df_2020 <- df_train %>% 
  filter(between(date, 
                 as.Date("2020-02-01") + 2, 
                 as.Date("2020-12-31")))%>% 
  mutate(date = date + 366 - 2, year = 2020)

df_2020_fit <- df_2020 %>% 
  select(date, oktmo, all_of(predict_cols)) %>% 
  rename(setNames(predict_cols, paste0(predict_cols, ".2020"))) %>% 
  arrange(date)


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

except_oktmo <- c()
except_col <- c()

# prev year only: 0.007461143484185842

shifts <- list()

set.seed(42)
for (cur_oktmo in oktmo_set$oktmo){
  print(cur_oktmo)
  df_train_date_reg <- df_train_date %>% filter(oktmo == cur_oktmo)
  
  df_predict_reg <- tibble(oktmo = cur_oktmo,
                           date_int = predict_dates_int)
  
  # df_lm <- df_train_date_reg %>% filter(.[[col_name]] > 0)
  
  df_lm <- df_train_date_reg

  
  df_2020_fit_reg <- df_2020_fit %>% 
    filter(oktmo == cur_oktmo) %>% 
    select(-oktmo) %>% 
    arrange(date)
  
  df_predict <- tibble(date_int = predict_dates_int, 
                       week = factor(predict_dates_int %% 7),
                       date = predict_dates_int + min_date)
  df_predict <- merge(df_predict, df_2020_fit_reg,
                      by = "date")
  
  
  for (col_name in predict_cols){
    col_name2020 <- paste0(col_name, ".2020")
    
    df_lm_part <- df_lm %>% 
      arrange(date)
    
    
    col_2020_data <- df_2020_fit_reg[[col_name2020]]
    
    shift_2020 <- which.max(sapply(1:25, function(x){
      cor.test(col_2020_data[x:(x + nrow(df_lm_part) - 1)],
               df_lm_part[[col_name]])$conf.int[1]}))
    
    if (length(shift_2020) == 0)
      shift_2020 <- 1

    shifts[[cur_oktmo]][col_name] <- shift_2020
      
    df_lm_part[["shifted"]] <- col_2020_data[shift_2020:(shift_2020 + nrow(df_lm_part) - 1)]
    df_predict[["shifted"]] <- col_2020_data[shift_2020:(shift_2020 + nrow(df_predict) - 1)]

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
                          col_name2020)
      
      
      fit.glm <- glm(as.formula(lm_formula),
                 data = df_lm_part)
      
      sf <-  summary(fit.glm)
      
      if (last(sf$coefficients[, 1]) < 0){
        lm_formula <- paste(col_name,
                            " ~ date_int + week")
        
        fit.glm <- glm(as.formula(lm_formula),
                   data = df_lm_part)
      }
      
      
      df_predict <- df_predict %>% mutate(!!col_name := predict(fit.glm, df_predict))
      
      
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
          except_oktmo[exc_cnt] <- cur_oktmo
          except_col[exc_cnt] <- col_name
          
          # fit.exc <- glm(as.formula(paste(col_name,
          #                                 " ~ ",
          #                                 col_name2020)),
          #                data = df_lm_part)
          # 
          # df_predict <- df_predict %>%
          #   mutate(!!col_name := predict(fit.exc,
          #                                df_predict))
          
          if (col_name != "сucumbers_tomatoes"){
            fit.exc <- randomForest(as.formula(paste(col_name,
                                                     " ~ date_int")),
                                    data = df_lm_part, ntree=50)
            
            
            df_predict <- df_predict %>%
              mutate(!!col_name := predict(fit.exc,
                                           df_predict))
            
          }
          else{
            fit.exc <- glm(as.formula(paste(col_name,
                                            " ~ shifted")),
                           data = df_lm_part)
            
            df_predict <- df_predict %>%
              mutate(!!col_name := predict(fit.exc,
                                           df_predict))
            
            # df_predict[[col_name]][df_predict$date > as.Date("2021-04-20")] <-
            #   df_predict[[col_name]][df_predict$date == as.Date("2021-04-20")]
            
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
  geom_point(data = df_2020_fit %>% 
               filter(oktmo == cur_oktmo),
             aes(x = date, 
                 y = .data[[paste0(col_name,
                                   ".2020")]]), col = "green")
  


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
    geom_point(data = df_2020_fit %>% 
                 filter(oktmo == cur_oktmo),
               aes(x = date, 
                   y = .data[[paste0(col_name,
                                     ".2020")]]), col = "green"))
  
}
