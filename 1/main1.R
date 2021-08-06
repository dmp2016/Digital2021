library(tidyverse)
library(scorer)
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

# for (cur_oktmo in oktmo_set$oktmo) {
#   for (cur_col in paste0(predict_cols, ".2020")){
#     df_2020_fit[[cur_col]][df_2020_fit$oktmo == cur_oktmo] <-
#       smooth(x = df_2020_fit[[cur_col]][df_2020_fit$oktmo == cur_oktmo])
#   }
# }

df_2020_fit$date_int_1 <- NULL


# ggplot(data = df_2020_fit %>% filter(oktmo == "30000000000")) +
#   geom_point(aes(x = date, y = vegetables.2020), col = "red") +
#   geom_point(data = df_2020_fit_a %>% filter(oktmo == "30000000000"), aes(x = date, y = vegetables), col = "blue")
#        
# qplot(data = df_2020_fit %>% filter(oktmo == "30000000000"), 
#       x = date, 
#       y = pasta.2020)


df_train_date <- merge(df_train_date, df_2020_fit, by = c("oktmo", "date"))


# df_train_date %>% select(date, is_holiday) %>% distinct()


# date_list <- as.Date(c("2021-01-25", "2021-02-01", "2021-02-07", "2021-02-15"))
# start_predict <- last(date_list)
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
  df_train_date_reg <- df_train_date %>% filter(oktmo == cur_oktmo)
  
  df_predict_reg <- tibble(oktmo = cur_oktmo,
                           date_int = predict_dates_int)
  
  # df_lm <- df_train_date_reg %>% filter(.[[col_name]] > 0)
  
  df_lm <- df_train_date_reg
  df_lm$isw1 = as.integer(df_lm$week == 1)
  df_lm$isw2 = as.integer(df_lm$week == 2)
  df_lm$isw3 = as.integer(df_lm$week == 3)
  df_lm$isw4 = as.integer(df_lm$week == 4)
  df_lm$isw5 = as.integer(df_lm$week == 5)
  df_lm$isw6 = as.integer(df_lm$week == 6)
  
  
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
    
    df_lm_part <- df_lm_part[5:(nrow(df_lm_part) - 5), ]
    
    if (nrow(df_lm_part) > 0 & sum(df_lm_part[[col_name]]) != 0)
    {

      if (add_col_name != "")
        lm_formula <- paste(col_name, 
                            " ~ date_int + week + ", 
                            col_name2020,
                            " + ",
                            add_col_name)
      else
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
          # fit <- randomForest(as.formula(paste(col_name, 
          #                                      " ~ ", 
          #                                      col_name2020)),
          #                     data = df_lm_part, ntree=50)
          if (add_col_name != "")
            lm_formula_new <- paste(col_name, 
                                    " ~ ", 
                                    col_name2020,
                                    " + ",
                                    add_col_name)
          else
            lm_formula_new <- paste(col_name, 
                                    " ~ ", 
                                    col_name2020)
          
          fit <- lm(as.formula(lm_formula_new),
                    data = df_lm_part)
          
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


######################################################
# LM prediction
######################################################


cur_oktmo <- "71000000000"
df_train_date_reg <- df_train_date %>% filter(oktmo == cur_oktmo)


df_lm <- df_train_date_reg
df_lm$isw1 = as.integer(df_lm$week == 1)
df_lm$isw2 = as.integer(df_lm$week == 2)
df_lm$isw3 = as.integer(df_lm$week == 3)
df_lm$isw4 = as.integer(df_lm$week == 4)
df_lm$isw5 = as.integer(df_lm$week == 5)
df_lm$isw6 = as.integer(df_lm$week == 6)
wday(as.Date("2021-07-27"))

df_predict <- tibble(date_int = predict_dates_int, 
                     week = factor(predict_dates_int %% 7),
                     date = predict_dates_int + min_date,
                     isw1 = as.integer(week == 1),
                     isw2 = as.integer(week == 2),
                     isw3 = as.integer(week == 3),
                     isw4 = as.integer(week == 4),
                     isw5 = as.integer(week == 5),
                     isw6 = as.integer(week == 6),
)
df_predict$is_holiday <- factor(sapply(df_predict$date, is_holiday))


col_name  <- "chicken_value"
fit <- glm(as.formula(paste(col_name, " ~ date_int + week + is_holiday")),
           data = df_lm)
summary(fit)
# df_predict <- df_predict %>% mutate(!!col_name := predict(fit, df_predict))
# df_predict[[col_name]] <- ifelse(df_predict[[col_name]] < 0, 0, df_predict[[col_name]])


test_f <- function(x){
  return(sum(abs(df_lm[[col_name]] - (x[1] + df_lm$date_int * x[2] +
                                        df_lm$isw1 * x[3] +
                                        df_lm$isw2 * x[4] +
                                        df_lm$isw3 * x[5] +
                                        df_lm$isw4 * x[6] +
                                        df_lm$isw5 * x[7] +
                                        df_lm$isw6 * x[8] +
                                        as.integer(df_lm$is_holiday == "TRUE") * x[9]))))}


res <- optim_sa(test_f,
                start = fit$coefficients,
                lower = fit$coefficients - abs(fit$coefficients) / 2 - 0.1,
                upper = fit$coefficients + abs(fit$coefficients) / 2 + 0.1)

res <- optim_nm(test_f, start = c(fit$coefficients), k = 9, exit = 2000)

test_f(c(fit$coefficients))
test_f(res$par)
fit$coefficients
res$par

ggplot(data = df_train_date %>% 
         filter(oktmo == cur_oktmo)) +
  geom_point(aes(x = date_int, y = .data[[col_name]]), col = "blue") +
  geom_line(aes(x = date_int, y = .data[[col_name]]), col = "blue") +
  geom_smooth(aes(x = date_int, y = .data[[col_name]]), method = "lm") +
  geom_point(data = df_predict, 
             aes(x = date_int, y = .data[[col_name]]), col = "red") +
  geom_point(data = df_predict,
             aes(x = date_int, y = res$par[1] + 
                   date_int * res$par[2] + 
                   as.integer(is_holiday == "TRUE") * res$par[3]), col = "green")




#########################
# Test lm Warnings
#########################

cur_oktmo <- "75000000000"
col_name <- "pasta"
qplot(data = df_train_date %>% 
        filter(oktmo == cur_oktmo & date_int <= 80 & .[[col_name]] > 0), 
      x = date, 
      y = .data[[col_name]])


df_lm <- df_train_date %>% filter(oktmo == cur_oktmo & date_int <= 80) %>% filter(.[[col_name]] > 0)
df_predict <- tibble(date_int = predict_dates_int, week = factor(predict_dates_int %% 7))

fit <- lm(as.formula(paste(col_name, " ~ date_int")), 
          data = df_lm)
df_predict <- df_predict %>% mutate(!!col_name := predict(fit, df_predict))



#######################################################################



# length(colnames(df_res))
# sum(colnames(df_res)[5:79] != predict_cols)
# 
# res_test <- merge(df_test %>% select(region, oktmo, okato, date, date_formate),
#                   res_predict %>% select(-date_int), 
#                   by.x = c("oktmo", "date_formate"),
#                   by.y = c("oktmo", "date"))
# 
# 
# res_test$date_formate <- NULL
# 
# write_csv(res_test, "1/mytest.csv")




col_name <- "bread_value"
df_train_date_reg <- df_train_date %>% filter(oktmo == "75000000000")
df_lm <- df_train_date_reg %>% select(date, date_int, all_of(col_name))

fit <- lm(all_of(bread_value) ~ date_int, data = df_train_date_reg)

fit$coefficients

df_lm$pred <- predict(fit, df_lm)


ggplot(data = df_lm %>% 
         arrange(date)) +
  geom_point(aes(x = date_int, y = bread_value)) +
  geom_line(aes(x = date_int, y = bread_value)) +
  geom_point(aes(x = date_int, y = pred + 0.5 * sin(2 * pi * df_lm$date_int / 10)), 
             col = "red") +
  geom_smooth(aes(x = date_int, y = bread_value), method = "lm")

sum(abs(df_lm$pred  + 0.5 * sin(2 * pi * df_lm$date_int / 10 + 1.8) - df_lm$bread))




##########################################



summary(fit)



for (col in predict_cols){
  df_train_col
}


df_train_reg <- df_train_date %>% filter(oktmo == cur_oktmo)


df_train_reg$week <- factor(df_train_reg$date_int %% 7)
weekdays(df_train_reg$date)
wday(df_train_reg$date)


col_name <- "bread"
fit <- glm(as.formula(paste(col_name, " ~ date_int + week")), data = df_train_reg)
# fit <- lm(as.formula(paste(col_name, " ~ date_int")), data = df_train_reg)

df_train_reg$pred <- predict(fit, df_train_reg)


ggplot(data = df_train_reg %>% 
         arrange(date)) +
  geom_point(aes(x = date_int, y = bread)) +
  geom_line(aes(x = date_int, y = bread)) +
  geom_point(aes(x = date_int, y = pred), col = "red") +
  geom_smooth(aes(x = date_int, y = bread), method = "lm")


sum(abs(df_train_reg$pred - df_train_reg$bread))



opt_f <- function(x){
  sum(abs(df_train_reg$pred  + x[1] * sin(2 * pi * (df_train_reg$date_int + x[3]) / x[2])
          - df_train_reg[[col_name]]))}


opt_f(c(0, 1, 1))

amp <- mean(abs(df_train_reg[[col_name]] - df_train_reg$pred)) / 2

res <- optim_sa(opt_f, 
                start = c(amp, 45, 45),
                lower = c(0, 1, 0),
                upper = c(2 * amp, 90, 90)
)


res$par
res$function_value


ggplot(data = df_train_reg %>% 
         arrange(date)) +
  geom_point(aes(x = date_int, y = bread_value)) +
  geom_line(aes(x = date_int, y = bread_value)) +
  geom_point(aes(x = date_int, y = pred + res$par[1] * sin(2 * pi * (date_int + res$par[3]) / res$par[2])), 
             col = "red") +
  geom_smooth(aes(x = date_int, y = bread_value), method = "lm")



opt_f(c(amp, 30, 50))
opt_f(c(0.5, 1.8, 10))
opt_f(c(0, 6.93, 0))




opt_f_t <- function(x){ opt_f(c(0.2, 2, x)) }


qqplot(x = 1:11, y = sapply(1:11, opt_f_t))

test_f <- function(x){x ^ 2 + 1}
test_f(2)
optim_sa(test_f, 
         start = 1,
         lower = -5,
         upper = 5)

version