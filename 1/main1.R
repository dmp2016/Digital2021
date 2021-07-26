library(tidyverse)
library(scorer)
library(Rcpp)
library(optimization)
# install.packages("optimization")
# install.packages('Rcpp')


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

df_train_date <- df_train %>% filter(date >= start_date)

min_date <- min(df_train_date$date)

df_train_date$date_int <- as.integer(difftime(df_train_date$date, 
                                              min_date, 
                                              units = "days"))

df_train_date$week <- factor(df_train_date$date_int %% 7)

oktmo_set <- df_train_date %>% select(oktmo) %>% distinct()

start_predict <- start_date
finish_predict <- as.Date("2021-06-30")

start_date_int <- as.integer(difftime(start_predict, min_date, units = "days"))
finish_date_int <- as.integer(difftime(finish_predict, min_date, units = "days"))
predict_dates_int = start_date_int:finish_date_int

# cur_oktmo <- oktmo_set$oktmo[1]
# 
# col_name <-  predict_cols[1]

res_predict <- NULL

for (cur_oktmo in oktmo_set$oktmo){
  print(cur_oktmo)
  df_train_date_reg <- df_train_date %>% filter(oktmo == cur_oktmo)
  
  df_predict_reg <- tibble(oktmo = cur_oktmo,
                           date_int = predict_dates_int)

  for (col_name in predict_cols){
    fit <- glm(as.formula(paste(col_name, " ~ date_int + week")), data = df_train_date_reg)
    # df_predict <- tibble(date_int = predict_dates_int)
    df_predict <- tibble(date_int = predict_dates_int, week = factor(predict_dates_int %% 7))
    df_predict <- df_predict %>% mutate(!!col_name := predict(fit, df_predict))
    
    ###
    # df_train_date_reg$tmp_predict <- predict(fit, df_train_date_reg)
    # 
    # opt_f <- function(x){
    #   sum(abs(df_train_date_reg$tmp_predict + x[1] * 
    #             sin(2 * pi * (df_train_date_reg$date_int + x[3]) / x[2])
    #           - df_train_date_reg[[col_name]]))}
    
    
    # amp <- mean(abs(df_train_date_reg[[col_name]] - df_train_date_reg$tmp_predict)) / 2
    # 
    # if (amp > 1){
    #   res <- optim_sa(opt_f, 
    #                   start = c(amp, 45, 45),
    #                   lower = c(0, 1, 0),
    #                   upper = c(2 * amp, 90, 90))
    #   ###
    #   if (res$function_value < opt_f(c(0, 1, 1))){
    #     print(c(res$function_value, opt_f(c(0, 1, 1))))
    #     df_predict[[col_name]] <- df_predict[[col_name]] + res$par[1] * 
    #       sin(2 * pi * (df_predict$date_int  + res$par[3]) / res$par[2])
    #   }
    # }

    df_predict_reg <- cbind(df_predict_reg, df_predict %>% select(!!col_name))
  }
  
  if (is.null(res_predict))
    res_predict <- df_predict_reg
  else
    res_predict <- rbind(
      res_predict,
      df_predict_reg)
}


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

df_test[1, 1:4]

df_res <- NULL

for (ind in 1:nrow(df_test)){
  print(ind)
  cur_date <- df_test$date_format[ind]
  cur_oktmo <- df_test$oktmo[ind]
  if (is.null(df_res))
    df_res <- cbind(df_test[ind, 1:4], 
                    res_predict %>% 
                      filter(date == cur_date & 
                               oktmo == cur_oktmo) %>% 
                      select(!!predict_cols))
  else
    df_res <- rbind(df_res,
                    cbind(df_test[ind, 1:4], 
                          res_predict %>% 
                            filter(date == df_test$date_format[ind] & 
                                     oktmo == df_test$oktmo[ind]) %>% 
                            select(!!predict_cols)))
}


write_csv(df_res, "1/mytest_ord.csv")


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




cur_oktmo <- "64000000000"
ggplot() +
  geom_point(data = res_predict %>% filter(oktmo == cur_oktmo), 
             aes(x = date, pasta_value), col = "red") +
  geom_point(data = df_train_date %>% filter(oktmo == cur_oktmo), 
             aes(x = date, pasta_value), col = "blue")



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


ggplot(data = df_train_reg %>% 
         arrange(date), aes(x = date, y = bread_value)) +
  geom_point() +
  geom_line() +
  geom_smooth(method = "lm")
  

ggplot(data = df_train_reg %>% 
         arrange(date)) +
  geom_point(aes(x = date_int, y = bread_value)) +
  geom_line(aes(x = date_int, y = bread_value)) +
  geom_point(aes(x = date_int, y = pred + amp * sin((2 * pi * date_int + 50) / 30)), 
             col = "red") +
  geom_smooth(aes(x = date_int, y = bread_value), method = "lm")




# df_test <- read_csv("1/test.csv", col_types = cols(.default = col_character()))
# df_test %>% group_by(oktmo) %>% 
#   summarise(cnt = n()) %>% 
#   filter(cnt > 91)

df_train_reg$week <- factor(df_train_reg$date_int %% 7)

col_name <- "bread_value"
fit <- glm(as.formula(paste(col_name, " ~ date_int + week")), data = df_train_reg)
fit <- lm(as.formula(paste(col_name, " ~ date_int")), data = df_train_reg)

df_train_reg$pred <- predict(fit)


ggplot(data = df_train_reg %>% 
         arrange(date)) +
  geom_point(aes(x = date_int, y = bread_value)) +
  geom_line(aes(x = date_int, y = bread_value)) +
  geom_point(aes(x = date_int, y = pred), col = "red") +
  geom_smooth(aes(x = date_int, y = bread_value), method = "lm")



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
