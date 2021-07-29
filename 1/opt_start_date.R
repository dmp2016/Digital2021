start_date <- as.Date("2021-01-15")

df_train_date <- df_train %>% filter(date >= start_date)
min_date <- min(df_train_date$date)

df_train_date$date_int <- as.integer(difftime(df_train_date$date, 
                                              min_date, 
                                              units = "days"))

df_train_date$week <- factor(df_train_date$date_int %% 7)
df_train_date$is_holiday <- factor(sapply(df_train_date$date, is_holiday))

date_list <- as.Date(c("2021-01-25", "2021-02-01", "2021-02-07", "2021-02-15"))
start_predict <- last(date_list)
finish_predict <- as.Date("2021-06-30")

start_date_int <- as.integer(difftime(start_predict, min_date, units = "days"))
finish_date_int <- as.integer(difftime(finish_predict, min_date, units = "days"))
predict_dates_int = start_date_int:finish_date_int



cur_oktmo <- "71000000000"

print(cur_oktmo)
df_train_date_reg <- df_train_date %>% filter(oktmo == cur_oktmo)
  
df_predict_reg <- tibble(oktmo = cur_oktmo,
                         date_int = predict_dates_int)
  
#  for (col_name in predict_cols){
  # df_lm <- df_train_date_reg %>% filter(.[[col_name]] > 0)
df_lm <- df_train_date_reg
df_lm$isw1 = as.integer(df_lm$week == 1)
df_lm$isw2 = as.integer(df_lm$week == 2)
df_lm$isw3 = as.integer(df_lm$week == 3)
df_lm$isw4 = as.integer(df_lm$week == 4)
df_lm$isw5 = as.integer(df_lm$week == 5)
df_lm$isw6 = as.integer(df_lm$week == 6)
  
  
#  if (nrow(df_lm) > 0){
    
test_f <- function(x){
  return(sum(abs(df_lm_part[[col_name]] - (x[1] + df_lm_part$date_int * x[2] +
                                             df_lm_part$isw1 * x[3] +
                                             df_lm_part$isw2 * x[4] +
                                             df_lm_part$isw3 * x[5] +
                                             df_lm_part$isw4 * x[6] +
                                             df_lm_part$isw5 * x[7] +
                                             df_lm_part$isw6 * x[8] +
                                             as.integer(df_lm_part$is_holiday == "TRUE") * x[9])))/nrow(df_lm_part))}


col_name <- "mutton"
opt_f <- 2000000000
opt_date <- NA

for (cur_date_from in date_list){
  df_lm_part <- df_lm %>% filter(date >= cur_date_from)
  fit <- glm(as.formula(paste(col_name, " ~ date_int + week + is_holiday")),
             data = df_lm_part)
  cur_f <- summary(fit)$coefficients[2, "Pr(>|t|)"] # test_f(fit$coefficients)
  print(cur_f)
  if (cur_f < opt_f)
  {
    opt_date <- cur_date_from
    opt_f <- cur_f
  }
}


summary(fit)
str(fit)
gg <- summary(fit)

coef(summary(fit))[,"Pr(>|z|)"]

as.Date(opt_date, origin = "1970-01-01")
    
    
fit <- glm(as.formula(paste(col_name, " ~ date_int + week + is_holiday")),
           data = df_lm)
    
    
res <- optim_nm(test_f, k = 9, start = fit$coefficients)
# res <- optim_sa(test_f,
#                 start = fit$coefficients,
#                 lower = fit$coefficients - abs(fit$coefficients) / 2 - 0.1,
#                 upper = fit$coefficients + abs(fit$coefficients) / 2 + 0.1)

    
df_predict <- tibble(date_int = predict_dates_int, 
                     week = factor(predict_dates_int %% 7),
                     date = predict_dates_int + min_date,
                     isw1 = as.integer(week == 1),
                     isw2 = as.integer(week == 2),
                     isw3 = as.integer(week == 3),
                     isw4 = as.integer(week == 4),
                     isw5 = as.integer(week == 5),
                     isw6 = as.integer(week == 6))
df_predict$is_holiday <- factor(sapply(df_predict$date, is_holiday))


####################################################
    
if (test_f(res$par) < test_f(fit$coefficients))
{
  x <- res$par
  df_predict[[col_name]] <- x[1] + df_predict$date_int * x[2] +
    df_predict$isw1 * x[3] +
    df_predict$isw2 * x[4] +
    df_predict$isw3 * x[5] +
    df_predict$isw4 * x[6] +
    df_predict$isw5 * x[7] +
    df_predict$isw6 * x[8] +
    as.integer(df_predict$is_holiday == "TRUE") * x[9]
} else 
{
  df_predict <- df_predict %>% mutate(!!col_name := predict(fit, df_predict))
  print("LM is better")
  # print(col_name)
}
df_predict[[col_name]] <- ifelse(df_predict[[col_name]] < 0, 0, df_predict[[col_name]])

}
else
df_predict <- df_predict %>% mutate(!!col_name := 0)
df_predict_reg <- cbind(df_predict_reg, 
                    df_predict %>% 
                      filter(date >= date_list[length(date_list)]) %>% 
                      select(!!col_name))
}

if (is.null(res_predict))
  res_predict <- df_predict_reg
else
  res_predict <- rbind(
    res_predict,
    df_predict_reg)

