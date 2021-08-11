library(tidyverse)


# Датасет с результатами вычислений
df_test <- df_res %>% 
  mutate(date = convert_input_date(date))

# Датасет с ответами
df_answer <- df_train %>% 
  filter(between(date,
                 as.Date("2020-04-01"),
                 as.Date("2020-06-30")))

# Проверим, все ли в порядке?
if (nrow(df_test) != nrow(df_answer))
  stop("Размеры датасетов не совпадают")

df_calc <- df_test %>% 
  inner_join(df_answer, by = c("date", "oktmo"))

# Проверим длину объединенных данных
if (nrow(df_calc) != nrow(df_answer))
  stop("Изменился размер датасета после объединения данных")


detail_est <- list()
col_est <- c()

for (col_name in predict_cols){
  col_test <- paste0(col_name, ".x")
  col_ans <- paste0(col_name, ".y")
  # for (cur_oktmo in oktmo_set$oktmo){
  #   df_calc_reg <- df_calc %>% 
  #     filter(oktmo == cur_oktmo)
  #   err <- mean(abs(df_calc_reg[[col_test]] - df_calc_reg[[col_ans]])) /
  #     mean(df_calc_reg[[col_ans]])
  #   detail_est[[cur_oktmo]][col_name] <- 1/(1000 * err)
  # }
  
  col_est[col_name] <- mean(abs(df_calc[[col_test]] - df_calc[[col_ans]])) /
    mean(df_calc[[col_ans]])
}

score <- 1/(1000 * mean(col_est))
print(score)

col_est_sort <- sort(col_est)

write_csv(tibble(name = names(col_est_sort), 
                 est = col_est_sort), 
          "estimates_col.csv")

# [1] 0.004009896 - current best, 
# [1] 0.00648237 - use best method
