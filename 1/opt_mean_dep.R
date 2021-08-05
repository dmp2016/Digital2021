install.packages("RcppRoll")
library(RcppRoll)

cur_oktmo <- "71000000000"
cur_oktmo <- "26000000000"
cur_oktmo <- "64000000000"
cur_oktmo <- "75000000000"
cur_oktmo <- "76000000000"


df_train_date_reg <- df_train_date %>% 
  filter(oktmo == cur_oktmo) %>% 
  arrange(date)

df_year_reg <- df_2020 %>% 
  filter(oktmo == cur_oktmo) %>% 
  arrange(date)

col_name <- "bread_value"
col_name <- "roots"


df_grp <- df_train_date_reg %>% 
  group_by(week(date)) %>% 
  summarise(val = mean(.data[[col_name]])) %>% 
  rename(week = 'week(date)') %>% 
  arrange(week)


df_grp_prev <- df_years_reg %>% 
  group_by(week(date)) %>% 
  summarise(val = mean(.data[[col_name]])) %>% 
  rename(week = 'week(date)') %>% 
  arrange(week)


ggplot() +
  geom_point(data = df_grp, aes(x = week, y = val), col = "red") +
  geom_smooth(data = df_grp, aes(x = week, y = val), col = "red", method = "loess") +
  geom_point(data = df_grp_prev, aes(x = week, y = val), col = "blue") +
  geom_smooth(data = df_grp_prev, aes(x = week, y = val), col = "blue", method = "loess")


cor.test(df_grp$val, df_grp_prev$val[1:nrow(df_grp)])


col_name <- "rice"
col_name <- "roots"
col_name <- "bread_value"
col_name <- "dt"


df_data <- tibble(x = df_train_date_reg$date[1:50], 
                  y = roll_mean(df_train_date_reg[[col_name]], 10, align = "left"))

df_data_prev <- tibble(x = df_year_reg$date[1:141], 
                       y = roll_mean(df_year_reg[[col_name]], 10, align = "left"))


ggplot() +
  geom_point(data = df_data, aes(x = x, y = y), col = "red") +
  geom_point(data = df_data_prev, aes(x = x, y = y), col = "blue")

cor.test(df_data$y, df_data_prev$y[1:50])


ggplot() +
  geom_point(aes(x = df_data$y[1:53], y = df_data_prev$y[1:53]), col = "red")
