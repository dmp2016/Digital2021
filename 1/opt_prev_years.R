library(tidyverse)

source("1/prepare_data.r", encoding = "UTF-8")

wday("2021-02-01")
wday(as.Date("2020-02-01") + 2)
wday(as.Date("2019-02-01") + 3)

# Начинаем с того же дня недели, с понедельника

days_amount <- difftime("2021-06-30", "2021-02-01", units = "days")

df_2021 <- df_train %>% 
  filter(between(date, 
                 as.Date("2021-02-01"), 
                 as.Date("2021-06-30"))) %>% 
  mutate(date = date, year = 2021)


df_2020 <- df_train %>% 
  filter(between(date, 
                 as.Date("2020-02-01") + 2, 
                 as.Date("2020-02-01") + 2 + days_amount)) %>% 
  mutate(date = date + 366 - 2, year = 2020)

predict_cols1 <- predict_cols

df_2020 %>% 
  select(date, oktmo, all_of(predict_cols)) %>% 
  rename(setNames(predict_cols, paste0(predict_cols, ".2021")))


min(df_2020$date)

df_2019 <- df_train %>% 
  filter(between(date, 
                 as.Date("2019-02-01") + 3, 
                 as.Date("2019-02-01") + 3 + days_amount)) %>% 
  mutate(date = date + 365 - 3 + 366, year = 2019)

min(df_2019$date)

df_years <- rbind(df_2019, df_2020, df_2021)
df_years$year <- factor(df_years$year)

colnames(df_train)

col_name <- "pasta"

df_predict_test <- read_csv("1/mytest_ord.csv",
                            col_types = cols(
                              .default = col_double(),
                              date = col_character(),
                              oktmo = col_character(),
                              okato = col_character(),
                              region = col_character()
                            ))


df_predict_test$date <- as.Date(paste(substr(df_predict_test$date, 7, 10), 
                               "-", 
                               substr(df_predict_test$date, 4, 5),
                               "-",
                               substr(df_predict_test$date, 1, 2),
                               sep = ""))


cur_oktmo <- "71000000000"
cur_oktmo <- "26000000000"
cur_oktmo <- "64000000000"
cur_oktmo <- "75000000000"
cur_oktmo <- "45000000000"


cnt <- 0

vec_oktmo <- c()
vec_col_name <- c()

for (cur_oktmo in oktmo_set$oktmo){
  tmp <- df_predict_test %>% 
    filter(date >= as.Date("2021-04-01") & oktmo == cur_oktmo)
  tmp_train <- df_2021 %>% 
    filter(date < as.Date("2021-04-01") & oktmo == cur_oktmo)
  for (col_name in predict_cols){
    if (min(tmp_train[[col_name]]) > 0){
      rate <- mean(tmp[[col_name]][between(tmp$date, 
                                          as.Date("2021-06-20"), 
                                          as.Date("2021-06-30"))]) /
        mean(tmp[[col_name]][between(tmp$date, 
                                     as.Date("2021-04-01"), 
                                     as.Date("2021-04-10"))])
      if (rate < 1/2 | rate > 2)  {
        print(paste(cur_oktmo, col_name))
        vec_oktmo[length(vec_oktmo) + 1] <- cur_oktmo
        vec_col_name[length(vec_col_name) + 1] <- col_name
        cnt <- cnt + 1
      }
    }
  }
}


length(vec_oktmo)


df_except <- tibble(e_oktmo = vec_oktmo, e_col_name = vec_col_name)

write_csv(df_except, "df_except.csv")

cur_oktmo <- "57000000000"

df_years_reg <- df_years %>% filter(oktmo == cur_oktmo)
df_predict_test_reg <- df_predict_test %>% filter(oktmo == cur_oktmo)

head(df_years_reg$oktmo)
head(df_predict_test_reg$oktmo)

colnames(df_years_reg)

col_name <- "сucumbers_tomatoes"

ggplot(data = df_years_reg) +
  geom_path(aes(x = date,
                y = .data[[col_name]], col = year)) +
  geom_smooth(aes(x = date,
                y = .data[[col_name]], col = year), method = "lm") +
  geom_path(data = df_predict_test_reg,
            aes(x = date,
                y = .data[[col_name]])) +
  ggtitle(col_name)

for (col_name in colnames(df_train)[5:length(colnames(df_train))]){
  show(ggplot(data = df_years_reg) +
         geom_path(aes(x = date,
                       y = .data[[col_name]], col = year)) +
         geom_path(data = df_predict_test_reg,
                   aes(x = date,
                       y = .data[[col_name]])) +
         # geom_point(aes(x = date, y = .data[[col_name]], col = year)) +
         # geom_point(aes(x = date, y = smooth(x = .data[[col_name]]), col = year)) +
         # geom_path(aes(x = date, y = .data[[col_name]], col = year), method = "loess") +
         # geom_path(aes(x = date, y = .data[[col_name]], col = year), method = "lm") +
         ggtitle(col_name))
  # data_m <- merge(df_years_reg %>% 
  #                   select(year, date, col_name) %>% 
  #                   filter(year == 2019),
  #                 df_years_reg %>% 
  #                   select(year, date, col_name) %>% 
  #                   filter(year == 2021),
  #                 by = "date")
  # show(ggplot(data = data_m) +
  #        geom_point(aes(x = .data[[paste0(col_name, ".x")]], 
  #                      y = .data[[paste0(col_name, ".y")]])) +
  #        # geom_point(aes(x = date, y = .data[[col_name]], col = year)) +
  #        # geom_point(aes(x = date, y = smooth(x = .data[[col_name]]), col = year)) +
  #        # geom_path(aes(x = date, y = .data[[col_name]], col = year), method = "loess") +
  #        # geom_path(aes(x = date, y = .data[[col_name]], col = year), method = "lm") +
  #        ggtitle(col_name))
  # f <- cor.test(data_m[[paste0(col_name, ".x")]], 
  #               data_m[[paste0(col_name, ".y")]])
  # 
  # print(paste(col_name, 
  #             f$conf.int[1],
  #             f$conf.int[2]))
}


f <- cor.test(data_m[[paste0(col_name, ".x")]], 
         data_m[[paste0(col_name, ".y")]])


f$conf.int

ggplot(data = df_years_reg) +
  geom_path(aes(x = date, 
                y = .data[[col_name]], col = year)) +
  geom_path(data = df_predict_test,
            aes(x = df_predict_test$date,
                y = df_predict_test[[col_name]]))

col_name <- "legumes"

ggplot(data = df_years_reg) +
  # geom_point(aes(x = date, y = .data[[col_name]], col = year)) +
  geom_path(aes(x = date, y = smooth(x = .data[[col_name]], kind = "3RSS"), col = year))


c("3RS3R", "3RSS", "3RSR", "3R", "3", "S")

  geom_smooth(aes(x = date, y = .data[[col_name]] / mean(.data[[col_name]]), col = year), method = "loess") +
  geom_smooth(aes(x = date, y = .data[[paste0(col_name, "_value")]] / mean(.data[[paste0(col_name, "_value")]]), col = year), method = "loess")
