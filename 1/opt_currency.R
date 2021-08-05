library(tidyverse)

source("1/prepare_data.r", encoding = "UTF-8")


df_currency <- read_csv("1/forex_usd_data.csv") %>% 
  rename(date = "date(y-m-d)", rub = "Russian Ruble") %>% 
  select(date, rub) %>% 
  filter(between(date, 
                 as.Date("2021-02-01"), 
                 as.Date("2021-06-30")))

df_2021 <- df_train %>% 
  filter(between(date, 
                 as.Date("2021-02-01"), 
                 as.Date("2021-06-30"))) %>% 
  mutate(date = date, year = 2021)


df_2020 <- df_train %>% 
  filter(between(date, 
                 as.Date("2020-02-01"), 
                 as.Date("2020-06-30"))) %>% 
  mutate(date = date + 366, year = 2020)


df_2019 <- df_train %>% 
  filter(between(date, 
                 as.Date("2019-02-01"), 
                 as.Date("2019-06-30"))) %>% 
  mutate(date = date + 366 + 365, year = 2019)

df_years <- rbind(df_2019, df_2020, df_2021)
df_years$year <- factor(df_years$year)

cur_oktmo <- "64000000000"
df_years_reg <- df_years %>% filter(oktmo == cur_oktmo)

col_name <- "tea"

ggplot(data = df_years_reg) +
  geom_point(aes(x = date, y = .data[[col_name]], col = year)) +
  geom_smooth(aes(x = date, y = .data[[col_name]], col = year), method = "loess") +
  geom_point(data = df_currency, aes(x = date, y = rub))


