get_mae <- function(x, y){mean(abs(x - y))}

source("1/prepare_data.R", encoding = "UTF-8")


df1 <- df_2019
df2 <- df_2020

get_df_score(df1, df2, predict_cols){
  df <- merge(df1, df2, by = c("date", "oktmo"), all.y = T)
  
  x_predict_cols <- paste0(predict_cols, ".x")
  y_predict_cols <- paste0(predict_cols, ".y")
}


colnames(df)
df_tmp <- df %>% filter(is.na(pasta.x))
