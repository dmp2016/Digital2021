convert_input_date <- function(x){
  as.Date(paste(substr(x, 7, 10), 
                "-", 
                substr(x, 4, 5),
                "-",
                substr(x, 1, 2),
                sep = ""))
  
}


convert_date_to_inp <- function(d){
  return(sapply(d, function(x){
    s <- as.character(x)
    return(paste0(
      substr(s, 9, 10),
      ".",
      substr(s, 6, 7),
      ".",
      substr(s, 1, 4)))}))
}

