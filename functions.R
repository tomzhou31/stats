

## change the Chinese character to pinyin for the variable names
new_names <- function(){
  
  names(df_name) <- NULL 
  df_name <- unlist(c(df_name)) ## transfer dataframe to vector method = c("quanpin", "tone", "toneless"),
  df_name <- gsub( ' ', '', trimws(df_name) )
  df_name <- py(char =df_name , sep = "", other_replace = NULL, dic = pydic(method ="toneless" ))# Chinese character - pinyin
  df_name <- gsub("[^A-Za-z0-9_]" ,"" ,df_name, ignore.case = TRUE)
  
  ## dealing with the duplicate variable names
  temp_name  <- data.frame(name = df_name) %>% mutate(seq = row_number()) %>% arrange(name, seq) %>% 
    dplyr::group_by(name) %>% mutate(dup = row_number() ) %>% ungroup() %>% 
    mutate(name1 = if_else(dup>=2, paste(name,dup, sep = "_"), paste(name) ) ) %>% 
    arrange(seq)
  
  temp_name <- temp_name$name1
  names(temp_name) <- NULL 
  temp_name <- unlist(c(temp_name))
  
  return (temp_name)
}

## keep the oraginal name of the import data
oraginal_name <- function(){
  names(df_name) <- NULL 
  df_name_org <- unlist(c(df_name))
  return (df_name_org)
}

