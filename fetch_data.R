##########################################################################################
## name    : function_fetch_data.R 
## author  : TOM ZHOU
## DATE    : 10/04/2020
## Purpose : Automatically scan the "input/" folder then import data from various data formats
##            including csv/txt/xls/xlsx/SAS
## NOTE    :
##          if the import data format is "xls" or "xlsx" make sure the tab name is "Sheet1"
##########################################################################################   

source("setup.R")
source("functions.R")

library(readxl)
library(dplyr)
library(pinyin)
library(sas7bdat)
library(foreign) #import data from SPSS,STATA

## copy data from /raw folder to target file - input
file.copy(file.path(RAW_WD,list.files(RAW_WD)), INPUT_WD)

##NOTE: make sure there is no "." in the file name (except the postfix like .xls)
data_list_org <- list.files(INPUT_WD, pattern=NULL, all.files=FALSE, full.names=FALSE)
data_list <- gsub("[^._a-z]", "", data_list_org, ignore.case = TRUE) ##keep alphabetas, dot and underscore
data_list <- tolower(data_list) 

  
len_list <-length(data_list)


tab_name <- "all"
## https://stackoverflow.com/questions/39878978/how-to-change-a-name-of-multiple-data-frames-in-r-by-adding-the-same-phrase-mo
for (i in 1:len_list) {
  
  ## excel format
  if (gregexpr(".xls", data_list[i], fixed = TRUE) >0 ) {
    df <- read_excel( paste0(INPUT_WD,  "/", data_list_org[i] ), sheet = tab_name)
    ## rename the varibales  - replece the Chinese characters to pinyin 
    ##  and delete the special characters like * -  
    df_name <- read_excel(paste0(INPUT_WD,  "/", data_list_org[i] ), sheet = 1, col_names = FALSE,n_max = 1)
    names(df) <- new_names()
    # assign(gsub(".csv","",data_list[i]) , df)
    # assign(paste(gsub(".csv","",data_list[i]) , "_names", sep = "" ) , oraginal_name()) 
    assign(paste("raw_", i, sep = "" ) , df)
    assign(paste("raw_", i, "_names", sep = "" ) , oraginal_name())  
    rm(df_name, df)
  }
  
  ## CSV format
  if (gregexpr(".csv", data_list[i], fixed = TRUE) >0 ){
    df <-  read.csv( paste0(INPUT_WD,  "/", data_list_org[i] ), header = TRUE)
    df_name <- read.csv( paste0(INPUT_WD,  "/", data_list_org[i] ), header = FALSE,nrows=1)
    names(df) <- new_names()
    
    assign(paste("raw_", i, sep = "" ) , df)
    assign(paste("raw_", i, "_names", sep = "" ) , oraginal_name()) 

    
    rm(df_name, df)
  }
  
  ## SAS format
  if (gregexpr(".sas7bdat", data_list[i], fixed = TRUE) >0 ){
    assign(paste("raw_", i, sep = "" ) , read.sas7bdat( paste0(INPUT_WD,  "/", data_list_org[i] )) )
  }
  
  ## SPSS format
  if (gregexpr(".sav", data_list[i], fixed = TRUE) >0 ){
    assign(paste("raw_", i, sep = "") ,
        read.spss(paste0(INPUT_WD,  "/", data_list_org[i] ), to.data.frame=TRUE,use.value.labels=FALSE) )
  }
  
}
