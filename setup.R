
##########################################################################################
## name    : setup.R 
## author  : TOM ZHOU
## DATE    : 10/04/2020
## Purpose : Initialise the environment and define univsiral variables
## system  : macro os    
## Update history
##          version   update date   note
##             1      10/04/2020    original
##
##
## Instruction:
##          The process defaultly generated a new folder under the input and output folder
##            the folder names is by using the date when you are running the script
##########################################################################################   
rm(list=ls())
library(lubridate)

R_VERSION <-R.Version()$version.string ## get the R version
RUN_DATE <- Sys.Date()                 ## get the sys date as running date
## automatically define the foder name which is used to create a project input and output folder for each
##  project.
FOLD_ALIAS <- "test"
FOLD_NAME <- paste(format(RUN_DATE, "%Y"),format(RUN_DATE, "%m"), format(RUN_DATE, "%d"), sep="_" ) ## format like "2020_04_11"

FOLD_NAME <- paste(FOLD_NAME, FOLD_ALIAS, sep = "_")  



WD <- "user/tomzhou/downloads"               ## difined the working direction
RAW_WD <- paste(WD,"/raw", sep = "")   ## assign raw data direction
INPUT_WD <-  paste(WD,"/input/",FOLD_NAME, sep = "")   ## assign input direction
OUTPUT_WD <- paste(WD,  "/output/",FOLD_NAME, sep = "") ## assign output direcction

## create folder for input data and output result for each project
dir.create(INPUT_WD)
## set up the work direction
setwd(WD)
getwd()

# install.packages("sqldf")
# install.packages("lubridate")

# library(data.table)
# library(lubridate)
# library(openxlsx)
# library(readxl)
# library(dplyr)
# library(sqldf)
# library(tidyverse)

