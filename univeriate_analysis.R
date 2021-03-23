
##########################################################################################
## name    : univariate_analysis.R
## author  : TOM ZHOU
## DATE    : 10/04/2020
## Purpose : Univeriate analysis
##           
## NOTE    :
##          chi-square/ t-test/ ANOVA/ CMH test was implemented based on the data format
##########################################################################################  

library(arsenal)
df <- raw_1
 as.factor(df$outcome)

aa <- tableby(severity1 ~ outcome , data=raw_1)
summary(aa)


var_class <- data.frame(format = apply(df, 2, class), name = names(df) )

names(var_class)