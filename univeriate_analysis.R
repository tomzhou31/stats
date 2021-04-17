
##########################################################################################
## name    : univariate_analysis.R
## author  : TOM ZHOU
## DATE    : 10/04/2020
## Purpose : Univeriate analysis
##           
## NOTE    :
##          chi-square/ t-test/ ANOVA/ CMH test was implemented based on the data format
##########################################################################################  
library(rmarkdown)
library(arsenal)
df <- raw_1
 as.factor(df$outcome)

aa <- tableby(severity1 ~ outcome , data=raw_1)
summary(aa)

# load mockstudy data
data(mockstudy)
tab.ex <- table(mockstudy[c("arm", "sex", "mdquality.s")], useNA = "ifany")
noby <- freqlist(tab.ex, na.options = "include")
summary(noby)
# show the top 6 rows' frequencies and percents
head(summary(sort(noby, decreasing = TRUE)[c(1:4, 6)]))
withby <- freqlist(tab.ex, strata = c("arm","sex"), na.options = "showexclude")
summary(withby)
var_class <- data.frame(format = apply(df, 2, class), name = names(df) )

names(var_class)