############ clean data
# rm(list = ls())
## import data
library(readxl)
library(dplyr)
library(sqldf)

wd <- paste("C:/Users/tomzh/Downloads/Covid19")

### import data 
covid <- read_excel( paste0(wd,  "/input/data16.xls" ), "data16") %>% mutate(age_group = case_when(
  age >0 & age <=40 ~ 1,
  age > 40 & age <=60 ~ 2,
  age > 60 ~ 3
),
age_group1 = case_when(
  age >0 & age <=50 ~ 1,
  age > 50 ~ 2
  
  
) ) %>% mutate(severity2 = severity1 - 1 ) %>% filter(!is.na(regularity) )
covid <- covid %>% mutate(severity_char = if_else(severity1 ==1, "Non-severe", "severe")) 
# select(id, age, age1, age_group,amh1, age_group1, severity1, severity2, admissiondate,	curedate,	deathdate,
#        regularity,regularity1, outcome,coexsiting) %>% mutate(admission_date = as.Date(admissiondate) , 
#        death_date = as.Date(deathdate),recovery_date = as.Date(curedate))

### import data for correlation for E2 and c3 due to one row missing in COVID data (don't know the reason)
E2andC3 <- read_excel( paste0(wd,  "/input/E2andC3.xls" ), "E2andC3") %>% filter(!is.na(e2) & !is.na(c3))


# 
# amh_control <-  read_excel( paste0(wd,  "/input/AMH.xls" ), "amh")  
# plot(x=amh_control$age, y=amh_control$BECKAMH)
# 
# covid_amh <- covid %>% filter(!is.na(amh)) %>% mutate(amh_group = if_else(amh < 1.1, 1, 2)) 
# plot(x=covid_amh$age, y=covid_amh$amh)
# 
# # relationship between amh and sevevrity
# table(covid_amh$amh_group, covid_amh$severity)
# chisq.test(aa$regularity1, aa$severity1)
# fisher.test(covid_amh$amh_group, covid_amh$severity)
# 
# aa <- covid %>% select(amh,amh1,regularity,severity2,age,age_group) %>% filter(!is.na(amh1 ))
# 
# table(covid$regularity,covid$severity1 )
# 
# mylogit <- glm(severity2 ~ age_group + amh_group + regularity1, data = covid_amh, family = "binomial")
# summary(mylogit)
# 
# 
# raw <- read_excel( paste(wd,  "/raw/epidata数据导出 --0304.xlsx" ,sep = ""), "epidata+检验")   
# raw1 <- raw %>% select(PATIENT_ID, `疾病程度`)
# names(raw1) <- c("id", "level")
# 
# huizong <- read_excel( paste(wd,  "/raw/20200304汇总ncov-卵巢功能.xlsx" ,sep = ""), "Sheet1")   
# names(huizong) <- c("name", "id", "age", "severity", "outcome", "icu",	"coexsiting",	"coexsitingdisease"	,"local",
#                     "exposurehistoty", "first_symptom", "wbc"	,"lc"	,"lcpercentage",	"es",	"crp"	,"procalcitonin",
#                     "ferritin",	"respiratoryvirus",	"iga",	"igg"	,"igm", "c3"	,"c4",	"il10",	"il1",	"il2"	,"il6",	"il8","tnf", 
#                     "delete_ct_time", "delete_ct_discription", "delete_ct_result", 
#                     "family",	"coffee",	"smoke"	,"drink",	"benign",	"malignancy", "operationhistory",	"operationdate"	,
#                     "menarhe",	"Menstrual",	"menstrualcycle",	"menstrualvolume",
#                     "lmp"	,"pmp"	,"menstrualchange"	,"dysmenorrhea",	"regularity",	"delete_commnet", "g",	"p"	,"a",	"sexualpartner",	"sexualactivity",
#                     "sexualcount",	"contraception"	,"normalmental"	,"recentmental", "amh",	"p2",	"fsh",	"lh",	"prl",	"e2",	"t",
#                     "delete_sex_hormone", "delete_globulin", "height",	"weight"
# ) 
# huizong1<- huizong %>%  select(-starts_with("delete_"))
# 
# huizong2 <- sqldf("
#                   select a.*, b.level from huizong1 a left join raw1 b on a.id = b.id ") %>% select(-severity) %>% rename(severity = level) %>% 
#   mutate(age1 = gsub("[^0-9.-]", "", age) )  %>% 
#   mutate(age1 = if_else(age1 == "", as.numeric(age1), NA))
# mutate(age_group = case_when(
#   age1 >0 & age1 <=40 ~ 1,
#   age1 > 40 & age1 <=55 ~ 2,
#   age1 > 55 ~ 3
# ), severity1 = if_else(severity == 1, 0, 1), 
# regularity1 = case_when(
#   regularity == "1" | regularity == "1A" ~ 1,
#   regularity == "2"  ~ 2,
#   regularity == "3" | regularity == "4" | regularity == "4A" | regularity == "哺乳期" |  regularity == "妊娠期" ~ 3
# )) %>% mutate(regularity2 = if_else(regularity1<=2, 1, 0 ) )
# # bmi	ground	localpathy	bilateral	interstitial
# # fever	cough	throatache	fatigue	soremuscle	breathe	stomachache	diarrhea   
# 
# names(huizong)
# 
# 
# str(raw)
# add <-  read_excel( paste0(wd,  "/data/AMH补充.xlsx" ), "Sheet1")  
# names(add) <- c("un1"	, "un2",	"id",	"name"	,"age",	"amh"	,"p" ,"fsh" , "lh"	,"prl",	"e2",	"t",	"il10"	,"il1"	,"il2"	,"il6"	,"il8"	,"tnf")
# 
# exclude <- read_excel( paste0(wd,  "/data/0307轻症随访汇总.xlsx" ), "sheet2信息整理")  
# exclude1 <- exclude[,1:2]
# names(exclude1) <- c("name", "age")
# covid_1 <- covid %>% filter(id == 2001978673) %>% mutate(
#   # 年龄	AMH	P	FSH	LH	PRL	E2	T	il10	il1	il2	il6	il8	tnf
#   age ="42"	, amh= "2.46", 	p="1.12",	fsh="6.17",	lh="28.9",	prl="18.04"	,e2= "519"	, t="0.65",	il10 ="<5.0",
#   il1 ="11",	il2 ="250",	il6="<1.5",	il8="<5.0", 	tnf="9")
# #   34		1.82	3.79	2.84	24.22	99	0.59	<5.0	5.7	220	5.14	6.2	4.6
# #   45	0.17	0.93	6.52	4.38	40.11	237	0.61	<5.0	<5.0	335	2.49	14.1	7.7
# 
# 
# 
# covid_2 <- covid %>% filter(id == 2002318528)%>% mutate(
#   # 年龄	AMH	P	FSH	LH	PRL	E2	T	il10	il1	il2	il6	il8	tnf
#   age ="34"	, amh= "", 	p="1.82",	fsh="3.79",	lh="2.84",	prl="24.22"	,e2= " 99"	, t="0.59",	il10 ="<5.0",
#   il1 ="5.7",	il2 ="220",	il6="5.14",	il8="6.2", 	tnf="4.6")
# 
# covid_3 <- covid %>% filter(id == 2002283772) %>% mutate(
#   age ="45"	, amh= "0.17", 	p="0.93",	fsh="6.52",	lh="4.38",	prl="40.11"	,e2= " 237"	, t="0.61",	il10 ="<5.0",
#   il1 ="<5.0",	il2 ="335",	il6="2.49",	il8="14.1", 	tnf="7.7")
# 
# aa <- as.data.frame(names(covid))
# covid1 <- sqldf("
#                 select case when b.id >0 then b.id
#                 ")
# covid1 <- covid %>% mutate(age_group = case_when(
#   
#   age >0 & age <=40 ~ 1,
#   age > 40 & age <=60 ~ 2,
#   age > 60 ~ 3
# )
# )
# 
# 
# covid1_no_fanc <- sqldf("select a.* from covid1 a left join exclude1 b on a.name = b.name where b.name is null ")
# 




