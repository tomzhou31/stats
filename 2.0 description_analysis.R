


library(readxl)
library(dplyr)
library(data.table)
library(lubridate)
library(openxlsx)

## IMPORT DATA
desc <- read_excel( paste0(wd,  "/input/description.xls" ), "SQL Results")

names(desc) <- c(
  "delete_id", "id", "inpatient_freq", "age", "gender",
  "nation", "marrige", "occupation", "hospital_visit_freq", "enter_date", 
  "out_date","discharge_disposition", "discharge_method", "diagnose","delete_department", 
  "onset_date","first_symptom",  "first_diagnose_hospital", "first_diagnose_date","clinical_symptom",
  "clinical_manifestation", "epidemical_investigation", "sevirity", "outcome", "doctor_label",
  "sample_type", "sample_resut", "delete_note","codition", "hr",
  "delete_stool",  "breathe", "pulse", "height","weight",
  "temperature", "bp", "blood_oxgen_saturation", "record_date"
      ) 

desc_ <- desc %>% mutate(gender = case_when (gender == "男" ~ "Male",
                                              gender == "女" ~ "Female",
                                              TRUE ~ gender
                                   
                                   ))



desc_ <- desc_ %>% mutate(month_label = if_else(gregexpr(age,  pattern = '月')>0,1,0 ) ) %>%
  mutate(age1 = if_else(month_label == 1, as.numeric(substr(age, start = 1, stop = 1) ) , as.numeric( gsub('\\D+','', age) ) ))%>% 
  mutate( month = dplyr::if_else(month_label == 1, 
                         as.numeric(substr(age, start = 3, stop = 3) ) ,
                         0  ) ) %>% mutate(age1 = age1 + month/12)

desc_ <- desc_ %>% mutate(
  
  age_group = case_when(
    age1 < 10 ~ '0-10',
    age1 >= 10 & age1 <20 ~ '10-19',
    age1 >= 20 & age1 <30 ~ '20-29',
    age1 >= 30 & age1 <40 ~ '30-39',
    age1 >= 40 & age1 <50 ~ '40-49',
    age1 >= 50 & age1 <60 ~ '50-59',
    age1 >= 60 & age1 <70 ~ '60-69',
    age1 >=70 ~'70-' 
    
  ),
  age_group1 = case_when(
    age1 <= 50 ~ '0-50',
    age1 > 50 ~'51-' 
    
  ),
  nation_group = case_when(
    nation == "汉族" ~ "han",
    TRUE ~ "others"
  ),
  
  sevirity_group = case_when(
    sevirity == "轻症" ~ "1. Mild",
    sevirity == "重症"  ~ "2. Severe",
    sevirity == "危重症" ~ "3. Critical"
  ),
  
  outcome_group = case_when(
    discharge_method == "治愈" | discharge_method == "其他" | discharge_method == "好转" ~ "1. Discharge",
    is.na(discharge_method) ~ "2. remained in hospital",
    discharge_method == "死亡" ~ "3. death",
    TRUE ~ "5. Others"
    
  ),
  hospotal_period = as.numeric(difftime(out_date ,enter_date , units = c("days")) ),
  hospotal_period1 = round(as.numeric(difftime(out_date ,enter_date , units = c("days")) ) , digits=0)
  
) %>% mutate(outcome_group = if_else (
  outcome_group == "5. Others", "1. Recovery", outcome_group))

## delete duplication
desc1 <- desc_ %>% group_by(id) %>% arrange(record_date ) %>%  mutate(ord = row_number()) %>% filter(ord == 1) %>% 
  filter(!is.na(sevirity)) %>% filter(sevirity != "非肺炎患者")


# table(desc1$outcome_group)

################################################################################################################
#### PART 1 comparison between male and female
#### 
################################################################################################################


##age##########################################################################
age <- sqldf("select gender, age_group,count(*)  as count
                        from desc1 group by gender, age_group  " ) %>% 
  dcast( age_group  ~ gender) %>% mutate(total = Male + Female)
## get the total volumn
sum <- sum(age$total)

pvalue <- chisq.test(table(desc1$gender, desc1$age_group))

age <- age %>% mutate(Male= paste(Male, "(", round(Male/sum(Male)*100, digits = 2) , "%)" ),
                      Female= paste(Female, "(", round(Female/sum(Female)*100, digits = 2) , "%)") ,
                      p_value = if_else (row_number() == 1,  paste(pvalue[3] )   , "")
) %>% mutate(p_value = as.numeric(p_value)) %>%  mutate(variable = "age") %>% rename(group = age_group ) %>% 
  mutate(p_value = if_else(p_value   < 0.01, "<0.01", paste(round(p_value, digits = 2 ))) )

##nation##########################################################################
nation <- sqldf("select gender, nation_group,count(*)  as count
                        from desc1 group by gender, nation_group  " ) %>% 
  dcast( nation_group  ~ gender) %>% mutate(total = Male + Female)
## get the total volumn
sum <- sum(nation$total)

pvalue <- chisq.test(table(desc1$gender, desc1$nation_group))

nation <- nation %>% mutate(Male= paste(Male, "(", round(Male/sum(Male)*100, digits = 2) , "%)" ),
                      Female= paste(Female, "(", round(Female/sum(Female)*100, digits = 2) , "%)") ,
                      p_value = if_else (row_number() == 1,  paste(pvalue[3] )   , "")
) %>% mutate(p_value = as.numeric(p_value)) %>% mutate(variable = "nation") %>% rename(group = nation_group ) %>% 
  mutate(p_value = if_else(p_value   < 0.01, "<0.01", paste(round(p_value, digits = 2 ))) )



##sevirity##########################################################################
sevirity <- sqldf("select gender, sevirity_group,count(*)  as count
                        from desc1 group by gender, sevirity_group  " ) %>% filter(!is.na(sevirity_group) ) %>% 
  dcast( sevirity_group  ~ gender) %>% mutate(total = Male + Female)
## get the total volumn
sum <- sum(sevirity$total)

pvalue <- chisq.test(table(desc1$gender, desc1$sevirity_group))

sevirity <- sevirity %>% mutate(Male= paste(Male, "(", round(Male/sum(Male)*100, digits = 2) , "%)" ),
                      Female= paste(Female, "(", round(Female/sum(Female)*100, digits = 2) , "%)") ,
                      p_value = if_else (row_number() == 1,  paste(pvalue[3] )   , "")) %>% 
  mutate(p_value = as.numeric(p_value)) %>% mutate(variable = "sevirity") %>% rename(group = sevirity_group ) %>% 
  mutate(p_value = if_else(p_value   < 0.01, "<0.01", paste(round(p_value, digits = 2 ))) )


##outcome_group##########################################################################
outcome <- sqldf("select gender, outcome_group,count(*)  as count
                  from desc1 group by gender, outcome_group  " ) %>% filter(!is.na(outcome_group) ) %>% 
  dcast( outcome_group  ~ gender) %>% mutate(total = Male + Female)
## get the total volumn
sum <- sum(outcome$total)

pvalue <- chisq.test(table(desc1$gender, desc1$outcome_group))

outcome <- outcome %>% mutate(Male= paste(Male, "(", round(Male/sum(Male)*100, digits = 2) , "%)" ),
                                Female= paste(Female, "(", round(Female/sum(Female)*100, digits = 2) , "%)") ,
                                p_value = if_else (row_number() == 1,  paste(pvalue[3] )   , "")
) %>% mutate(p_value = as.numeric(p_value)) %>% mutate(variable = "outcome") %>% rename(group = outcome_group ) %>% 
  mutate(p_value = if_else(p_value   < 0.01, "<0.01", paste(round(p_value, digits = 2 ))) )

# library(sqldf)
# cross_table <- function(var, out_df) {
# 
#   var <<- sqldf(sprintf("select gender, %s ,count(*)  as count from desc1 group by gender, %s ", var, var  ))
#   
#  }
# out_df <<- tem1
# cross_table("age_group", tem2)
# 
# var <- "age_group"
# 
# %>% 
#   dcast( age_group  ~ gender) %>% mutate(total = Male + Female)
# ## get the total volumn
# sum <- sum(age$total)
# 
# pvalue <- chisq.test(table(desc1$gender, desc1$age_group))
# 
# age <- age %>% mutate(Male= paste(Male, "(", round(Male/sum*100, digits = 2) , "%)" ),
#                       Female= paste(Female, "(", round(Female/sum*100, digits = 2) , "%)") ,
#                       p_value = if_else (row_number() == 1,  paste(pvalue[3] )   , "")
# ) %>% mutate(p_value = as.numeric(p_value)) %>% mutate(p_value = if_else(p_value   < 0.01, "<0.01", paste(round(p_value, digits = 2 ))) )
# 
# }

############################################################################
## t-test for the days in hospital in different gender group
a<- desc1$hospotal_period[desc1$gender == "Male"]
b<- desc1$hospotal_period[desc1$gender == "Female"]
pvalue<-  t.test(a,b)

period <- desc1 %>% filter(!is.na(hospotal_period)) %>% 
  group_by(gender) %>% 
  summarize(mean = mean(hospotal_period),
            sd = sd(hospotal_period)
           ) %>% mutate(value = paste(round(mean, digits=2) , "±" , round(sd, digits = 2) ) ) %>% dplyr::select(gender, value) %>% 
# melt(id.vars ="gender" ,  value.name =  "value")
dcast( "period" ~ gender, value.var = "value") %>% mutate(group = "period")
names(period) <- c("variable", "Female" , "Male", "group") 
period <- period %>% mutate(p_value = if_else (row_number() == 1,  paste(pvalue[3] )   , "")) %>% 
  mutate(p_value = as.numeric(p_value)) %>% mutate(variable = "days in hospital")  %>% 
  mutate(p_value = if_else(p_value   < 0.01, "<0.01", paste(round(p_value, digits = 2 ))) )




period_sum <- desc1 %>% filter(!is.na(hospotal_period)) %>% mutate(aa = 1) %>% 
  group_by(aa) %>% 
  summarize(mean = mean(hospotal_period),
            sd = sd(hospotal_period)
  )%>%
  mutate(total = paste(round(mean, digits=2) , "±" , round(sd, digits = 2) ) ) %>% dplyr::select(total)

period <- cbind(period, period_sum)


############################################################################
## t-test for the age in different gender group
a<- desc1$age1[desc1$gender == "Male"]
b<- desc1$age1[desc1$gender == "Female"]
pvalue<-  t.test(a,b)

age1 <- desc1 %>% filter(!is.na(age1)) %>% 
  group_by(gender) %>% 
  summarize(mean = mean(age1),
            sd = sd(age1)
  ) %>% mutate(value = paste(round(mean, digits=2) , "±" , round(sd, digits = 2) ) ) %>% dplyr::select(gender, value) %>% 
  # melt(id.vars ="gender" ,  value.name =  "value")
  dcast( "age1" ~ gender, value.var = "value") %>% mutate(group = "age mean/sd")
names(age1) <- c("variable", "Female" , "Male", "group") 
age1 <- age1 %>% mutate(p_value = if_else (row_number() == 1,  paste(pvalue[3] )   , "")) %>% 
  mutate(p_value = as.numeric(p_value)) %>% mutate(variable = "age (mean/sd)")  %>% 
  mutate(p_value = if_else(p_value   < 0.01, "<0.01", paste(round(p_value, digits = 2 ))) )


age1_sum <- desc1 %>% filter(!is.na(age1)) %>% mutate(aa = 1) %>% 
  group_by(aa) %>% 
  summarize(mean = mean(age1),
            sd = sd(age1)
  )%>%
  mutate(total = paste(round(mean, digits=2) , "±" , round(sd, digits = 2) ) ) %>% dplyr::select(total)

age1 <- cbind(age1, age1_sum) %>% dplyr::select(variable, group, total,Male, Female,p_value)
age1 <- age1 %>% dplyr::select(variable, group, total,Male, Female,p_value)
############################################################################


pvalue_ <- wilcox.test(age1  ~ gender,data=desc1)
pvalue_ <- pvalue_$p.value 

age2 <- desc1 %>% filter(!is.na(age1)) %>% 
  group_by(gender) %>% 
  summarize(median = median(age1),
            p25=quantile(age1, probs=0.25),
            median=quantile(age1, probs=0.5),
            p75 =quantile(age1, probs=0.75)
  ) %>% mutate(value = paste(median , " (" , p25, " - ", p75, " )", sep="" ) ) %>% dplyr::select(gender, value) %>% 
  # melt(id.vars ="gender" ,  value.name =  "value")
  dcast( "age2" ~ gender, value.var = "value") %>% mutate(group = "age median")
names(age2) <- c("variable", "Female" , "Male", "group") 
age2 <- age2 %>% mutate(p_value = if_else (row_number() == 1,  paste(pvalue_) , "")) %>% 
  mutate(p_value = as.numeric(p_value)) %>% mutate(variable = "age median")  %>% 
  mutate(p_value = if_else(p_value   < 0.01, "<0.01", paste(round(p_value, digits = 2 ))) )

age2_sum <- desc1 %>% filter(!is.na(age1)) %>% mutate(aa = 1) %>% group_by(aa) %>% 
  summarize(median = median(age1),
            p25=quantile(age1, probs=0.25),
            median=quantile(age1, probs=0.5),
            p75 =quantile(age1, probs=0.75)
  ) %>% mutate(total = paste(median , " (" , p25, " - ", p75, " )", sep="" ) ) %>% dplyr::select(total)

age2 <- cbind(age2, age2_sum) %>% dplyr::select(variable, group, total,Male, Female,p_value)

table1 <- rbind(age,age1,age2, nation, sevirity , period, outcome) %>% dplyr::select(variable, group, total,Male, Female,p_value)


################################################################################################################
#### PART 2 outcome & severity breakdown by age group for female 
#### 
################################################################################################################

desc1_female <- desc1 %>% filter(gender == 'Female')

age_female <- sqldf("select sevirity_group, age_group1,count(*)  as count from desc1_female
                    group by  sevirity_group, age_group1  " ) %>%   filter(!is.na(sevirity_group) ) %>%
  dcast( sevirity_group ~ age_group1 )
names(age_female) <- c("group" , "age_less_50", "age_large_50" )
## get the total volumn
pvalue <- chisq.test(table(desc1_female$sevirity_group, desc1_female$age_group1))

sum <- age_female %>% mutate(group ="total", age_less_50 = paste(sum(age_less_50) ),
                    age_large_50= paste(sum(age_large_50)),  variable = "sevirity - Female", p_value =NA ) %>% distinct()

age_female <- age_female %>% 
  mutate(age_less_50= paste(age_less_50, "(", round(age_less_50/sum(age_less_50) *100, digits = 2) , "%)" ),
         age_large_50 = paste(age_large_50, "(", round(age_large_50/sum(age_large_50) *100, digits = 2) , "%)" ),
         p_value = if_else (row_number() == 1,  paste(pvalue[3] )   , "") ) %>%
  mutate(p_value = as.numeric(p_value)) %>%  mutate(variable = "sevirity - Female") %>% 
  mutate(p_value = if_else(p_value   < 0.01, "<0.01", paste(round(p_value, digits = 2 ))) )

age_female <- rbind(age_female, sum)
##outcome_group - female##########################################################################
outcome_female <- sqldf("select age_group1, outcome_group,count(*)  as count
                        from desc1_female group by age_group1, outcome_group  " ) %>% filter(!is.na(outcome_group) ) %>% 
  dcast( outcome_group ~ age_group1)
names(outcome_female) <- c("group" , "age_less_50", "age_large_50" )
# pvalue <- chisq.test(table(desc1_female$age_group1, desc1_female$outcome_group))

pvalue <- fisher.test(table(desc1_female$age_group1, desc1_female$outcome_group))


outcome_female$age_less_50[is.na(outcome_female$age_less_50)] <- 0
sum <- outcome_female %>% mutate(group ="total", age_less_50 = paste(sum(age_less_50) ),
                             age_large_50= paste(sum(age_large_50)),  variable = "outcome_Female", p_value =NA ) %>% distinct()

outcome_female <- outcome_female %>% 
  mutate(age_less_50= paste(age_less_50, "(", round(age_less_50/sum(age_less_50) *100, digits = 2) , "%)" ),
         age_large_50 = paste(age_large_50, "(", round(age_large_50/sum(age_large_50) *100, digits = 2) , "%)" ),
         p_value = if_else (row_number() == 1,  paste(pvalue[3] )   , "") ) %>%
  mutate(p_value = as.numeric(p_value)) %>% mutate(variable = "outcome_Female") %>%  
  mutate(p_value = if_else(p_value   < 0.01, "<0.01", paste(round(p_value, digits = 2 ))) )

outcome_female <- rbind(outcome_female, sum)

female_base <- rbind(age_female, outcome_female) %>% select(variable, group ,age_less_50, age_large_50,p_value)


title1 <- "comparison between male and female"
title2 <- "outcome & severity breakdown by age group for female "
wb <- createWorkbook("desc")

openxlsx::addWorksheet(wb, "desc") 
openxlsx::writeData(wb, sheet="desc", title1, startCol = 2, startRow = 1)
openxlsx::writeData(wb, sheet="desc", table1, startCol = 2, startRow = 2)

openxlsx::writeData(wb, sheet="desc", title2, startCol = 2, startRow = 24)
openxlsx::writeData(wb, sheet="desc", female_base, startCol = 2, startRow = 25)

saveWorkbook(wb, "output/table1v7.xlsx", overwrite = TRUE)








# 
# 
# fig1a <- sqldf("select gender,outcome_group, count(*) as count from desc1 group by gender,outcome_group ")
# fig1a <- fig1a %>% group_by(gender) %>% mutate(pct= round(count / sum(count),2))
# 
# a <- 
# ggplot(data=fig1a, aes(x=outcome_group , y=pct*100, fill=gender)) +
#   geom_bar(stat="identity", position=position_dodge())+
#   geom_text(aes(label=paste0(round(pct*100), "%")), vjust=0.9, color="white",
#             position = position_dodge(0.9), size=4)+
# scale_x_discrete(labels=c("1. Recovery" = "Recovery", "2. Improve" = "Improved",
#                           "3. remained in hospital" = "Remained in hospital", "4. Critcal" = "Critcal" )) + 
#   # scale_fill_brewer(palette="Paired")+
#   # scale_fill_manual(values=c("gray" ,'black'))+
#   # scale_fill_manual(values= c("#ff6361", "#2E9FDF")) +
#   scale_fill_grey() +
#   theme_bw() +
#   theme(legend.position=c(0.10, 0.80) ,text = element_text(size=15)) +
#     xlab("Outcomes") +
#     ylab("%")+
#   labs(fill = "Gender")



