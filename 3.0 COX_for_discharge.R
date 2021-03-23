
library(readxl)
library(dplyr)
library(sqldf)
library(survival)
library(survminer)
library(openxlsx)

covid_dis <- read_excel( paste0(wd,  "/input/data10.xls" ), "data10") %>% mutate(age_group = case_when(
  age >0 & age <=40 ~ 1,
  age > 40 & age <=60 ~ 2,
  age > 60 ~ 3
),
age_group1 = case_when(
  age >0 & age <=50 ~ 1,
  age > 50 ~ 2
  
  
) ) %>% mutate(severity2 = severity1 - 1 ) %>% filter(!is.na(regularity) )
covid_dis <- covid %>% mutate(severity_char = if_else(severity1 ==1, "Non-severe", "severe")) 
# select(id, age, age1, age_group,amh1, age_group1, severity1, severity2, admissiondate,	curedate,	deathdate,
#        regularity,regularity1, outcome,coexsiting) %>% mutate(admission_date = as.Date(admissiondate) , 
#        death_date = as.Date(deathdate),recovery_date = as.Date(curedate))


# covid %>% mutate(regularity1 = if_else(regularity >=2 , 2, 1))
  
hist(covid_dis$age)
## import survival data
suv<-  read_excel( paste0(wd,  "/input/time.xlsx" ), "Sheet1")
names(suv) <- c("id", "name", "admission_date", "death_date", "recovery_date") 
suv <- suv %>% mutate(admission_date = as.Date(admission_date) , death_date = as.Date(death_date),
                      recovery_date = as.Date(recovery_date)) %>%
  mutate(recovery_date = if_else(id == 2002283247, as.Date("2020-02-21"), recovery_date)  )

aa1 <- suv %>% filter(id ==2002283247 )
suvival <- sqldf("
                 select a.id, b.outcome, b.severity1,b.amh1,b.amh,b.il6, b.e2,
case when b.il6 <7 then 0 when b.il6 >=7 then 1 end as il6_grp,
                  b.coexsiting, b.age, b.age1, b.age_group,b.age_group1,
                    case when b.regularity >=3 then 3 else b.regularity end as regularity,
                  case when b.regularity >=2 then 2 else b.regularity end as regularity1,
                 case when b.regularity in (1,2) then 1 when b.regularity >2 then 2  end as regularity2,
                  a.admission_date, a.death_date, a.recovery_date,
                  case when a.recovery_date is not null then 2 else 1 end as status,
                case when a.recovery_date is not null and a.admission_date is not null
                then a.recovery_date - a.admission_date end as time

                from suv  a 
                 left join covid_dis b on a.id = b.id
                 ") %>% filter(!(age == 47 & id ==2501786558 )) %>% 
  mutate(time = if_else(is.na(time), as.Date('2020-03-08') - admission_date, time ) ) %>% 
  mutate(time = as.numeric(time, units="days")) 

suvival <- suvival %>% mutate( status = ifelse(time >40, 1, status), time = ifelse(time >40, 40, time ) )


# 
# %>%  filter(time <=40) #%>% filter(time >0 )
# group_by(id) %>%  mutate(ord = row_number()) 
# 
# aa <- suvival %>% filter(time >40)
# suvival <- suvival %>% filter(ord==1)

# suvival$SurvObj <- with(suvival, Surv(time, status == 2))
# 
# 
# suvival$regularity <- as.factor(suvival$regularity)

# aa <- suvival %>% filter(time < 0)
# aa <- suv %>% filter(id == 2002283247)
windowsFonts(Times=windowsFont("Arial"))
# res.cox <- coxph(Surv(time,status ) ~ age_group1  + regularity1 + severity1 , data =  suvival)
# summary(res.cox)
suvival_COX <- suvival %>% select(time,status,age_group1, coexsiting , regularity1 , severity1,death_date) %>% 
  rename(Age = age_group1, Comorbidities = coexsiting, Menstruation =regularity1, Severity =severity1)

aa <- suvival_COX %>% filter( !is.na(Age) & !is.na(Comorbidities) & !is.na(Menstruation) & !is.na(Severity)) 
aa1 <- aa %>% filter(!is.na(death_date))

suvival_COX$SurvObj <- with(suvival_COX, Surv(time, status == 2))

res.cox <- coxph(Surv(time,status ) ~ Age + Comorbidities + Menstruation + Severity , data =  suvival_COX)
 aa <- summary(res.cox)
 
 options(OutDec="\xB7")
 
 cox_forest <- ggforest(res.cox, noDigits=2, fontsize = 0.7)
 cox_forest
 tiff("output/FIG6_forest.tiff", units="in", width=6, height=3, res=300)
 cox_forest
 dev.off()
 
 
 pdf(file = "output/FIG6_forest.pdf",   # The directory you want to save the file in
     width = 6, # The width of the plot in inches
     height = 3) # The height of the plot in inches
 
 cox_forest
 
 dev.off()
 # setEPS()
 # setEPS(bg = "white",family = "Times", width = 6)
 # postscript("output/FIG6_forest.eps")
 # cox_forest
 # dev.off()
 # 
 # ggsave("output/FIG6_forest.eps", width = 12, height = 6,  units = "in", dpi = 300 ) + theme_bw(base_size = 10)
 # 
 

p <- as.data.frame (coef(summary(res.cox))[,5] )
ci <- as.data.frame (aa$conf.int )
cox_result<- cbind(ci, p) 
names(cox_result) <- c("value", "_value", "low", "upper", "p")

cox_result <- cox_result %>% mutate( value = paste( round(value, digits = 2), 
                                          "(", round(low, digits = 2), "-", round(upper, digits = 2), ")" ) ,
                  p_value = if_else(p < 0.00001, "<0.00001", paste(round(p, digits = 6 )) )  )


var <- as.data.frame(c("Age","Comorbidity" ,"Regularity","Severity" ), nrow(4))
names(var) <- "varables"

cox_result <- cbind(var,cox_result ) %>% select(varables, value , p_value)



###############################################################################

covariates <- c("age_group", "coexsiting",  "regularity1", "severity1", "il6_grp", "amh1", "age_group1", "e2")
univ_formulas <- sapply(covariates,
                        function(x) as.formula(paste('Surv(time, status)~', x)))

univ_models <- lapply( univ_formulas, function(x){coxph(x, data = suvival)})
# Extract data 
univ_results <- lapply(univ_models,
                       function(x){ 
                         var <- paste(x)
                         x <- summary(x)
                         p.value<-signif(x$wald["pvalue"], digits=6)
                         wald.test<-signif(x$wald["test"], digits=6)
                         beta<-signif(x$coef[1], digits=2);#coeficient beta
                         HR <-signif(x$coef[2], digits=2);#exp(beta)
                         HR.confint.lower <- signif(x$conf.int[,"lower .95"], 2)
                         HR.confint.upper <- signif(x$conf.int[,"upper .95"],2)
                         HR <- paste0(HR, " (", 
                                      HR.confint.lower, "-", HR.confint.upper, ")")
                         res<-c(beta, HR, wald.test, p.value)
                         names(res)<-c("beta", "HR (95% CI for HR)", "wald.test", 
                                       "p.value")
                         return(res)
                         #return(exp(cbind(coef(x),confint(x))))
                       })
res <- t(as.data.frame(univ_results, check.names = FALSE))
cox_univariate <- as.data.frame(res) 

name <- as.data.frame(covariates) 
names(name) <- "variable" 
cox_univariate <- cbind(name, cox_univariate)


cox_title1 <- "Univariate cox regression"
cox_title2 <- "multivariate cox regression"

wb <- createWorkbook("COX_reg")

openxlsx::addWorksheet(wb, "cox") 
openxlsx::writeData(wb, sheet="cox", cox_title1, startCol = 1, startRow = 1)
openxlsx::writeData(wb, sheet="cox", cox_univariate, startCol = 2, startRow = 2)

openxlsx::writeData(wb, sheet="cox", cox_title2, startCol = 2, startRow = 13)
openxlsx::writeData(wb, sheet="cox", cox_result, startCol = 2, startRow = 14)

saveWorkbook(wb, "output/cox1_v4.xlsx", overwrite = TRUE)


# aa <- as.data.frame(res.cox)
# pvc <- coef(summary(res.cox))[,5]
# 
# 
# hr <- round(coef(summary(res.cox))[,2],3)
# 
# round(exp.coef.lci(summary(res.cox))[,2],3)

# summary(res.cox,times=c(10,20,30))

# res <- summary(res.cox)
# save.df <- as.data.frame(res[c("strata", "lower", "upper")])
# write.csv(save.df, file = "./file.csv")


# res.cox <- coxph(Surv(time,status ) ~ age_group1 + coexsiting + regularity1 + severity1 , data =  suvival)
# summary(res.cox)
# 
#  suvival %>% filter(!is.na(coexsiting)) %>% count()
# 
# suvival2 <- suvival1 %>% 
# res.cox <- coxph(Surv(time,status ) ~ age  + regularity1 + severity1  +  coexsiting   , data =  suvival1)
# summary(res.cox) 

# table(covid$regularity)
# splots[[4]] <- ggforest(res.cox)
# 
# ggsave("output/FIG6_forest.png", cox_forest ,width = 14 , height = 10, units='cm')
# 
# 
# ggsave("output/FIG5_all.png", arrangeGrob(Regularity, Regularity) ,width = 20 , height = 20, units='cm')
# 





# res.cox <- coxph(Surv(time,status ) ~  il6_grp, data =  suvival)
# summary(res.cox)
# 
# mylogit <- glm(severity1 ~ age_group + regularity1 , data = huizong2, family = "binomial")
# summary(mylogit)
# 
# 
# 
# aa <- suvival %>% filter(!is.na(il6) )

