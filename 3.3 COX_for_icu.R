icu <- read_excel( paste0(wd,  "/input/data16.xls" ), "data16") %>% mutate(age_group = case_when(
  age >0 & age <=40 ~ 1,
  age > 40 & age <=60 ~ 2,
  age > 60 ~ 3
),
age_group1 = case_when(
  age >0 & age <=50 ~ 1,
  age > 50 ~ 2
) )
icu <- icu %>% mutate(severity_char = if_else(severity1 ==1, "Non-severe", "severe")) 
# select(id, age, age1, age_group,amh1, age_group1, severity1, severity2, admissiondate,	curedate,	deathdate,
#        regularity,regularity1, outcome,coexsiting) %>% mutate(admission_date = as.Date(admissiondate) , 
#        death_date = as.Date(deathdate),recovery_date = as.Date(curedate))

icu_symptom <- read_excel( paste0(wd,  "/input/first_symptom_addition.xlsx" ), "Sheet1")
names(icu_symptom) <- c("id", "name","age", "first_symptom_date")



icu_Ventilation <- read_excel( paste0(wd,  "/input/Ventilation_summary.xlsx" ), "Sheet1") 
icu_Ventilation <- icu_Ventilation[,c(1,2,3,4)]
names(icu_Ventilation) <- c("id", "name","age", "oxygen_method")

icu_Ventilation_add <- read_excel( paste0(wd,  "/input/Ventilation_addition.xlsx" ), "Sheet1")
names(icu_Ventilation_add) <- c("id", "name","age", "oxygen_method")


icu1 <- sqldf("
                 select a.id, a.age,a.oxygen_method, b.outcome,  b.icu_time, pce_tijeinterval,pce,
                  b.severity1,b.amh1,b.amh,b.il6, b.e2, b.il10,il2,il8,il6,c4,
                  case when b.il6 <7 and b.il6 >=0 then 0 when b.il6 >=7 then 1 end as il6_grp,
case when b.il8 < 62 and b.il8 >=0 then 0 when b.il8 >=62 then 1 end as il8_grp,
case when b.il10 < 9.1 and b.il10 >= 0 then 0 when b.il10 >=9.1 then 1 end as il10_grp,
case when b.il2 < 710 and b.il2 >223 then 0 when b.il2 > 0 and (b.il2>=710 or b.il2<=223)  then 1 end as il2_grp,
                  b.coexsiting, b.age as age_10, b.age1, b.age_group,b.age_group1,
                  case when b.regularity >=2 then 2 else b.regularity end as regularity1,
                c.first_symptom_date, b.vantilatio_time
                
                from icu_Ventilation  a 
                 left join icu b on a.id = b.id
left join icu_symptom c on a.id =c.id
                 ") %>%  mutate(flag = if_else( (age_10 == 47 & id ==2501786558 ) , 1, 0)) %>% 
  filter(flag ==0 |is.na(flag)) %>% 
  mutate(first_symptom_date = as.Date(first_symptom_date) , vantilatio_time = as.Date(vantilatio_time), 
         icu_time= as.Date(icu_time) ) 

icu2 <- icu1 %>% filter(!is.na(first_symptom_date)) %>% 
  mutate(time = if_else(is.na(vantilatio_time), as.Date('2020-03-08') - first_symptom_date, 
                        vantilatio_time - first_symptom_date ),
         status = if_else(pce == 1, 2, 1)) 

icu2 <-icu2 %>%  
  mutate(log_il2 = log(il2), log_il6 = log(il6), log_il8 = log(il8), 
         log_il10 =log(il10), log_e2 =log(e2) , log_c4 = log(c4) )


icu_COX <- icu2 %>% select(time,status,pce_tijeinterval,amh1, pce  ,age_group1, amh,
                           log_il2,log_il6,log_il8, log_il10,log_e2,
                           coexsiting , regularity1 , severity1, il2_grp, il6_grp, il8_grp, il10_grp, il10) %>% 
  rename(Age = age_group1, Coexsiting = coexsiting, Regularity =regularity1, 
         Severity =severity1,  IL2R = il2_grp, IL6 = il6_grp, IL8 = il8_grp, IL10 = il10_grp)



plot(icu2$il2, icu2$il6)
plot(icu2$il2, icu2$il8)
plot(icu2$il2, icu2$il10)
plot(icu2$il6, icu2$il8)
plot(icu2$il6, icu2$il10)
plot(icu2$il8, icu2$il10)
icu3<- icu2 %>% mutate(log_il2 = log(il2), log_il6 = log(il6), log_il8 = log(il8))
# 
# scatter <- function(var1, var2, lab1, lab2) {
# ggscatter(icu2, x = var1, y = var2, add = "reg.line") +
#   stat_cor(
#     aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~")),
#     label.x = 3
#   ) + 
#   theme_bw() +
#   labs(title=sprintf( "Correlation between %s and %s",lab1,lab2 ),
#        x = var1,
#        y = var2)
# }
# 
# scatter("il2", "il6", "IL2", "IL6")
# il2_8 <- scatter("il6", "il8","IL6", "il8")
# il2_10 <- scatter("il2", "il10")




res.cox <- coxph(Surv(pce_tijeinterval,pce ) ~ Severity + IL6 + IL10   , data =  icu_COX)
summary(res.cox)
aa <- summary(res.cox)


##################################################################################
## forest plot
cox_forest <- ggforest(res.cox)

tiff("output/FIG7_forest.tiff", units="in", width=6, height=3, res=300)
cox_forest
dev.off()



p <- as.data.frame (coef(summary(res.cox))[,5] )
ci <- as.data.frame (aa$conf.int )
cox_result<- cbind(ci, p) 
names(cox_result) <- c("value", "_value", "low", "upper", "p")

cox_result <- cox_result %>% mutate( value = paste( round(value, digits = 2), 
                                                    "(", round(low, digits = 2), "-", round(upper, digits = 2), ")" ) ,
                                     p_value = if_else(p < 0.01, "<0.01", paste(round(p, digits = 2 )) )  )


var <- as.data.frame(c("Severity","IL6" ,"IL10"), nrow(4))
names(var) <- "varables"

cox_result <- cbind(var,cox_result ) %>% select(varables, value , p_value)


###############################################################################

covariates <- c("Age", "Coexsiting",  "Regularity", "Severity", "IL2R", "IL6","IL8",  "IL10")
univ_formulas <- sapply(covariates,
                        function(x) as.formula(paste('Surv(pce_tijeinterval,pce)~', x)))

univ_models <- lapply( univ_formulas, function(x){coxph(x, data = icu_COX)})
# Extract data 
univ_results <- lapply(univ_models,
                       function(x){ 
                         var <- paste(x)
                         x <- summary(x)
                         p.value<-signif(x$wald["pvalue"], digits=2)
                         wald.test<-signif(x$wald["test"], digits=2)
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

wb <- createWorkbook("COX_icu")

openxlsx::addWorksheet(wb, "cox") 
openxlsx::writeData(wb, sheet="cox", cox_title1, startCol = 1, startRow = 1)
openxlsx::writeData(wb, sheet="cox", cox_univariate, startCol = 2, startRow = 2)

openxlsx::writeData(wb, sheet="cox", cox_title2, startCol = 2, startRow = 13)
openxlsx::writeData(wb, sheet="cox", cox_result, startCol = 2, startRow = 14)

saveWorkbook(wb, "output/cox_ventilation_v2.xlsx", overwrite = TRUE)


###################################################################################################
# K - M - AGE
###################################################################################################

        fit <- survfit(Surv(pce_tijeinterval,pce ) ~ Age, data = icu_COX)
FIG7_age <- ggsurvplot(
          # splots[[1]] <- ggsurvplot(
          fit,
          data = icu_COX,
          ylab="Probability of hospitalization",
          xlab = "Days",
          xlim = c(0,40),
          size = 1,                 # change line size
          palette =
            c("#E7B800", "#2E9FDF"),# custom color palettes
          conf.int = FALSE,          # Add confidence interval
          pval = TRUE,              # Add p-value
          risk.table = FALSE,        # Add risk table
          risk.table.col = "age_group",# Risk table color by groups
          legend.labs =
            c("0 ~ 50 yrs", "51 ~ yrs"),    # Change legend labels
          risk.table.height = 0.25, # Useful to change when you have multiple groups
          ggtheme = theme_bw()      # Change ggplot2 theme
        ) +  guides(
          fill = guide_legend(title = 'Age Group'),
          color = guide_legend(title = 'Age Group')
        )
         
         
         tiff("output/FIG7_age.tiff", units="in", width=6, height=3, res=300)
         FIG7_age
         dev.off()
         
         # K - M - IL6
         ###################################################################################################
         fit <- survfit(Surv(pce_tijeinterval,pce ) ~ IL6, data = icu_COX)
         FIG7_IL6 <- ggsurvplot(
           # splots[[1]] <- ggsurvplot(
           fit,
           data = icu_COX,
           ylab="Probability of no need to Venilation",
           xlab = "Days",
           xlim = c(0,40),
           size = 1,                 # change line size
           palette =
             c("#E7B800", "#2E9FDF"),# custom color palettes
           conf.int = FALSE,          # Add confidence interval
           pval = TRUE,              # Add p-value
           risk.table = FALSE,        # Add risk table
           risk.table.col = "age_group",# Risk table color by groups
           legend.labs =
             c("", "51 ~ yrs"),    # Change legend labels
           risk.table.height = 0.25, # Useful to change when you have multiple groups
           ggtheme = theme_bw()      # Change ggplot2 theme
         ) +  guides(
           fill = guide_legend(title = 'IL6 Group'),
           color = guide_legend(title = 'IL6 Group')
         )
         
         tiff("output/FIG7_IL6.tiff", units="in", width=6, height=3, res=300)
         FIG7_IL6
         dev.off()
         ## il8
         fit <- survfit(Surv(pce_tijeinterval,pce ) ~ IL8, data = icu_COX)
         FIG7_IL8 <- ggsurvplot(
           # splots[[1]] <- ggsurvplot(
           fit,
           data = icu_COX,
           ylab="Probability of no need to Venilation",
           xlab = "Days",
           xlim = c(0,40),
           size = 1,                 # change line size
           palette =
             c("#E7B800", "#2E9FDF"),# custom color palettes
           conf.int = FALSE,          # Add confidence interval
           pval = TRUE,              # Add p-value
           risk.table = FALSE,        # Add risk table
           risk.table.col = "age_group",# Risk table color by groups
           legend.labs =
             c("Normal", "Abnormal"),    # Change legend labels
           risk.table.height = 0.25, # Useful to change when you have multiple groups
           ggtheme = theme_bw()      # Change ggplot2 theme
         ) +  guides(
           fill = guide_legend(title = 'IL8 Group'),
           color = guide_legend(title = 'IL8 Group')
         )
         
         tiff("output/FIG7_IL8.tiff", units="in", width=6, height=3, res=300)
         FIG7_IL8
         dev.off()
         
         ## il10
         
         fit <- survfit(Surv(pce_tijeinterval,pce ) ~ IL10, data = icu_COX)
         FIG7_IL10 <- ggsurvplot(
           # splots[[1]] <- ggsurvplot(
           fit,
           data = icu_COX,
           ylab="Probability of no need to Venilation",
           xlab = "Days",
           xlim = c(0,40),
           size = 1,                 # change line size
           palette =
             c("#E7B800", "#2E9FDF"),# custom color palettes
           conf.int = FALSE,          # Add confidence interval
           pval = TRUE,              # Add p-value
           risk.table = FALSE,        # Add risk table
           risk.table.col = "IL10",# Risk table color by groups
           legend.labs =
             c("Normal", "Abnormal"),    # Change legend labels
           risk.table.height = 0.25, # Useful to change when you have multiple groups
           ggtheme = theme_bw()      # Change ggplot2 theme
         ) +  guides(
           fill = guide_legend(title = 'IL10 Group'),
           color = guide_legend(title = 'IL10 Group')
         )
         
         tiff("output/FIG7_IL10.tiff", units="in", width=6, height=3, res=300)
         FIG7_IL10
         dev.off()
      #IL2R
         
         fit <- survfit(Surv(pce_tijeinterval,pce ) ~ IL2R, data = icu_COX)
         FIG7_IL2R<- ggsurvplot(
           # splots[[1]] <- ggsurvplot(
           fit,
           data = icu_COX,
           ylab="Probability of no need to Venilation",
           xlab = "Days",
           xlim = c(0,40),
           size = 1,                 # change line size
           palette =
             c("#E7B800", "#2E9FDF"),# custom color palettes
           conf.int = FALSE,          # Add confidence interval
           pval = TRUE,              # Add p-value
           risk.table = FALSE,        # Add risk table
           risk.table.col = "IL2R",# Risk table color by groups
           legend.labs =
             c("Normal", "Abnormal"),    # Change legend labels
           risk.table.height = 0.25, # Useful to change when you have multiple groups
           ggtheme = theme_bw()      # Change ggplot2 theme
         ) +  guides(
           fill = guide_legend(title = 'IL2R Group'),
           color = guide_legend(title = 'IL2R Group')
         )
         
         tiff("output/FIG7_IL2R.tiff", units="in", width=6, height=3, res=300)
         FIG7_IL2R
         dev.off()
         
         # SEVIRITY
         fit <- survfit(Surv(pce_tijeinterval,pce ) ~ Severity, data = icu_COX)
         FIG7_Severity<- ggsurvplot(
           # splots[[1]] <- ggsurvplot(
           fit,
           data = icu_COX,
           ylab="Probability of no need to Venilation",
           xlab = "Days",
           xlim = c(0,40),
           size = 1,                 # change line size
           palette =
             c("#E7B800", "#2E9FDF"),# custom color palettes
           conf.int = FALSE,          # Add confidence interval
           pval = TRUE,              # Add p-value
           risk.table = FALSE,        # Add risk table
           risk.table.col = "age_group",# Risk table color by groups
           legend.labs =
             c("Non-Severe", "Severe"),    # Change legend labels
           risk.table.height = 0.25, # Useful to change when you have multiple groups
           ggtheme = theme_bw()      # Change ggplot2 theme
         ) +  guides(
           fill = guide_legend(title = 'Severity Group'),
           color = guide_legend(title = 'Severity Group')
         )
         
         tiff("output/FIG7_Severity.tiff", units="in", width=6, height=3, res=300)
         FIG7_Severity
         dev.off()
         
         fit <- survfit(Surv(pce_tijeinterval,pce ) ~ Regularity, data = icu_COX)
         FIG7_Regularity<- ggsurvplot(
           # splots[[1]] <- ggsurvplot(
           fit,
           data = icu_COX,
           ylab="Probability of no need to Venilation",
           xlab = "Days",
           xlim = c(0,40),
           size = 1,                 # change line size
           palette =
             c("#E7B800", "#2E9FDF"),# custom color palettes
           conf.int = FALSE,          # Add confidence interval
           pval = TRUE,              # Add p-value
           risk.table = FALSE,        # Add risk table
           risk.table.col = "age_group",# Risk table color by groups
           legend.labs =
             c("Normal/Abnormal Menstruation", "Postmenopausal"),    # Change legend labels
           risk.table.height = 0.25, # Useful to change when you have multiple groups
           ggtheme = theme_bw()      # Change ggplot2 theme
         ) +  guides(
           fill = guide_legend(title = 'Regularity Group'),
           color = guide_legend(title = 'Regularity Group')
         )
         
         tiff("output/FIG7_Regularity.tiff", units="in", width=6, height=3, res=300)
         FIG7_Regularity
         dev.off()
         
  ###########coexisting
         fit <- survfit(Surv(pce_tijeinterval,pce ) ~ Coexsiting, data = icu_COX)
         FIG7_Coexsiting<- ggsurvplot(
           # splots[[1]] <- ggsurvplot(
           fit,
           data = icu_COX,
           ylab="Probability of no need to Venilation",
           xlab = "Days",
           xlim = c(0,40),
           size = 1,                 # change line size
           palette =
             c("#E7B800", "#2E9FDF"),# custom color palettes
           conf.int = FALSE,          # Add confidence interval
           pval = TRUE,              # Add p-value
           risk.table = FALSE,        # Add risk table
           risk.table.col = "age_group",# Risk table color by groups
           legend.labs =
             c("Yes", "No"),    # Change legend labels
           risk.table.height = 0.25, # Useful to change when you have multiple groups
           ggtheme = theme_bw()      # Change ggplot2 theme
         ) +  guides(
           fill = guide_legend(title = 'Coexsiting Group'),
           color = guide_legend(title = 'Coexsiting Group')
         )
         
         tiff("output/FIG7_Coexsiting.tiff", units="in", width=6, height=3, res=300)
         FIG7_Coexsiting
         dev.off()
         
         