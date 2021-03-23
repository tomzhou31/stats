
options(OutDec="\xB7")

## import survival data
# suv<-  read_excel( paste0(wd,  "/input/time.xlsx" ), "Sheet1")
# names(suv) <- c("id", "name", "admission_date", "death_date", "recovery_date") 
# suv <- suv %>% mutate(admission_date = as.Date(admission_date) , death_date = as.Date(death_date),
#                       recovery_date = as.Date(recovery_date))
# 
# suvival <- sqldf("
#                  select a.id, b.bmi2,b.IL6, b.outcome, b.severity1, b.coexsiting, b.age, b.age1, b.age_group,
#                     case when b.regularity >=3 then 3 else b.regularity end as regularity,
#                   case when b.regularity >=2 then 2 else b.regularity end as regularity1,
#                  case when b.regularity in (1,2) then 1 when b.regularity >2 then 2  end as regularity2,
#                   a.admission_date, a.death_date, a.recovery_date,
#                   case when a.recovery_date is not null then 2 else 1 end as status,
#                 case when a.recovery_date is not null and a.admission_date is not null
#                 then a.recovery_date - a.admission_date end as time
# 
#                 from suv  a 
#                  left join covid b on a.id = b.id
#                  ") %>% filter(!(age == 47 & id ==2501786558 )) %>% 
#   mutate(time = if_else(is.na(time), as.Date('2020-03-10') - admission_date, time ) )
# 



# suvival$regularity <- as.factor(suvival$regularity)

## Kaplan-Meier estimator. The "log-log" confidence interval is preferred.
# km.by.regularity1 <- survfit(SurvObj ~ regularity1, data = suvival, conf.type = "log-log")

splots <- list()

########################regularity#############################################
# fit <- survfit(SurvObj ~ regularity, data = suvival)
# ggsurvplot(
#   fit, 
#   data = suvival_COX, 
#   size = 1,                 # change line size
#   palette = 
#     c("#E7B800", "#2E9FDF", "#000000"),# custom color palettes
#   conf.int = FALSE,          # Add confidence interval
#   pval = TRUE,              # Add p-value
#   risk.table = FALSE,        # Add risk table
#   risk.table.col = "regularity",# Risk table color by groups
#   legend.labs =
#     c("Normal Menstruation", "Abnormal Menstruation", "No Menstruation"),    # Change legend labels
#   risk.table.height = 0.25, # Useful to change when you have multiple groups
#   ggtheme = theme_bw()      # Change ggplot2 theme
# )

options(OutDec="\xB7")
#  ########################age_group#############################################
fit <- survfit(SurvObj ~ Age, data = suvival_COX)
# age_group <- ggsurvplot(
  splots[[1]] <- ggsurvplot(
  fit,
  data = suvival_COX,
  ylab="Probability of hospitalization",
  xlab = "Days",
  xlim = c(0,40),
  size = 1,                 # change line size
  palette =
    c("#E7B800", "#2E9FDF"),# custom color palettes
  conf.int = FALSE,          # Add confidence interval
  pval = "P < 0·0001",              # Add p-value
  pval.size = 10,
  risk.table = FALSE,        # Add risk table
  risk.table.col = "age_group",# Risk table color by groups
  legend.labs =
    c("0 ~ 50 yrs", "51 ~ yrs"),    # Change legend labels
  risk.table.height = 0.25, # Useful to change when you have multiple groups
  ggtheme = theme_bw(base_size = 30) ,     # Change ggplot2 theme
  font.main = c(25, "plain", "black"),
  font.submain = c(25, "plain", "black"),
  font.caption = c(25, "plain", "black"),
  font.x = c(30, "plain", "black"),
  font.y = c(30, "plain", "black"),
  font.tickslab = c(20, "plain", "black")
) +  guides(
  fill = guide_legend(title = 'Age Group'),
  color = guide_legend(title = 'Age Group') 
) 
# 
# age_group <-   age_group  %+% annotate("text", x = 1, y = 0.20, size=7, label="italic(p) == 0.03", parse=TRUE) 
#   
# age_group  %+% theme_survminer(
#     font.main = c(16, "bold", "darkblue"),
#     font.submain = c(15, "bold.italic", "purple"),
#     font.caption = c(14, "plain", "orange"),
#     font.x = c(14, "bold.italic", "red"),
#     font.y = c(14, "bold.italic", "darkred"),
#     font.tickslab = c(12, "plain", "darkgreen")
#   )
#   

# tiff("output/FIG6_age_group.tiff", units="in", width=6, height=3, res=300)
# age_group
# dev.off()

# EPS("output/FIG6_age_group.eps", units="in", width=6, height=3, res=300)
# age_group
# dev.off()
# 
# setEPS(bg = "white",family = "Times", width = 6, height=3, resolution=300 )
# postscript("output/FIG6_age_group.eps")
# age_group
# dev.off()

########################regularity1#############################################
fit <- survfit(SurvObj ~ Menstruation, data = suvival_COX)
# Regularity <- ggsurvplot(
  splots[[2]] <- ggsurvplot(
  fit, 
  data = suvival_COX, 
  ylab="Probability of hospitalization",
  xlab = "Days",
  xlim = c(0,40),
  size = 1,                 # change line size
  palette = 
    c("#E7B800", "#2E9FDF"),# custom color palettes
  conf.int = FALSE,          # Add confidence interval
  pval = TRUE,              # Add p-value
  pval.size = 10,
  risk.table = FALSE,        # Add risk table
  risk.table.col = "Regularity",# Risk table color by groups

  legend.labs =
    c("Non-menopause", "Menopause"),    # Change legend labels
 
  risk.table.height = 0.25, # Useful to change when you have multiple groups
  ggtheme = theme_bw(base_size = 28) ,     # Change ggplot2 theme
  font.main = c(25, "plain", "black"),
  font.submain = c(25, "plain", "black"),
  font.caption = c(25, "plain", "black"),
  font.x = c(30, "plain", "black"),
  font.y = c(30, "plain", "black"),
  font.tickslab = c(20, "plain", "black")
) +  guides(
  fill = guide_legend(title = 'Menstruation'),
  color = guide_legend(title = 'Menstruation') 

)
# tiff("output/FIG6_Regularity.tiff", units="in", width=6, height=3, res=300)
# Regularity
# dev.off()


########################severity1#############################################
# surv_diff <- survdiff(SurvObj ~ Severity, data = suvival_COX)
 fit <- survfit(SurvObj ~ Severity, data = suvival_COX)
splots[[3]] <- ggsurvplot(
# Severity <-ggsurvplot(
  fit, 
  data = suvival_COX, 
  ylab="Probability of hospitalization",
  xlab = "Days",
  xlim = c(0,40),
  size = 1,                 # change line size
  palette = 
    c("#E7B800", "#2E9FDF"),# custom color palettes
  conf.int = FALSE,          # Add confidence interval
  pval = "P = 0·0029",              # Add p-value
  pval.size = 10,
  risk.table = FALSE,        # Add risk table
  risk.table.col = "Severity",# Risk table color by groups
  legend.labs =
    c("Non-severe", "Severe"),    # Change legend labels
  risk.table.height = 0.25, # Useful to change when you have multiple groups
  ggtheme = theme_bw(base_size = 30) ,     # Change ggplot2 theme
  font.main = c(25, "plain", "black"),
  font.submain = c(25, "plain", "black"),
  font.caption = c(25, "plain", "black"),
  font.x = c(30, "plain", "black"),
  font.y = c(30, "plain", "black"),
  font.tickslab = c(20, "plain", "black")
  # font.legend = c(20, "black")
)+  
    guides(
  fill = guide_legend(title = 'Disease Severity'),
  color = guide_legend(title =  'Severity')
) 


# tiff("output/FIG6_Severity.tiff", units="in", width=6, height=3, res=300)
# Severity
# dev.off()


 ########################coexsiting#############################################
surv_diff <- survdiff(SurvObj ~ Comorbidities, data = suvival_COX)
fit <- survfit(SurvObj ~ Comorbidities, data = suvival_COX)
Coexisting <- ggsurvplot(
  fit, 
  data = suvival_COX, 
  ylab="Probability of hospitalization",
  xlab = "Days",
  xlim = c(0,40),
  size = 2,                 # change line size
  palette = 
    c("#E7B800", "#2E9FDF"),# custom color palettes
  conf.int = FALSE,          # Add confidence interval
  pval = TRUE,              # Add p-value
  risk.table = FALSE,        # Add risk table
  risk.table.col = "Comorbidity",# Risk table color by groups
  legend.labs =
    c("Yes", "No"),    # Change legend labels
  risk.table.height = 0.25, # Useful to change when you have multiple groups
  ggtheme = theme_bw(base_size = 30) ,     # Change ggplot2 theme
  font.main = c(25, "plain", "black"),
  font.submain = c(25, "plain", "black"),
  font.caption = c(25, "plain", "black"),
  font.x = c(30, "plain", "black"),
  font.y = c(30, "plain", "black"),
  font.tickslab = c(20, "plain", "black")
  # font.legend = c(20, "black")
)+ 
 guides(
  fill = guide_legend(title = 'Coexisting diseases'),
  color = guide_legend(title = 'Coexisting diseases')
) 

tiff("output/FIG6_Coexisting.tiff", units="in", width=6, height=3, res=300)
Coexisting
dev.off()


# splots[[4]] <-  ggforest(res.cox)
#######################################################################################
## Combine 4 plots into 1
##
#######################################################################################

# tiff("output/km_plot.tiff", units="in", width=24, height=9, res=300)
# ggarrange(age_group,Severity,
#           labels = c("A", "B" ),
#           ncol = 2, nrow = 1, font.label = list(size = 25, color = "black") , align = "hv")
# 
# dev.off()


tiff("output/km_plot.tiff", units="in", width=12, height=12, res=300)
res <- arrange_ggsurvplots(splots, print = TRUE,
                    ncol = 2, nrow = 2, risk.table.height = 0.4)
dev.off()


res <- arrange_ggsurvplots(splots, print = TRUE, 
                    ncol = 3, nrow = 1, risk.table.height = 0)


ggsave("output/km_plot.pdf", res, 
       width = 27,
       height = 9,
       units = "in",
       dpi = 300)



# 
# tiff("output/FIG6.tiff", units="in", width=10, height=6, res=300)
# res
# dev.off()
# 
# # ggsave("output/FIG6.png", res ,width = 20 , height = 20, units='cm')
# 
# 
# 
# 
# 
# cox<- coxph(Surv(time, status) ~ age_group, data = suvival1)
#  summary(cox)
#  ggforest(cox)
#  
#  
#  ####Multivariate Cox regression analysis
#  res.cox <- coxph(Surv(time,status ) ~ age_group + coexsiting + regularity1 + severity1 , data =  suvival)
#  summary(res.cox) 
#  
#  new_df <- with(suvival,
#                 data.frame(regularity1 = c(1, 2), 
#                            age = rep(mean(age, na.rm = TRUE), 2),
#                            wt.loss = rep(mean(wt.loss, na.rm = TRUE), 2)
#                 )
#  )
#  fit <- survfit(res.cox, newdata = new_df)
#  ggsurvplot(fit, data=suvival)
#  ggsurvplot(fit, data=suvival, fun="event")
#  
#  
#  cox <- summary(res.cox) 
#  cox_forest <- ggforest(res.cox)
#  ggsave("output/FIG6_forest.png", cox_forest ,width = 14 , height = 10, units='cm')
#  
 

 
 
 # 
 # 
 # res.cox <- coxph(SurvObj ~ age_group + coexsiting + regularity1 + severity1 + il6, data =  suvival)
 # summary(res.cox)
 # 
 # x <- summary(res.cox)
 # p.value<-signif(x$wald["pvalue"], digits=2)
 # wald.test<-signif(x$wald["test"], digits=2)
 # beta<-signif(x$coef[1], digits=2);#coeficient beta
 # HR <-signif(x$coef[2], digits=2);#exp(beta)
 # HR.confint.lower <- signif(x$conf.int[,"lower .95"], 2)
 # HR.confint.upper <- signif(x$conf.int[,"upper .95"],2)
 # HR <- paste0( " (", 
 #              HR.confint.lower, "-", HR.confint.upper, ")")
 # res<-c(beta, HR, wald.test, p.value)
 # names(res)<-c("beta", "HR (95% CI for HR)", "wald.test", 
 #               "p.value")
 # 
 # 
 # # Plot survival curves
 # library(survminer)
 # 
 # sex_df <- with(suvival,
 #                data.frame(regularity = c(1, 2, 3), 
 #                           coexsiting = rep(min(age, na.rm = TRUE), 3),
 #                           age_group = c(1, 1,1)
 #                )
 # )
 # sex_df
 # fit <- survfit(res.cox, newdata = sex_df)
 # 
 # ggsurvplot(fit, conf.int = TRUE, legend.labs=c("1=1", "2=2", "3"),
 #            ggtheme = theme_minimal() )
 # 
 # aa <-  survfit(formula = SurvObj ~ age_group, data = suvival, conf.type = "log-log")
 # 
 # plot(aa)
 # 