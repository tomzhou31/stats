
library(tidyverse)
library(viridis)
library(ggplot2)
library(ggpubr)
library(ggridges)
library(gridExtra)
library(Unicode)

covid_fig5 <-covid %>%  
  mutate(log_il2 = log(il2), log_il6 = log(il6), log_il8 = log(il8), 
         log_il10 =log(il10), log_e2 =log(e2) , log_c4 = log(c4) , log_tnf = log(tnf))
## corelation
# & il6 < 600 
# & il8 < 200
fig5_il6 <- covid %>% select(severity_char, il6) %>% filter(!is.na(il6) ) %>% mutate(grp = "il6" ) %>% rename(value=il6)

fig5_il8 <- covid %>% select(severity_char, il8) %>% filter(!is.na(il8) ) %>% mutate(grp = "il8" ) %>% rename(value=il8)
fig5_il81 <- fig5_il8 %>% filter(!is.na(severity_char)) %>%
  mutate(severity_char1 = if_else(severity_char =="Non-severe", "1. Non-severe", "2. severe" ) )


fig5_c4 <- covid %>% select(severity_char, c4) %>% filter(!is.na(c4) ) %>% mutate(grp = "c4" ) %>% rename(value=c4)
fig5_il2 <- covid %>% select(severity_char, il2) %>% filter(!is.na(il2) ) %>% mutate(grp = "il2" ) %>% rename(value=il2)

fig5 <- rbind(fig5_il6, fig5_il8, fig5_c4, fig5_il2)

mean_fig5 <- fig5 %>% select(severity_char, grp, value) %>% group_by(severity_char, grp) %>%
  mutate( value = mean(value) ) %>% distinct()

mean_il6 <- mean_fig5 %>% filter(grp == "il6")
mean_il8 <- mean_fig5 %>% filter(grp == "il8") 

mean_c4 <- mean_fig5 %>% filter(grp == "c4")
mean_il2 <- mean_fig5 %>% filter(grp == "il2")


p_il6 <- wilcox.test(value  ~ severity_char,data=fig5_il6)
p_il6 <- p_il6$p.value 
p_il6 <- if_else ( as.numeric(p_il6)< 0.01, " < 0.01", paste0("= " , round(as.numeric(p_il6), digits = 2 )) ) 


windowsFonts(Times=windowsFont("Arial"))


icu2_pce_il6 <- icu2 %>% filter(!is.na(il6) & !is.na(pce)) %>% select( pce, il6) %>% 
  mutate(pce_char = if_else(pce ==0, "No", "Yes")) 

mean_pce_il6 <- icu2_pce_il6 %>% filter(!is.na(il6)) %>% select( pce_char, il6) %>%
  group_by(pce_char) %>% summarise(value = mean(il6)) 

p_pce_il6 <- wilcox.test(il6  ~ pce_char,data=icu2_pce_il6)
p_pce_il6 <- p_pce_il6$p.value 
p_pce_il6 <- if_else ( as.numeric(p_pce_il6)< 0.01, " < 0.01", paste0( round(as.numeric(p_pce_il6), digits = 2 )) ) 



# 
# fig_il6 <- ggplot()+
#   ggplot(data= fig5_il6, aes(x=severity_char, y=value)) +
#   geom_jitter(colour = I("BLACK"))
# 
# 
# geom_point(pch = 21, position = position_jitterdodge())

# ggdotplot(fig5_il6, x = "severity_char", y = "value",
#                 color = "BLACK", palette = "jco") +
# theme_bw(base_size = 25) 

fig_il6 <- ggplot()+
  geom_jitter(aes(pce_char, il6) , data = icu2_pce_il6, #alpha=0.5 ,#
              position = position_jitter(width = 0.15))+
  
  geom_jitter(aes(severity_char, value ), data = fig5_il6, colour = I("BLACK") , #alpha=0.5 #
              position = position_jitter(width = 0.15))+
  scale_y_continuous(breaks=seq(0,300,50) ) + 
  
  ylim(0, 300) +
  geom_crossbar(data=mean_il6,aes(x=severity_char,ymin=value, ymax=value,y=value,group=severity_char), width = 0.5) +
  geom_crossbar(data=mean_pce_il6,aes(x=pce_char,ymin=value, ymax=value,y=value,group=pce_char), width = 0.5) + 
  # annotate("text", x = 1.4, y = 285, size=7, label="italic(p) == 0.04", parse=TRUE) +
  annotate("text", x = 1.4, y = 285, size=7, label = "p = 0·040", parse = FALSE)  +
  annotate("text", x = 3.7, y = 285, size=7, label = "p < 0·0001", parse = FALSE)  +
  # annotate("text", x = 3.7, y = 285, size=7, label="italic(p) < 0.0001", parse=TRUE) +
  
  scale_x_discrete(limits=c("Non-severe", "severe", "Yes" , "No") ) +

  
  theme_bw(base_size = 22) +
  labs(
    x = "Disease severity   Composite endpoint",
    y = "IL6") 

fig_il6
# tiff("output/fig_severity_il6.tiff", units="in", width=6, height=4.5, res=300)
# fig_il6
# dev.off()

# setEPS()
# postscript("output/fig_severity_il6.eps")
# fig_il6
# dev.off()



# 
# fig_log_il6 <- ggplot()+
#   geom_jitter(aes(severity_char, log(value) ), data = fig5_il6, colour = I("BLACK") , alpha=0.5 ,
#               position = position_jitter(width = 0.15))+
#   scale_y_continuous(breaks=seq(0,3,1) ) + 
#   ylim(0, 10) +
#   geom_crossbar(data=mean_il6,aes(x=severity_char,ymin=value, ymax=value,y=value,group=severity_char), width = 0.5) + 
#   annotate("text", label = p_il6, x = 0.7, y = 9, size=6) +
#   theme_bw(base_size = 20) +
#   labs(
#     x = "Disease severity",
#     y = "Log(IL6)") 


icu2_pce_il8 <- icu2 %>% filter(!is.na(il8) & !is.na(pce)) %>% select( pce, il8) %>% 
  mutate(pce_char = if_else(pce ==0, "No", "Yes")) 

mean_pce_il8 <- icu2_pce_il8 %>% filter(!is.na(il8)) %>% select( pce_char, il8) %>%
  group_by(pce_char) %>% summarise(value = mean(il8)) 

p_pce_il8 <- wilcox.test(il8  ~ pce_char,data=icu2_pce_il8)
p_pce_il8 <- p_pce_il8$p.value 
p_pce_il8 <- if_else ( as.numeric(p_pce_il8)< 0.01, " < 0.01", paste0( round(as.numeric(p_pce_il8), digits = 2 )) ) 



p_il8 <- wilcox.test(value  ~ severity_char,data=fig5_il8)
p_il8 <- p_il8$p.value 
p_il8 <- if_else ( as.numeric(p_il8)< 0.01, " < 0.01", paste0( round(as.numeric(p_il8), digits = 2 )) ) 


icu2_pce_il8$pce_char <- factor(icu2_pce_il8$pce_char, levels=c("No", "Yes"))
# fig5_il8$severity_char <- factor(fig5_il8$severity_char, levels=c("Non-severe", "Severe"))

fig_il8 <- ggplot()+
  geom_jitter(aes(pce_char, il8) , data = icu2_pce_il8, #alpha=0.5 ,
              position = position_jitter(width = 0.15))+

  geom_jitter(aes(severity_char, value) , data = fig5_il8, #alpha=0.5 ,
              position = position_jitter(width = 0.15))+
  ylim(0, 300) +
  scale_x_discrete(limits=c("Non-severe", "severe", "Yes" , "No") ) +
  
  geom_crossbar(data=mean_il8,aes(x=severity_char,ymin=value, ymax=value,y=value,group=severity_char), width = 0.5) + 
  geom_crossbar(data=mean_pce_il8,aes(x=pce_char,ymin=value, ymax=value,y=value,group=pce_char), width = 0.5) + 
  # annotate("text", label = p_il8, x = 1.5, y = 100, size=6) +
  annotate("text", x = 1.4, y = 285, size=7, label="p = 0·033", parse=FALSE) +
  annotate("text", x = 3.7, y = 285, size=7, label="p = 0·00017", parse=FALSE) +
  geom_text(x = 14.25, # Set the position of the text to always be at '14.25'
            hjust = 0,
            size = 8) +
  theme_bw(base_size = 22) +
  labs(
    x = "Disease severity  Composite endpoint",
    y = "IL8")


# tiff("output/fig_severity_il8.tiff", units="in", width=6, height=4.5, res=300)
# fig_il8
# dev.off()


#################################################################################################3



icu2_pce_il2 <- icu2 %>% filter(!is.na(il2) & !is.na(pce)) %>% select( pce, il2, il6, il8) %>% 
  mutate(pce_char = if_else(pce ==0, "No", "Yes")) 
icu2_pce_il2$pce <- as.factor(icu2_pce_il2$pce) 

mean_pce_il2 <- icu2_pce_il2 %>% filter(!is.na(il2)) %>% select( pce_char, il2) %>%
  group_by(pce_char) %>% summarise(value = mean(il2)) 


p_pce_il2 <- wilcox.test(il2  ~ pce_char,data=icu2_pce_il2)
p_pce_il2 <- p_pce_il2$p.value 
p_pce_il2 <- if_else ( as.numeric(p_pce_il2)< 0.01, " < 0.01", paste0( round(as.numeric(p_pce_il2), digits = 2 )) ) 



fig_pce_il2 <- ggplot()+
  geom_jitter(aes(pce_char, il2) , data = icu2_pce_il2, #alpha=0.5 ,
              position = position_jitter(width = 0.15))+
  ylim(0, 3000) +
  scale_x_discrete(limits=c("Yes" , "No") ) +
  geom_crossbar(data=mean_pce_il2,aes(x=pce_char,ymin=value, ymax=value,y=value,group=pce_char), width = 0.5) + 
  # annotate("text", label = p_il8, x = 1.5, y = 100, size=6) +
  annotate("text", x = 0.8, y = 2800, size=7, label="p = 0·0091", parse=FALSE) +
  geom_text(x = 14.25, # Set the position of the text to always be at '14.25'
            hjust = 0,
            size = 8) +
  theme_bw(base_size = 25) +
  labs(
    x = "Composite endpoint",
    y = "IL2R")

# shapiro.test(icu2_pce_il2$il2 )

#######################################################################################################
#### Scatter plot for IL2,6,8,10,TNF AND E2
####
#######################################################################################################
aa <- covid_fig5 %>% filter(cycle ==1) %>% select(c3,e2 ) %>% filter(!is.na(c3) & !is.na(e2))
# 
# ggscatter(aa, x = "c3", y = "e2", add = "reg.line", cor.method = "pearson" ) +
#   stat_cor(
#     aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~")),
#     # label.x = 3,
#     label.x.npc = 0.40, 
#     label.y.npc = 0.9, hjust = 0, size = 7
#   ) + 
#   theme_bw() +
#   labs(title=sprintf( "Correlation between %s / %s",tit,lab2 ),
#        x = lab1,
#        y = lab2)+
#   theme_bw(base_size = 25) 


covid_fig5 <-covid %>%  
  mutate(log_il2 = log(il2), log_il6 = log(il6), log_il8 = log(il8), 
         log_il10 =log(il10), log_e2 =log(e2) , log_c4 = log(c4) , log_tnf = log(tnf))

windowsFonts(Times=windowsFont("Arial"))

scatter1 <- function(var1, var2, lab1, lab2, yellow , tit) {
  

  if (yellow == TRUE ) {
    covid_fig5_ <- covid_fig5 %>% filter(cycle ==2)
  } else {
    covid_fig5_ <- covid_fig5 %>% filter(cycle ==1)
  }
  
  ggscatter(covid_fig5_, x = var1, y = var2, add = "reg.line", cor.method = "pearson") +
    stat_cor(
      aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~")),
      # label.x = 3,
      label.x.npc = 0.40, 
      label.y.npc = 0.9, hjust = 0, size = 7
    ) + 
    theme_bw() +
    labs(title=sprintf( "Correlation between %s / %s",tit,lab2 ),
         x = lab1,
         y = lab2)+
    theme_bw(base_size = 25) 
}


# cor(aa$c3, aa$e2,  method = "pearson", use = "complete.obs")

# covid_fig5_ <- covid_fig5 %>% filter(cycle ==2)

##############il6
# il6 <- scatter1("e2", "il6", "E2 (In luteal phase)", "IL6", TRUE, "E2" )
# il6 <- il6 + font("title", size = 22)
lab <- "IL6"
covid_fig5_2 <- covid_fig5 %>% filter(cycle ==2) %>% filter(!is.na(e2) & !is.na(il6))

il6 <- ggplot(covid_fig5_2, aes(x=e2, y=il6)) + 
  geom_point() +
  geom_smooth(method=lm, se=FALSE, color="black") + 
  theme_bw() +
  font("title", size = 21) +
  labs(title=sprintf( "Correlation between E2 / %s", lab ) ,
       x = "E2 (In luteal phase)",
       y = lab ) +
  theme_bw(base_size = 23) +
  # annotate("text", x = 380, y = 1.15, size=7, label = "R = -0·65, p = 0·030", parse = FALSE)+
  annotate("text", x=max(covid_fig5_2$e2) * 0.75 ,y=max(covid_fig5_2$il6) * 0.9, label = "R = -0·56, p = 0·048", size=7)


# tiff("output/FIG5_corpus_il6.tiff" , units="in", width=6, height=3.5, res=300)
# il6
# dev.off()




##############il8
# il8 <- scatter1("e2", "il8", "E2 (In luteal phase)", "IL8", TRUE, "E2" )
# il8 <- il8 + font("title", size = 22)
lab <- "IL8"
covid_fig5_2 <- covid_fig5 %>% filter(cycle ==2) %>% filter(!is.na(e2) & !is.na(il8))

il8 <- ggplot(covid_fig5_2, aes(x=e2, y=il8)) + 
  geom_point() +
  geom_smooth(method=lm, se=FALSE, color="black") + 
  theme_bw() +
  font("title", size = 21) +
  labs(title=sprintf( "Correlation between E2 / %s", lab ) ,
       x = "E2 (In luteal phase)",
       y = lab ) +
  theme_bw(base_size = 23) +
  # annotate("text", x = 380, y = 1.15, size=7, label = "R = -0·65, p = 0·030", parse = FALSE)+
  annotate("text", x=max(covid_fig5_2$e2) * 0.75 ,y=max(covid_fig5_2$il8) * 0.9, label = "R = -0·55, p = 0·054", size=7)


# tiff("output/FIG5_corpus_il8.tiff" , units="in", width=6, height=3.5, res=300)
# il8
# dev.off()

##############il2
# il2 <- scatter1("e2", "il2", "E2 (In luteal phase)", "IL2R", TRUE, "E2" )
# il2 <- il2 + font("title", size = 22)

lab <- "IL2R"
covid_fig5_2 <- covid_fig5 %>% filter(cycle ==2) %>% filter(!is.na(e2) & !is.na(il2))

il2 <- ggplot(covid_fig5_2, aes(x=e2, y=il2)) + 
  geom_point() +
  geom_smooth(method=lm, se=FALSE, color="black") + 
  theme_bw() +
  font("title", size = 21) +
  labs(title=sprintf( "Correlation between E2 / %s", lab ) ,
       x = "E2 (In luteal phase)",
       y = lab ) +
  theme_bw(base_size = 23) +
  # annotate("text", x = 380, y = 1.15, size=7, label = "R = -0·65, p = 0·030", parse = FALSE)+
  annotate("text", x=max(covid_fig5_2$e2) * 0.75 ,y=max(covid_fig5_2$il2) * 0.94, label = "R = -0·59, p = 0·033", size=7)


##############TNF
windowsFonts(Times=windowsFont("Arial"))

lab <- paste("TNF", expression("\u03B1") , sep="")
covid_fig5_2 <- covid_fig5 %>% filter(cycle ==2) %>% filter(!is.na(e2) & !is.na(tnf))

# tnf <- scatter1("e2", "tnf", "E2 (In luteal phase)", paste("TNF", expression("\u03B1") , sep=""), TRUE, "E2" )
# tnf <- tnf + font("title", size = 22)

tnf <- ggplot(covid_fig5_2, aes(x=e2, y=tnf)) + 
  geom_point() +
  geom_smooth(method=lm, se=FALSE, color="black") + 
  theme_bw() +
  font("title", size = 21) +
       labs(title=sprintf( "Correlation between E2 / %s", lab ) ,
       x = "E2 (In luteal phase)",
       y = lab ) +
  theme_bw(base_size = 23) +
  # annotate("text", x = 380, y = 1.15, size=7, label = "R = -0·65, p = 0·030", parse = FALSE)+
  annotate("text", x=max(covid_fig5_2$e2) * 0.75 ,y=max(covid_fig5_2$tnf) * 0.9, label = "R = -0·62, p = 0·023", size=7)

# tiff("output/FIG5_corpus_tnf.tiff" , units="in", width=6, height=3.5, res=300)
# tnf
# dev.off()
# 
# tit_tnf <-  expression(paste("Correlation between E2 aaa / TNF", "\u03B1"), sep = "") 
# 
# covid_fig5_ <- covid_fig5 %>% filter(cycle ==2)
# tnf <- ggscatter(covid_fig5_, x ="e2", y = "tnf", add = "reg.line", cor.method = "spearman" ) +
#   stat_cor(
#     aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~")),
#     # label.x = 3,
#     label.x.npc = 0.40, 
#     label.y.npc = 0.9, hjust = 0, size = 7) + 
#   theme_bw() +
#   labs(title= tit_tnf,
#        x = "E2 (In luteal phase)",
#        y = "TNF")+
#   theme_bw(base_size = 15) 
# 
# tiff("output/FIG5_corpus_tnf.tiff" , units="in", width=6, height=3.5, res=300)
# tnf
# dev.off()
##############C3
# format_pval <- function(pval){
#   pval <- scales::pvalue(pval, accuracy= 0.0001, add_p = TRUE)
#   gsub(pattern = "(=|<)", replacement = " \\1 ", x = pval)
# }

options(OutDec="\xB7")

windowsFonts(Times=windowsFont("Arial"))
# c3 <- ggscatter(E2andC3, x = "e2", y = "c3", add = "reg.line", cor.method = "pearson" ) +
#   #   
#   #   # aes( label = paste(..r.label.., ..p.label.., sep = "~`,`~")), 
#   #   label.x.npc = 0.40, 
#   #   label.y.npc = 0.9, hjust = 0, size = 7
#   # ) + 
#   theme_bw() +
#   labs(title="Correlation between E2 / C3" ,
#        x = "E2 (In follicular phase)",
#        y = "C3")+
#   theme_bw(base_size = 25) +
# # annotate("text", x = 240, y = 1.2, size=7, label = "italic(R) == -0.65, italic(p) == =0.030", parse = TRUE)  
# # annotate("text", x = 240, y = 1.2, size=7, label="paste(italic(R) italic(p) == 0.0091 )", parse=TRUE) 
# annotate("text", x = 380, y = 1.15, size=7, label = "R = -0·65, p = 0·030", parse = FALSE)
# 
# c3 <- c3 + font("title", size = 22)

c3 <- ggplot(E2andC3, aes(x=e2, y=c3)) + 
  geom_point() +
  geom_smooth(method=lm, se=FALSE, color="black") + 
  theme_bw() +
  font("title", size = 21) +
  labs(title="Correlation between E2 / C3" ,
       x = "E2 (In follicular phase)",
       y = "C3")+
  theme_bw(base_size = 23) +
  annotate("text", x=max(E2andC3$e2) * 0.75 ,y=max(E2andC3$c3) * 0.94, label = "R = -0·65, p = 0·030", size=7)
# tiff("output/FIG5_folli_C3.tiff" , units="in", width=6, height=3.5, res=300)
# c3
# dev.off();

## create a blank plot 
f_blank <- ggplot() + theme_void()

tiff("output/fig_severity.tiff", units="in", width=27, height=9, res=300)
ggarrange(fig_il6, fig_il8, fig_pce_il2, c3,il6, il8, il2, tnf,
          labels = c("A", "B", "C",  "D" , "E", "F", "G" ,"H" ),
          ncol = 4, nrow = 2, font.label = list(size = 25, color = "black") , align = "hv")

dev.off()


ggarrange(fig_il6, fig_il8, fig_pce_il2, c3,il6, il8, il2, tnf,
          labels = c("A", "B", "C",  "D" , "E", "F", "G" ,"H" ),
          ncol = 4, nrow = 2, font.label = list(size = 25, color = "black") , align = "hv")

ggsave("output/fig_severity.eps", width = 27, height = 9,  units = "in", dpi = 300)


options(OutDec="\xB7")

pdf(file = "output/fig_severity.pdf",   # The directory you want to save the file in
    width = 27, # The width of the plot in inches
    height = 9) # The height of the plot in inches

ggarrange(fig_il6, fig_il8, fig_pce_il2, c3,il6, il8, il2, tnf,
          labels = c("A", "B", "C",  "D" , "E", "F", "G" ,"H" ),
          ncol = 4, nrow = 2, font.label = list(size = 25, color = "black") , align = "hv")

dev.off()
# setEPS()
# postscript("whatever.eps")
# plot(rnorm(100), main="Hey Some Data")
# dev.off()



