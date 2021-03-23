
pvalue <- chisq.test(table(desc1$gender, desc1$outcome_group))
p_outcome <- paste(pvalue[3] )
p_outcome <- if_else ( as.numeric(p_outcome)< 0.01, "P < 0.01", paste0("P=", round(as.numeric(p_outcome), digits = 2 )) ) 

options(OutDec="\xB7")
windowsFonts(Times=windowsFont("Arial"))

fig1a <- sqldf("select gender,outcome_group, count(*) as count from desc1 group by gender,outcome_group ")
fig1a <- fig1a %>% group_by(gender) %>% mutate(pct= round(count / sum(count),2))


options(OutDec="\xB7")
 a1<- ggplot(data=fig1a, aes(x=outcome_group , y=pct*100, fill=gender)) +
  geom_bar(stat="identity", position=position_dodge())+
  coord_cartesian(ylim=c(0,10)) + 
   scale_y_continuous(breaks=seq(0,10,5) ) + 
  geom_text(aes(label=paste0(round(pct*100), "%")), vjust=2, color="white",
            position = position_dodge(0.9), size=6)+
  # theme(text = element_text(size=20) ) +
  scale_x_discrete(labels=c("1. Discharge" = "Discharge", "2. remained in hospital" = "Remained in hospital",
                            "3. death" = "Death" )) + 
  # scale_fill_brewer(palette="Paired")+
  # scale_fill_manual(values=c("gray" ,'black'))+
  # scale_fill_manual(values= c("#ff6361", "#2E9FDF")) +
  scale_fill_grey() +
  theme_bw() +
  # theme(legend.position=c(0.10, 0.80) ,text = element_text(size=15)) +
  theme(legend.position='none' ,text = element_text(size=15)) +
  xlab("Outcomes") +
  ylab(" ")+
  labs(fill = "Gender")
  # theme(plot.margin=unit(c(2,2,0,0),"mm")) 
 options(OutDec="\xB7")
a2 <- 
  ggplot(data=fig1a, aes(x=outcome_group , y=pct*100, fill=gender)) +
  geom_bar(stat="identity", position=position_dodge())+
  coord_cartesian(ylim=c(80,90)) +
  scale_y_continuous(breaks=seq(80,90,5) ) + 
  geom_text(aes(label=paste0(round(pct*100), "%")), vjust=2, color="white",
            position = position_dodge(0.9), size=6)+
  scale_x_discrete(labels=c("1. Recovery" = "Recovery", "2. Improve" = "Improved",
                            "3. remained in hospital" = "Remained in hospital", "4. Critcal" = "Critcal" )) + 
  annotate("text",  label="p = 0·00043" , x = 3, y = 85, size=6) +
  # annotate("text",  label="italic(p) == 0.00043" , x = 3, y = 85, size=6, parse=TRUE) +
  # annotate("text", x = 0.7, y = 185, size=7, label="italic(p) == 0.04", parse=TRUE) +

  ylab("Percentage (%)")+
  # scale_fill_brewer(palette="Paired")+
  # scale_fill_manual(values=c("gray" ,'black'))+
  # scale_fill_manual(values= c("#ff6361", "#2E9FDF")) +
  scale_fill_grey() +
  theme_bw() +
  theme(legend.position=c(0.10, 0.70) ,text = element_text(size=15)) +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        # axis.title.y = element_blank(),
        axis.ticks.x = element_blank()) + 
  labs(x=NULL) + 
  # theme(legend.position='none' ,text = element_text(size=15)) +
  xlab("Outcomes") +
  # ylab("    ")+
  labs(fill = "Gender") 
  # theme(plot.margin=unit(c(2,2,6,3),"mm")) 

A <- grid.arrange(a2,a1, heights=c(1.8/4, 2.2/4), ncol=1, nrow=2)

 # a <- grid.arrange(a2,a1, heights=c(2/4, 2/4), ncol=1, nrow=2)

 tiff("output/FIG1a.tiff", units="in", width=6, height=6, res=300)
 grid.arrange(a2,a1, heights=c(1.8/4, 2.2/4), ncol=1, nrow=2)
 dev.off()
 
 grid.arrange(a2,a1, heights=c(1.8/4, 2.2/4), ncol=1, nrow=2)
 ggsave("output/FIG1a.eps", width = 6, height = 6,  units = "in", dpi = 300)
 
 pdf(file = "output/FIG1a.pdf",   # The directory you want to save the file in
     width = 6, # The width of the plot in inches
     height = 6) # The height of the plot in inches
 
 grid.arrange(a2,a1, heights=c(1.8/4, 2.2/4), ncol=1, nrow=2)
 
 dev.off()
 
# require(ggplot2)
# require(gridExtra)
# data <- data.frame(qnt=c(10,20,22,12,14,9,1000),lbl=c("A","B","C","D","E","F","G"))
# 
# g1<-ggplot(data=data, aes(x=lbl, y=qnt)) + 
#   geom_histogram(stat="identity")+  
#   coord_cartesian(ylim=c(-10,50)) + 
#   labs(x=NULL, y=NULL)+
#   theme(plot.margin=unit(c(2,2,6,3),"mm")) 
# 
# 
# g2<-ggplot(data=data, aes(x=lbl, y=qnt)) + 
#   geom_histogram(stat="identity") +  
#   coord_cartesian(ylim=c(990,1010)) +
#   theme(axis.text.x = element_blank(),
#         axis.title.x = element_blank(),
#         axis.title.y = element_blank(),
#         axis.ticks.x = element_blank()) + 
#   labs(x=NULL, y=NULL) + 
#   theme(plot.margin=unit(c(5,2,0,0),"mm")) 
# 
# grid.arrange(g2,g1, heights=c(1/4, 3/4), ncol=1, nrow=2)
 
 # pvalue <- chisq.test(table(desc1$gender, desc1$outcome_group))
 # p_outcome <- paste(pvalue[3] )
 # p_outcome <- if_else ( as.numeric(p_outcome)< 0.01, "P < 0.01", paste(round(as.numeric(p_outcome), digits = 2 )) ) 
 # 
 
 pvalue <- chisq.test(table(desc1$gender, desc1$sevirity_group))
 p_severity <- paste(pvalue[3] )
 p_severity <- if_else ( as.numeric(p_severity)< 0.01, "P < 0.01", 
                         paste("P = ", round(as.numeric(p_severity), digits = 2 )) ) 
 
 ## sevirity
 fig1b <- sqldf("select gender,sevirity_group, count(*) as count from desc1 
                where sevirity_group is not null group by gender,sevirity_group ")
 fig1b <- fig1b %>% group_by(gender) %>% mutate(pct= round(count / sum(count),2))
 
 

 # table(fig1b$sevirity_group)
 
 # theme_minimal()
 b1<- ggplot(data=fig1b, aes(x=sevirity_group , y=pct*100, fill=gender)) +
   geom_bar(stat="identity", position=position_dodge())+
   coord_cartesian(ylim=c(0,10)) + 
   scale_y_continuous(breaks=seq(0,10,5) ) + 
   geom_text(aes(label=paste0(round(pct*100), "%")), color="white",  vjust=2,
             position = position_dodge(0.9), size=6,
             text = element_text(size=20))+
   # theme(text = element_text(size=20) ) +
   scale_x_discrete(labels=c("1. Mild" = "Mild", "2. Severe" = "Severe","3. Critical" = "Critcal" )) + 
   scale_fill_grey() +
   theme_bw() +
   xlab("Disease severity") +
   ylab("  ")+
   theme(legend.position='none' ,text = element_text(size=15)) 
   # labs(fill = "Gender") +

 # theme(plot.margin=unit(c(2,2,0,0),"mm")) 
 
 b2 <- 
   ggplot(data=fig1b, aes(x=sevirity_group , y=pct*100, fill=gender)) +
   geom_bar(stat="identity", position=position_dodge())+
   coord_cartesian(ylim=c(40,55)) +
   scale_y_continuous(breaks=seq(40,55,5) ) + 
   geom_text(aes(label=paste0(round(pct*100), "%")), color="white",  vjust=2,
             position = position_dodge(0.9), size=6)+
   scale_x_discrete(labels=c("1. Mild" = "Mild", "2. Severe" = "Severe","3. Critical" = "Critical" )) + 
   annotate("text", label = "p = 0·049", x = 3, y = 50, size=6) +
   # annotate("text", label = "italic(p) == 0.049", x = 3, y = 50, size=6, parse=TRUE) +
   # annotate("text",  label="italic(p) <= 0.01" , x = 3, y = 85, size=6, parse=TRUE) +
   
   ylab("Percentage (%)")+
   # scale_fill_brewer(palette="Paired")+
   # scale_fill_manual(values=c("gray" ,'black'))+
   # scale_fill_manual(values= c("#ff6361", "#2E9FDF")) +
   scale_fill_grey() +
   theme_bw() +
   theme(legend.position=c(0.1, 0.75) ,text = element_text(size=15)) +
   theme(axis.text.x = element_blank(),
         axis.title.x = element_blank(),
         # axis.title.y = element_blank(),
         axis.ticks.x = element_blank()) + 
   # labs(x=NULL, y=NULL) + 
   # theme(legend.position='none' ,text = element_text(size=15)) +
   # xlab("Outcomes") +
   labs(fill = "Gender") 
 # theme(plot.margin=unit(c(2,2,6,3),"mm")) 
 
 B <- grid.arrange(b2,b1, heights=c(2/4, 2/4), ncol=1, nrow=2)
 
 
 tiff("output/FIG1b.tiff", units="in", width=6, height=6, res=300)
 grid.arrange(b2,b1, heights=c(2/4, 2/4), ncol=1, nrow=2)
 dev.off()
 
 ggsave("output/FIG1b.eps", width = 6, height = 6,  units = "in", dpi = 300)
 

 
 pdf(file = "output/FIG1b.pdf",   # The directory you want to save the file in
     width = 6, # The width of the plot in inches
     height = 6) # The height of the plot in inches
 
 grid.arrange(b2,b1, heights=c(2/4, 2/4), ncol=1, nrow=2)
 
 dev.off()
 
 
 pdf(file = "output/FIG1.pdf",   # The directory you want to save the file in
     width = 13, # The width of the plot in inches
     height = 6) # The height of the plot in inches
 
 ggarrange(A, B, 
           labels = c("A", "B"),
           ncol = 2, nrow = 1, font.label = list(size = 25, color = "black") , align = "hv", hjust=0)
 
 dev.off()
 
 # ggsave("output/FIG1.tiff", arrangeGrob(a, b) ,width = 20 , height = 20, units='in')
 