

library(cluster)
library(factoextra)
library(tidyverse)

df <-  read.csv( "D:/stat_platform/raw/rsc.csv", header = TRUE)
names(df) <- c("keshi", "gaoji", "contribution")

## step1 delete missing value 
df <- df %>% filter(!is.na(contribution)) 
## step2 assign row names
rownames(df) <- df[,1]
df <- df %>% select(-keshi) ## delete first column
## step3 stand
df1 <- scale(df)
k3 <- kmeans(df1, centers = 3, nstart = 2)
fviz_cluster(k3, data = df1)

fviz_cluster(k3, data = df1, 
             palette = c("#488f31","#ffa600","#de425b"),
             star.plot=TRUE, 
             geom = c("point", "text"),
             pointsize = 1,
             textsize = 0.5,
             ellipse.type="euclid",
             ellipse.alpha = 0.1,
             repel = TRUE,
             xlab = "高级职称比例(低->高)",
             ylab = "贡献",
             show_labels = TRUE) + theme_bw()


