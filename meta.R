## meta analysis

library(meta)

meta <- read_excel( paste0("D:/stat_platform/raw",  "/templete.xls" ), sheet = 'all')
meta1 <- meta %>% filter(flag == 1)
meta_result <- metaprop (event ,n, data=meta1, studlab=paste(Author),sm="PLOGIT")
forest(metainf(meta_result,pooled= "random"))

metainf(meta_result,pooled= "random")