
library(ggplot2)
library(dplyr)

setwd("E:/Libraries/r projects/MEG_EWA_model-master/data")
df=readRDS("2019-02-25_18-50-29_EWA-hc.rds")

ggplot(df[df$chosen,],aes(x=round,y=rmse,group=hc_id,color=hc_id))+
  geom_line()

# do a k-means cluster analysis on the stopping points of the agents

# successful parameters
df_tail=df[df$round>max(df$round)-10 & df$chosen,]
df_tail[,1:8]=lapply(df_tail[,1:8],as.numeric)
df_tail=select(df_tail,1:8,rmse,hc_id,round)

sp=aggregate(df_tail[,1:4],list(df_tail$hc_id),mean)

kmeans(sp,2)

kmeans(sp,3)

kmeans(sp,4)



