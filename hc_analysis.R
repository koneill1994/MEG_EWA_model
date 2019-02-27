
library(ggplot2)
library(dplyr)
library(foreach)

setwd("E:/Libraries/r projects/MEG_EWA_model-master/data")
# df=readRDS("2019-02-25_18-50-29_EWA-hc.rds")
# start with this one

id_count=0
m_df=foreach(file=dir()[grepl("EWA-hc.rds",dir())], .combine=rbind) %do% {
  f_obj=readRDS(file)
  f_obj$hc_id=as.numeric(f_obj$hc_id)+id_count
  id_count=max(f_obj$hc_id)
  return(cbind(f_obj,file))
}

# check that we've renamed the id's correctly
table(m_df$hc_id)

# have a look at the success of the hill climbers
ggplot(m_df[m_df$chosen,],aes(x=round,y=rmse,group=hc_id,color=hc_id))+
  geom_line()

# do a k-means cluster analysis on the stopping points of the agents

# successful parameters
df_tail=m_df[m_df$round>max(m_df$round)-10 & m_df$chosen,]
df_tail[,1:8]=lapply(df_tail[,1:8],as.numeric)
df_tail=select(df_tail,1:8,rmse,hc_id,round)

sp=aggregate(df_tail[,1:4],list(df_tail$hc_id),mean)


k_alg=c("Hartigan-Wong", "Lloyd", "MacQueen")
n_clusters=2:10
n_runs=10

# create a bunch of runs of k-means
kdf_0=replicate(n_runs,{
  return(
    unlist(
      lapply(n_clusters,
             function(x){
               unlist(
                 lapply(k_alg,
                        function(y){
                          k=kmeans(sp[2:5],x,algorithm = y)
                          return(k$betweenss/k$totss)
                        }
                 )
               )
             }
      )
    )
  )
})



# put them into the proper format for ggplot
kdf=data.frame(
  clusters=unlist(lapply(n_clusters,function(x){rep(x,length(k_alg))})),
  algorithm=rep(k_alg,length(n_clusters)),
  ex_mean=apply(kdf_0,1,function(x){mean(x)}),
  ex_se=apply(kdf_0,1,function(x){sd(x)/sqrt(length(x))})
)

ggplot(kdf,aes(x=clusters,y=ex_mean,group=algorithm,color=algorithm))+
  geom_line(position=position_dodge(width=.1))+
  geom_point(position=position_dodge(width=.1))+
  geom_errorbar(aes(x=clusters,ymin=ex_mean-ex_se,ymax=ex_mean+ex_se),width=.1,
                position=position_dodge(width=.1))+
  labs(y="explained variance")

plot(hclust(dist(sp[2:5])))







