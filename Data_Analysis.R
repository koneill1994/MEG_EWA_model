library(dplyr)
library(Hmisc)

drawgraphs=F
Just_Agg_Data=T

Just_d1a=T

# Written by M_Collins, with adjustments by k_oneill

# analyze the data in the csv

# this creates the data frame to fit
# run the whole thing, don't need the plots
# just need data Agg_Data to fit EWA to

# all these a's and b's with no comments give me an aneurysm

#--------------------
setwd("C:/Users/Kevin/Dropbox/minimum_effort_game/EWA_model")
d1a=read.csv("ALL DATA up to session 10.csv", header=T)
#------------------------------------------------------
d1a$Condition=ifelse(d1a$Group %% 2 == 0, 2,1 )
a=group_by(d1a, Group, Round)
b=mutate(a, Distance = Effort_level - min(Effort_level) )
d1a=b
#-----------
a=group_by(d1a, Subject_ID)
b=mutate(a, Average_Payoff=mean(Payoff),
            Average_Choice=mean(Effort_level)
         )
d1a=b
#-----------------------------------------------------------
#-----------------------------------------------------------
a=group_by(d1a, Group, Round)
b=mutate(a, Min=ifelse(Effort_level==min(Effort_level), 1, 0))
d1a=b
#-------
a=group_by(d1a, Subject_ID)
b=mutate(a, Min_Freq=length(Min[Min==1])/length(Min) )
d1a=b

#-----------------------------------------------------------
#-----------------------------------------------------------
d1a$Risk_aversion=d1a$Risk_aversion_scale_shift-4
if(drawgraphs){
  hist(d1a$Min_Freq)
  hist(d1a$Risk_aversion[d1a$Round==1], breaks=seq(-4,6,1))
  hist(d1a$Effort_level[d1a$Round==1], breaks=seq(0,7,1))
}
#-----------------------------------------------------------
#-----------------------------------------------------------
a=group_by(d1a, Group)
b=mutate(a, 
         Rank_Payoff = ifelse( Average_Payoff == unique( Average_Payoff)[1],1, ifelse( Average_Payoff == unique( Average_Payoff)[2], 2, ifelse( Average_Payoff == unique( Average_Payoff)[3] ,3,4) )),
         Rank_Choice = ifelse( Average_Choice == unique( Average_Choice)[1],1, ifelse( Average_Choice == unique( Average_Choice)[2], 2, ifelse( Average_Choice == unique( Average_Choice)[3] ,3,4) )) 
         )
d1a=b

if(!Just_d1a){
  #====================================================================
  a=group_by(d1a, Rank_Payoff, Round)
  b=mutate(a,
    Choice=mean(Effort_level),
    Choice_CI=(sd(Effort_level)/sqrt(length(Effort_level))) * 1.96,
    Av_Dis=mean(Distance),
    Dis_CI=(sd(Distance)/sqrt(length(Distance))) * 1.96,
    Payoff_CI=(sd(Payoff)/sqrt(length(Payoff))) * 1.96,   
    Payoff=mean(Payoff)
    )
  Rank_Data=unique(select(b, Rank_Payoff, Round, Choice, Av_Dis, Payoff, Choice_CI, Dis_CI, Payoff_CI))
  #------------------------------------------------------------------------------------------
  if(drawgraphs){
    par(mfrow=c(1,1))
    plot(Rank_Data$Round[Rank_Data$Rank_Payoff==1], Rank_Data$Payoff[Rank_Data$Rank_Payoff==1],
         ylim=c(0,130), type="b", lwd=3, 
         cex.axis=1.2, cex.lab=1.5,
         xlab="Round", ylab="Payoff", 
         xaxt="n")
    axis(1, 1:20, 1:20)
    #--------------------------------------
    errbar(Rank_Data$Round[Rank_Data$Rank_Payoff==1], Rank_Data$Payoff[Rank_Data$Rank_Payoff==1],
           yplus  = Rank_Data$Payoff[Rank_Data$Rank_Payoff==1] + Rank_Data$Payoff_CI[Rank_Data$Rank_Payoff==1],
           yminus = Rank_Data$Payoff[Rank_Data$Rank_Payoff==1] - Rank_Data$Payoff_CI[Rank_Data$Rank_Payoff==1],
           add=T)
    errbar(          Rank_Data$Round[Rank_Data$Rank_Payoff==2],  Rank_Data$Payoff[Rank_Data$Rank_Payoff==2],
           yplus  = Rank_Data$Payoff[Rank_Data$Rank_Payoff==2] + Rank_Data$Payoff_CI[Rank_Data$Rank_Payoff==2],
           yminus = Rank_Data$Payoff[Rank_Data$Rank_Payoff==2] - Rank_Data$Payoff_CI[Rank_Data$Rank_Payoff==2],
           add=T, type="l", lwd=3, col=2, errbar.col = 2)
    errbar(          Rank_Data$Round[Rank_Data$Rank_Payoff==3],  Rank_Data$Payoff[Rank_Data$Rank_Payoff==3],
                     yplus  = Rank_Data$Payoff[Rank_Data$Rank_Payoff==3] + Rank_Data$Payoff_CI[Rank_Data$Rank_Payoff==3],
                     yminus = Rank_Data$Payoff[Rank_Data$Rank_Payoff==3] - Rank_Data$Payoff_CI[Rank_Data$Rank_Payoff==3],
                     add=T, type="b", lwd=3, col=3, errbar.col = 3)
    errbar(          Rank_Data$Round[Rank_Data$Rank_Payoff==4],  Rank_Data$Payoff[Rank_Data$Rank_Payoff==4],
                     yplus  = Rank_Data$Payoff[Rank_Data$Rank_Payoff==4] + Rank_Data$Payoff_CI[Rank_Data$Rank_Payoff==4],
                     yminus = Rank_Data$Payoff[Rank_Data$Rank_Payoff==4] - Rank_Data$Payoff_CI[Rank_Data$Rank_Payoff==4],
                     add=T, type="b", lwd=3, col=4, errbar.col = 4)
    #================================================================
    plot(Rank_Data$Round[Rank_Data$Rank_Payoff==1], Rank_Data$Choice[Rank_Data$Rank_Payoff==1],
         ylim=c(0,7), type="b", lwd=3, 
         cex.axis=1.2, cex.lab=1.5,
         xlab="Round", ylab="Effort Level", 
         xaxt="n")
    axis(1, 1:20, 1:20)
    #--------------------------------------
    errbar(Rank_Data$Round[Rank_Data$Rank_Payoff==1], Rank_Data$Choice[Rank_Data$Rank_Payoff==1],
           yplus  = Rank_Data$Choice[Rank_Data$Rank_Payoff==1] + Rank_Data$Choice_CI[Rank_Data$Rank_Payoff==1],
           yminus = Rank_Data$Choice[Rank_Data$Rank_Payoff==1] - Rank_Data$Choice_CI[Rank_Data$Rank_Payoff==1],
           add=T)
    errbar(          Rank_Data$Round[Rank_Data$Rank_Payoff==2],  Rank_Data$Choice[Rank_Data$Rank_Payoff==2],
                     yplus  = Rank_Data$Choice[Rank_Data$Rank_Payoff==2] + Rank_Data$Choice_CI[Rank_Data$Rank_Payoff==2],
                     yminus = Rank_Data$Choice[Rank_Data$Rank_Payoff==2] - Rank_Data$Choice_CI[Rank_Data$Rank_Payoff==2],
                     add=T, type="l", lwd=3, col=2, errbar.col = 2)
    errbar(          Rank_Data$Round[Rank_Data$Rank_Payoff==3],  Rank_Data$Choice[Rank_Data$Rank_Payoff==3],
                     yplus  = Rank_Data$Choice[Rank_Data$Rank_Payoff==3] + Rank_Data$Choice_CI[Rank_Data$Rank_Payoff==3],
                     yminus = Rank_Data$Choice[Rank_Data$Rank_Payoff==3] - Rank_Data$Choice_CI[Rank_Data$Rank_Payoff==3],
                     add=T, type="b", lwd=3, col=3, errbar.col = 3)
    errbar(          Rank_Data$Round[Rank_Data$Rank_Payoff==4],  Rank_Data$Choice[Rank_Data$Rank_Payoff==4],
                     yplus  = Rank_Data$Choice[Rank_Data$Rank_Payoff==4] + Rank_Data$Choice_CI[Rank_Data$Rank_Payoff==4],
                     yminus = Rank_Data$Choice[Rank_Data$Rank_Payoff==4] - Rank_Data$Choice_CI[Rank_Data$Rank_Payoff==4],
                     add=T, type="b", lwd=3, col=4, errbar.col = 4)
  }
  #------------------------------------------------------
  #------------------------------------------------------
  a=group_by(d1a, Rank_Payoff)
  b=mutate(a,
           Choice=mean(Effort_level),
           Choice_CI=(sd(Effort_level)/sqrt(length(Effort_level))) * 1.96,
           Av_Dis=mean(Distance),
           Dis_CI=(sd(Distance)/sqrt(length(Distance))) * 1.96,
           Payoff_CI=(sd(Payoff)/sqrt(length(Payoff))) * 1.96,   
           Payoff=mean(Payoff)
  )
  Rank_Data=unique(select(b, Rank_Payoff, Choice, Av_Dis, Payoff, Choice_CI, Dis_CI, Payoff_CI))
  
  if(drawgraphs){
    plot(Rank_Data$Rank_Payoff, Rank_Data$Choice, ylim=c(1,7))
    errbar(Rank_Data$Rank_Payoff, Rank_Data$Choice,
           yplus = Rank_Data$Choice + Rank_Data$Choice_CI,
           yminus= Rank_Data$Choice - Rank_Data$Choice_CI,
           add=T)
    #-------------------------------------------------------------
    plot(Rank_Data$Rank_Payoff, Rank_Data$Payoff, ylim=c(1, 130))
    errbar(Rank_Data$Rank_Payoff, Rank_Data$Payoff,
           yplus = Rank_Data$Payoff + Rank_Data$Payoff_CI,
           yminus= Rank_Data$Payoff - Rank_Data$Payoff_CI,
           add=T)
  }
  #------------------------------------------------------
  #Compute Average Behavior
  a=group_by(d1a, Round)
  b=mutate(a, Choice=mean(Effort_level),
              Choice_CI=(sd(Effort_level)/sqrt(length(Effort_level))) * 1.96,
              Av_Dis=mean(Distance),
              Dis_CI=(sd(Distance)/sqrt(length(Distance))) * 1.96,
              Payoff_CI=(sd(Payoff)/sqrt(length(Payoff))) * 1.96,   
              Payoff=mean(Payoff)
                   )
  Agg_Data=unique(select(b, Round, Choice, Av_Dis, Payoff, Choice_CI, Dis_CI, Payoff_CI))

}
#------------------------------------------------------------------------------------------
if(drawgraphs){
  par(mfrow=c(1,1))
  plot(Agg_Data$Round, Agg_Data$Choice,
       ylim=c(0,7), type="b", lwd=3, 
       cex.axis=1.2, cex.lab=1.5,
       xlab="Round", ylab="Effort Level", 
       xaxt="n")
  axis(1, 1:20, 1:20)
  #--------------------------------------
  errbar(Agg_Data$Round, Agg_Data$Choice,
         yplus  = Agg_Data$Choice + Agg_Data$Choice_CI,
         yminus = Agg_Data$Choice - Agg_Data$Choice_CI,
         add=T)
}
#========================================
a=group_by(d1a, Group)
b=mutate(a, 
         Sub_Group=ifelse( mean(Effort_level[Round == 20 ]) <= 4, 1,2),
         mean_id= mean(Effort_level[Round == 20]),
         min_id = min(Effort_level[Round == 20]),
         max_id = max(Effort_level[Round == 20])
         )
d1a=b

# this is where the final edit to d1a is made

if(!Just_d1a){
  
  #--------------------------------
  if(drawgraphs){
    hist(d1a$mean_id, xlim=c(0,7))
    hist(d1a$min_id, xlim=c(0,7))
    hist(d1a$max_id, xlim=c(0,7))
  }
  #--------------------
  a=group_by(d1a, Sub_Group, Round)
  b=mutate(a, Choice=mean(Effort_level),
           Choice_CI=(sd(Effort_level)/sqrt(length(Effort_level))) * 1.96,
           Av_Dis=mean(Distance),
           Dis_CI=(sd(Distance)/sqrt(length(Distance))) * 1.96,
           Payoff_CI=(sd(Payoff)/sqrt(length(Payoff))) * 1.96,   
           Payoff=mean(Payoff)
  )
  Sub_Data=unique(select(b, Sub_Group, Round, Choice, Av_Dis, Payoff, Choice_CI, Dis_CI, Payoff_CI))
  #------------------------------------------------------
  if(drawgraphs){
    plot(Sub_Data$Round[Sub_Data$Sub_Group==1], Sub_Data$Choice[Sub_Data$Sub_Group==1],
         ylim=c(0,7), type="b", lwd=3, 
         cex.axis=1.2, cex.lab=1.5,
         xlab="Round", ylab="Effort Level", 
         xaxt="n")
    axis(1, 1:20, 1:20)
    #--------------------------------------
    errbar(Sub_Data$Round[Sub_Data$Sub_Group==1], Sub_Data$Choice[Sub_Data$Sub_Group==1],
           yplus  = Sub_Data$Choice[Sub_Data$Sub_Group==1] + Sub_Data$Choice_CI[Sub_Data$Sub_Group==1],
           yminus = Sub_Data$Choice[Sub_Data$Sub_Group==1] - Sub_Data$Choice_CI[Sub_Data$Sub_Group==1],
           add=T)
    #------------
    errbar(Sub_Data$Round[Sub_Data$Sub_Group==2], Sub_Data$Choice[Sub_Data$Sub_Group==2],
           yplus  = Sub_Data$Choice[Sub_Data$Sub_Group==2] + Sub_Data$Choice_CI[Sub_Data$Sub_Group==2],
           yminus = Sub_Data$Choice[Sub_Data$Sub_Group==2] - Sub_Data$Choice_CI[Sub_Data$Sub_Group==2],
           add=T, col=2, errbar.col=2, type="b", lwd=2)
    #========================================
    plot(Agg_Data$Round, Agg_Data$Av_Dis,
         ylim=c(0,4), type="b", lwd=3,
         xlab="Round", ylab="Distance From Minimum Choice")
    errbar(Agg_Data$Round, Agg_Data$Av_Dis,
           yplus  = Agg_Data$Av_Dis + Agg_Data$Dis_CI,
           yminus = Agg_Data$Av_Dis - Agg_Data$Dis_CI,
           add=T)
    #--------------------------------------
    plot(Agg_Data$Round, Agg_Data$Payoff,
         ylim=c(0,130), type="b", lwd=3,
         xlab="Round", ylab="Payoff")
    errbar(Agg_Data$Round, Agg_Data$Payoff,
           yplus  = Agg_Data$Payoff + Agg_Data$Payoff_CI,
           yminus = Agg_Data$Payoff - Agg_Data$Payoff_CI,
           add=T)
  }
  #=======================================================
  #=======================================================
  #Compute Average Behavior per Condition
  a=group_by(d1a, Condition, Round)
  b=mutate(a, Choice=mean(Effort_level),
               Av_Dis=mean(Distance),
               Payoff=mean(Payoff)
           )
  Agg_Data=unique(select(b, Condition, Round, Choice, Av_Dis, Payoff))
  #----------
  if(drawgraphs){
    plot(Agg_Data$Round[Agg_Data$Condition==1], Agg_Data$Choice[Agg_Data$Condition==1],
         ylim=c(0,7), type="b", lwd=3, col=1,
         xlab="Round", ylab="Effort Level")
    lines(Agg_Data$Round[Agg_Data$Condition==2], Agg_Data$Choice[Agg_Data$Condition==2],
          col=2, type="b", lwd=3)
    
    t.test(Agg_Data$Choice[Agg_Data$Condition==1], Agg_Data$Choice[Agg_Data$Condition==2])
    #----------------------------
    plot(Agg_Data$Round[Agg_Data$Condition==1], Agg_Data$Av_Dis[Agg_Data$Condition==1],
         ylim=c(0,7), type="b", lwd=3, col=1,
         xlab="Round", ylab="Effort Level")
    lines(Agg_Data$Round[Agg_Data$Condition==2], Agg_Data$Av_Dis[Agg_Data$Condition==2],
          col=2, type="b", lwd=3)
    #----------------------------
    plot(Agg_Data$Round[Agg_Data$Condition==1], Agg_Data$Payoff[Agg_Data$Condition==1],
         ylim=c(0,130), type="b", lwd=3, col=1,
         xlab="Round", ylab="Effort Level")
    lines(Agg_Data$Round[Agg_Data$Condition==2], Agg_Data$Payoff[Agg_Data$Condition==2],
          col=2, type="b", lwd=3)
  }
  #=======================================================
  #Compute Average Behavior per Condition
  a=group_by(d1a, Group, Round)
  b=mutate(a, Choice=mean(Effort_level),
              Payoff=mean(Payoff)
           )
  Agg_Data=unique(select(b, Group, Round, Choice, Payoff))
  #this is the last instance of agg_data
  # after this is optional for EWA model fitting
  if(!Just_Agg_Data){
    
    #------------------------------------------------
    if(drawgraphs){
    for(a in 1: length(unique(d1a$Group)) ){
      a=1
          group_num=a
      if(a==1){
      
        plot(Agg_Data$Round[Agg_Data$Group==group_num], Agg_Data$Payoff[Agg_Data$Group==group_num],
           ylim=c(0,130), type="l", lwd=1, col=1,
           xlab="Round", ylab="Effort Level", cex.lab=1.5, cex.axis=1.2,
           xaxt="n")
        axis(1, 1:20, 1:20)
        
        } else {lines(Agg_Data$Round[Agg_Data$Group==group_num], Agg_Data$Payoff[Agg_Data$Group==group_num],col=a, lwd=1)}
      
        }
      #=======================================================
      #=======================================================
      plot(0,0)
      legend(-1,1, legend=c("MEG First", "MEG Second"), col=1:2, lwd=3)
    }
    #=======================================================
    #=======================================================
    a=group_by(d1a, Group, Round)
    b=mutate(a, Distance = Effort_level - min(Effort_level) )
    d1a=b
    #--------
    a=group_by(d1a, Subject_ID)
    b=mutate(a, mean_dis=mean(Distance), sd_dis=sd(Distance),
                First_Round=Effort_level[Round==1],
                Payoff=mean(Payoff)
             )
    I_Measure=unique(select(b, Subject_ID, mean_dis, sd_dis, Payoff, Industriousness_scale, Risk_aversion_scale_shift, 
                            Risk_aversion_scale_total, Brief_self_control_scale, Tolerance_mental_effort_scale, 
                            Trait_trust_scale, Need_for_cognition_scale, Tolerance_exercise_scale, First_Round, Rank_Payoff)) 
    #--------
    #Average Distance
    
    #--------
    if(drawgraphs){
      plot(I_Measure$mean_dis, I_Measure$Industriousness_scale,
           main=paste( round(cor(I_Measure$mean_dis, I_Measure$Industriousness_scale), 2) ))
      #------
      plot(I_Measure$mean_dis, I_Measure$Risk_aversion_scale_shift,
      main=paste( round(cor(I_Measure$mean_dis, I_Measure$Risk_aversion_scale_shift), 2) ))
      #------
      plot(I_Measure$mean_dis, I_Measure$Risk_aversion_scale_total,
           main=paste( round(cor(I_Measure$mean_dis, I_Measure$Risk_aversion_scale_total), 2) ))
      #------
      plot(I_Measure$mean_dis, I_Measure$Brief_self_control_scale,
           main=paste( round(cor(I_Measure$mean_dis, I_Measure$Brief_self_control_scale), 2) ))
      #------
      plot(I_Measure$mean_dis, I_Measure$Tolerance_mental_effort_scale,
           main=paste( round(cor(I_Measure$mean_dis, I_Measure$Tolerance_mental_effort_scale), 2) ))
      #------
      plot(I_Measure$mean_dis, I_Measure$Trait_trust_scale,
           main=paste( round(cor(I_Measure$mean_dis, I_Measure$Trait_trust_scale), 2) ))
      #------
      plot(I_Measure$mean_dis, I_Measure$Need_for_cognition_scale,
           main=paste( round(cor(I_Measure$mean_dis, I_Measure$Need_for_cognition_scale), 2) ))
      #------
      plot(I_Measure$mean_dis, I_Measure$Tolerance_exercise_scale,
           main=paste( round(cor(I_Measure$mean_dis, I_Measure$Tolerance_exercise_scale), 2) ))
      #------
    }
    #--------
    #Standard Deviation of Distance
    
    if(drawgraphs){
      #--------
      plot(I_Measure$sd_dis, I_Measure$Industriousness_scale,
           main=paste( round(cor(I_Measure$sd_dis, I_Measure$Industriousness_scale), 2) ))
      #------
      plot(I_Measure$sd_dis, I_Measure$Risk_aversion_scale_shift,
           main=paste( round(cor(I_Measure$sd_dis, I_Measure$Risk_aversion_scale_shift), 2) ))
      #------
      plot(I_Measure$sd_dis, I_Measure$Risk_aversion_scale_total,
           main=paste( round(cor(I_Measure$sd_dis, I_Measure$Risk_aversion_scale_total), 2) ))
      #------
      plot(I_Measure$sd_dis, I_Measure$Brief_self_control_scale,
           main=paste( round(cor(I_Measure$sd_dis, I_Measure$Brief_self_control_scale), 2) ))
      #------
      plot(I_Measure$sd_dis, I_Measure$Tolerance_mental_effort_scale,
           main=paste( round(cor(I_Measure$sd_dis, I_Measure$Tolerance_mental_effort_scale), 2) ))
      #------
      plot(I_Measure$sd_dis, I_Measure$Trait_trust_scale,
           main=paste( round(cor(I_Measure$sd_dis, I_Measure$Trait_trust_scale), 2) ))
      #------
      plot(I_Measure$sd_dis, I_Measure$Need_for_cognition_scale,
           main=paste( round(cor(I_Measure$sd_dis, I_Measure$Need_for_cognition_scale), 2) ))
      #------
      plot(I_Measure$sd_dis, I_Measure$Tolerance_exercise_scale,
           main=paste( round(cor(I_Measure$sd_dis, I_Measure$Tolerance_exercise_scale), 2) ))
      #------
    }
    #--------
    #First Round
    if(drawgraphs){
      #--------
      plot(I_Measure$First_Round, I_Measure$Industriousness_scale,
           main=paste( round(cor(I_Measure$First_Round, I_Measure$Industriousness_scale), 2) ))
      #------
      plot(I_Measure$First_Round, I_Measure$Risk_aversion_scale_shift,
           main=paste( round(cor(I_Measure$First_Round, I_Measure$Risk_aversion_scale_shift), 2) ))
      #------
      plot(I_Measure$First_Round, I_Measure$Risk_aversion_scale_total,
           main=paste( round(cor(I_Measure$First_Round, I_Measure$Risk_aversion_scale_total), 2) ))
      #------
      plot(I_Measure$First_Round, I_Measure$Brief_self_control_scale,
           main=paste( round(cor(I_Measure$First_Round, I_Measure$Brief_self_control_scale), 2) ))
      #------
      plot(I_Measure$First_Round, I_Measure$Tolerance_mental_effort_scale,
           main=paste( round(cor(I_Measure$First_Round, I_Measure$Tolerance_mental_effort_scale), 2) ))
      #------
      plot(I_Measure$First_Round, I_Measure$Trait_trust_scale,
           main=paste( round(cor(I_Measure$First_Round, I_Measure$Trait_trust_scale), 2) ))
      #------
      plot(I_Measure$First_Round, I_Measure$Need_for_cognition_scale,
           main=paste( round(cor(I_Measure$First_Round, I_Measure$Need_for_cognition_scale), 2) ))
      #------
      plot(I_Measure$First_Round, I_Measure$Tolerance_exercise_scale,
           main=paste( round(cor(I_Measure$First_Round, I_Measure$Tolerance_exercise_scale), 2) ))
      #------
    }
    #--------
    #Payoff
    if(drawgraphs){
      #--------
      plot(I_Measure$Payoff, I_Measure$Industriousness_scale,
           main=paste( round(cor(I_Measure$Payoff, I_Measure$Industriousness_scale), 2) ))
      #------
      plot(I_Measure$Payoff, I_Measure$Risk_aversion_scale_shift,
           main=paste( round(cor(I_Measure$Payoff, I_Measure$Risk_aversion_scale_shift), 2) ))
      #------
      plot(I_Measure$Payoff, I_Measure$Risk_aversion_scale_total,
           main=paste( round(cor(I_Measure$Payoff, I_Measure$Risk_aversion_scale_total), 2) ))
      #------
      plot(I_Measure$Payoff, I_Measure$Brief_self_control_scale,
           main=paste( round(cor(I_Measure$Payoff, I_Measure$Brief_self_control_scale), 2) ))
      #------
      plot(I_Measure$Payoff, I_Measure$Tolerance_mental_effort_scale,
           main=paste( round(cor(I_Measure$Payoff, I_Measure$Tolerance_mental_effort_scale), 2) ))
      #------
      plot(I_Measure$Payoff, I_Measure$Trait_trust_scale,
           main=paste( round(cor(I_Measure$Payoff, I_Measure$Trait_trust_scale), 2) ))
      #------
      plot(I_Measure$Payoff, I_Measure$Need_for_cognition_scale,
           main=paste( round(cor(I_Measure$Payoff, I_Measure$Need_for_cognition_scale), 2) ))
      #------
      plot(I_Measure$Payoff, I_Measure$Tolerance_exercise_scale,
           main=paste( round(cor(I_Measure$Payoff, I_Measure$Tolerance_exercise_scale), 2) ))
      #------
    }
    #--------
    #Payoff Rank
    if(drawgraphs){
      #--------
      plot(I_Measure$Rank_Payoff, I_Measure$Industriousness_scale,
           main=paste( round(cor(I_Measure$Rank_Payoff, I_Measure$Industriousness_scale), 2) ))
      #------
      plot(I_Measure$Rank_Payoff, I_Measure$Risk_aversion_scale_shift,
           main=paste( round(cor(I_Measure$Rank_Payoff, I_Measure$Risk_aversion_scale_shift), 2) ))
        #------
       plot(I_Measure$Rank_Payoff, I_Measure$Risk_aversion_scale_total,
           main=paste( round(cor(I_Measure$Rank_Payoff, I_Measure$Risk_aversion_scale_total), 2) ))
       
    
      plot(I_Measure$Rank_Payoff, I_Measure$Risk_aversion_scale_total,
            main=paste( round(cor(I_Measure$Rank_Payoff, I_Measure$Risk_aversion_scale_total), 2) ))
      #------
      plot(I_Measure$Rank_Payoff, I_Measure$Brief_self_control_scale,
           main=paste( round(cor(I_Measure$Rank_Payoff, I_Measure$Brief_self_control_scale), 2) ))
      #------
      plot(I_Measure$Rank_Payoff, I_Measure$Tolerance_mental_effort_scale,
           main=paste( round(cor(I_Measure$Rank_Payoff, I_Measure$Tolerance_mental_effort_scale), 2) ))
      #------
      plot(I_Measure$Rank_Payoff, I_Measure$Trait_trust_scale,
           main=paste( round(cor(I_Measure$Rank_Payoff, I_Measure$Trait_trust_scale), 2) ))
      #------
      plot(I_Measure$Rank_Payoff, I_Measure$Need_for_cognition_scale,
           main=paste( round(cor(I_Measure$Rank_Payoff, I_Measure$Need_for_cognition_scale), 2) ))
      #------
      plot(I_Measure$Rank_Payoff, I_Measure$Tolerance_exercise_scale,
           main=paste( round(cor(I_Measure$Rank_Payoff, I_Measure$Tolerance_exercise_scale), 2) ))
      #------
    }
    #--------
    #Payoff Rank (Between Lowest Payoff and Highest Payoff)
    if(drawgraphs){
      #--------
      plot(I_Measure$Rank_Payoff[I_Measure$Rank_Payoff %in% c(1,4)], I_Measure$Industriousness_scale[I_Measure$Rank_Payoff %in% c(1,4)],
           main=paste( round(cor(I_Measure$Rank_Payoff[I_Measure$Rank_Payoff %in% c(1,4)], I_Measure$Industriousness_scale[I_Measure$Rank_Payoff %in% c(1,4)]), 2) ))
      #------
      plot(I_Measure$Rank_Payoff[I_Measure$Rank_Payoff %in% c(1,4)], I_Measure$Risk_aversion_scale_shift[I_Measure$Rank_Payoff %in% c(1,4)],
           main=paste( round(cor(I_Measure$Rank_Payoff[I_Measure$Rank_Payoff %in% c(1,4)], I_Measure$Risk_aversion_scale_shift[I_Measure$Rank_Payoff %in% c(1,4)]), 2) ))
      #------
      plot(I_Measure$Rank_Payoff[I_Measure$Rank_Payoff %in% c(1,4)], I_Measure$Risk_aversion_scale_total[I_Measure$Rank_Payoff %in% c(1,4)],
           main=paste( round(cor(I_Measure$Rank_Payoff[I_Measure$Rank_Payoff %in% c(1,4)], I_Measure$Risk_aversion_scale_total[I_Measure$Rank_Payoff %in% c(1,4)]), 2) ))
      
      plot(I_Measure$Rank_Payoff[I_Measure$Rank_Payoff %in% c(1,4)], I_Measure$Risk_aversion_scale_total[I_Measure$Rank_Payoff %in% c(1,4)],
           main=paste( round(cor(I_Measure$Rank_Payoff[I_Measure$Rank_Payoff %in% c(1,4)], I_Measure$Risk_aversion_scale_total[I_Measure$Rank_Payoff %in% c(1,4)]), 2) ))
      #------
      plot(I_Measure$Rank_Payoff[I_Measure$Rank_Payoff %in% c(1,4)], I_Measure$Brief_self_control_scale[I_Measure$Rank_Payoff %in% c(1,4)],
           main=paste( round(cor(I_Measure$Rank_Payoff[I_Measure$Rank_Payoff %in% c(1,4)], I_Measure$Brief_self_control_scale[I_Measure$Rank_Payoff %in% c(1,4)]), 2) ))
      #------
      plot(I_Measure$Rank_Payoff[I_Measure$Rank_Payoff %in% c(1,4)], I_Measure$Tolerance_mental_effort_scale[I_Measure$Rank_Payoff %in% c(1,4)],
           main=paste( round(cor(I_Measure$Rank_Payoff[I_Measure$Rank_Payoff %in% c(1,4)], I_Measure$Tolerance_mental_effort_scale[I_Measure$Rank_Payoff %in% c(1,4)]), 2) ))
      #------
      plot(I_Measure$Rank_Payoff[I_Measure$Rank_Payoff %in% c(1,4)], I_Measure$Trait_trust_scale[I_Measure$Rank_Payoff %in% c(1,4)],
           main=paste( round(cor(I_Measure$Rank_Payoff[I_Measure$Rank_Payoff %in% c(1,4)], I_Measure$Trait_trust_scale[I_Measure$Rank_Payoff %in% c(1,4)]), 2) ))
      #------
      plot(I_Measure$Rank_Payoff[I_Measure$Rank_Payoff %in% c(1,4)], I_Measure$Need_for_cognition_scale[I_Measure$Rank_Payoff %in% c(1,4)],
           main=paste( round(cor(I_Measure$Rank_Payoff[I_Measure$Rank_Payoff %in% c(1,4)], I_Measure$Need_for_cognition_scale[I_Measure$Rank_Payoff %in% c(1,4)]), 2) ))
      #------
      plot(I_Measure$Rank_Payoff[I_Measure$Rank_Payoff %in% c(1,4)], I_Measure$Tolerance_exercise_scale[I_Measure$Rank_Payoff %in% c(1,4)],
           main=paste( round(cor(I_Measure$Rank_Payoff[I_Measure$Rank_Payoff %in% c(1,4)], I_Measure$Tolerance_exercise_scale[I_Measure$Rank_Payoff %in% c(1,4)]), 2) ))
      #------
    }
    #############################################################
    #############################################################
    #############################################################
    #############################################################
    if(drawgraphs){
      plot(I_Measure$Rank_Payoff[I_Measure$Rank_Payoff %in% c(1,4)], I_Measure$Risk_aversion_scale_shift[I_Measure$Rank_Payoff %in% c(1,4)],
           main=paste( round(cor(I_Measure$Rank_Payoff[I_Measure$Rank_Payoff %in% c(1,4)], I_Measure$Risk_aversion_scale_shift[I_Measure$Rank_Payoff %in% c(1,4)]), 2) ))
      
      t.test(I_Measure$Rank_Payoff[I_Measure$Rank_Payoff %in% c(1,4)], I_Measure$Risk_aversion_scale_shift[I_Measure$Rank_Payoff %in% c(1,4)])
    }
    #############################################################
    #############################################################
    a=group_by(d1a, Group)
    b=mutate(a, 
                Average_Choice=mean(Effort_level),
                Average_First=mean(Effort_level[Round==1]),
                av_Payoff=mean(Payoff),
                av_Dis=mean(Distance),
                av_Dis_r1=mean(Distance[Round==1]),
                  One_Freq=length(Effort_level[Effort_level==1]),
                Seven_Freq=length(Effort_level[Effort_level==7])
             )
    d1a=b
    Groups=unique(select(b, Group, Average_Choice, Average_First, av_Payoff, One_Freq, Seven_Freq, av_Dis,av_Dis_r1))
    #----------------------------------------------------------------
    if(drawgraphs){
      hist(Groups$Average_Choice, breaks=seq(0,7,1))
      plot(density(Groups$Average_Choice))
      #-------------------------------------------------------
      hist(Groups$Average_First, breaks=seq(0,7,1))
      plot(density(Groups$Average_First, breaks=seq(0,7,1)))
      #-------------------------------------------------------
      hist(Groups$av_Payoff, breaks=seq(0,200,1))
      plot(density(Groups$av_Payoff))
      #-------------------------------------------------------
      hist(Groups$av_Dis)
      plot(density(Groups$av_Dis, breaks=seq(0,7,1)))
      #-------------------------------------------------------
      hist(Groups$av_Dis_r1)
      plot(density(Groups$av_Dis_r1, breaks=seq(0,7,1)))
      #-------------------------------------------------------
      hist(Groups$One_Freq, breaks=seq(0,50,1))
      #-------------------------------------------------------
      hist(Groups$Seven_Freq, breaks=seq(0,72,1))
      #-------------------------------------------------
      plot(density(Groups$One_Freq), xlim=c(0,80))
      plot(density(Groups$Seven_Freq), xlim=c(0,80))
      #-------------------------------------------------
      abline(v=c(30,60))
    }
    #d1a$Category=ifelse(d1a$Average_Choice < 3,1,ifelse(d1a$Average_Choice > 3 & d1a$Average_Choice < 6,2,3))
    #d1a$Category=ifelse(d1a$av_Dis_r1 <= 1,1,ifelse(d1a$av_Dis_r1 > 1 & d1a$av_Dis_r1 <= 2.5,2,3))
    #d1a$Category=ifelse(d1a$One_Freq < 20,1,2)
    #d1a$Category=ifelse(d1a$Seven_Freq < 30,1, ifelse(d1a$Seven_Freq > 30 & d1a$Seven_Freq < 60, 2,3))
    #----------------------------------------------------------------
    
    a=group_by(d1a, Category, Round)
    b=mutate(a, 
             Choice=mean(Effort_level),
             av_dis=mean(Distance),
             av_Payoff=mean(Payoff),
             N_Groups=length(unique(Group))
    )
    Category=unique(select(b, Round, Category, Choice, av_dis, av_Payoff, N_Groups))
    
    if(drawgraphs){
      plot(Category$Round[Category$Category==1], Category$Choice[Category$Category==1], 
           ylim=c(0,7), type="b", col=1, lwd=3)
      lines(Category$Round[Category$Category==2], Category$Choice[Category$Category==2],
            col=2, type="b", lwd=3)
      lines(Category$Round[Category$Category==3], Category$Choice[Category$Category==3],
            col=4, type="b", lwd=3)
    }
    #----------------------------------------------------------------------------------
    a=group_by(d1a, Round)
    b=mutate(a, 
           One_Freq = length(Effort_level[Effort_level==1])/length(Effort_level),
         Seven_Freq = length(Effort_level[Effort_level==7])/length(Effort_level)
             )
    Freq=unique(select(b, Round, One_Freq, Seven_Freq))
    if(drawgraphs){
      plot(Freq$Round, Freq$One_Freq, ylim=c(0,1))
      plot(Freq$Round, Freq$Seven_Freq, ylim=c(0,1))
    }
    #======================================================================================
    #======================================================================================
    #======================================================================================
    #======================================================================================
    #======================================================================================
    #======================================================================================
  }
}

#getting rid of extraneous variables for elegance's sake

rm(a)
rm(b)
rm(drawgraphs)
rm(Just_Agg_Data)
rm(Just_d1a)