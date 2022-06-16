Pisa_Transformed 

library(mgcv)
library(visreg)
library(tidyverse)
library(voxel)
library(lmerTest)
library(mgcViz)
library(gamm4)


Pisa_Transformed$Sex = as.factor(Pisa_Transformed$Sex)
Pisa_Transformed$REPEAT = as.factor(Pisa_Transformed$REPEAT)
Pisa_Transformed$Migrant = as.factor(Pisa_Transformed$Migrant)
Pisa_Transformed$home_language = as.factor(Pisa_Transformed$home_language)
Pisa_Transformed$HISCED = as.factor(Pisa_Transformed$HISCED)


Pisa_Transformed$poor <- ifelse(Pisa_Transformed$Wealth_Quintile %in% c(1,2), 1,0)




GAM <- gam(Victim~Sex+REPEAT+s(Mathematics, bs="cr")+s(Reading, bs="cr")+s(Science, bs="cr")+UNDREM+METASUM+METASPAM, data=Pisa_Transformed, method="REML")  

GAM <- gam(Victim~Sex+s(Mathematics, bs="cs")+
             s(Reading, bs="cs")+s(Science, bs="cs"), data=Pisa_Transformed, method="REML")  

# cognitive model
GAM_cognitive <- gam(bullying~Sex+s(Mathematics, bs="cs")+s(Reading, bs="cs")+
                       s(Science, bs="cs"), data=Pisa_Transformed, method="REML")  

summary(GAM)
par(mfrow=c(2,2))
visreg(GAM)



#version with mgcViz
b <- getViz(GAM_cognitive)
print(plot(b, allTerms = T), pages = 1) # Calls print.plotGam()

pl <- 
  plot(b, allTerms = T) + 
  l_points(col = "blue", size = 10) + l_fitLine(linetype = 1) + 
  l_fitContour() + 
  l_ciLine(colour = "black") + l_ciBar() + 
  l_fitPoints(size = 1, col = "black") + theme_get() + 
  labs(title = NULL)
pl$empty # FALSE: because we added gamLayers
par(mfrow=c(2,2))
print(pl)


# economic model
GAM_econom <- gam(Victim~Sex+s(ESCS, bs="cs")+home_language, data=Pisa_Transformed, method="REML")  
GAM_econom <- gam(bullying~Sex+s(ESCS, bs="cs")+home_language, data=Pisa_Transformed, method="REML")  


summary(GAM_econom)
par(mfrow=c(3,3))
visreg(GAM_econom)

c <- getViz(GAM_econom)
print(plot(c, allTerms = T), pages = 1) # Calls print.plotGam()

pl <- 
  plot(c, allTerms = T) + 
  l_points(col = "blue", size = 10) + l_fitLine(linetype = 1) + 
  l_fitContour() + 
  l_ciLine(colour = "black") + l_ciBar() + 
  l_fitPoints(size = 1, col = "black") + theme_get() + 
  labs(title = NULL)
pl$empty # FALSE: because we added gamLayers
par(mfrow=c(2,2))
print(pl)

# psych model

GAM_psych <- gam(bullying~Sex+s(life_satisfaction, bs="cs")+s(school_belonging, bs="cs")+s(disciplinary_climate, bs="cs")
                 +s(teacher_enthusiasm, bs="cs")+s(perception_of_cooperation, bs="cs")+s(perception_of_competition, bs="cs")
                 +s(competitiveness, bs="cs")+s(fear_of_failure, bs="cs")+s(eudaemonia, bs="cs")
                 +s(truancy_and_lateness, bs="cs")+s(goal_orientation, bs="cs")+s(task_mastery, bs="cs")
                 +s(anti_bullying, bs="cs"), data=Pisa_Transformed, method="REML") 
summary(GAM_psych)
par(mfrow=c(4,4))
plot(GAM_psych)

## feelings model

GAM_feelings <- gam(bullying~Sex+s(index_of_positive_feelings, bs="cs")
                 +s(miserable, bs="cs", k=3)+s(afraid, bs="cs", k=3)+s(sad, bs="cs", k=3)+s(lively, bs="cs", k=3)
                 +s(proud, bs="cs", k=3), data=Pisa_Transformed, method="REML") 
summary(GAM_feelings)
par(mfrow=c(3,3))
visreg(GAM_feelings)
plot.gam(GAM_feelings)


eudaemonia = scale(rowMeans(select(., starts_with("ST185")), na.rm = T),center = TRUE, scale = TRUE),           
truancy_and_lateness = scale(rowMeans(select(., starts_with("ST062")), na.rm = T),center = TRUE, scale = TRUE),
goal_orientation = scale(rowMeans(select(., starts_with("ST208")), na.rm = T),center = TRUE, scale = TRUE),
task_mastery = scale(rowMeans(select(., starts_with("ST182")), na.rm = T),center = TRUE, scale = TRUE),                

