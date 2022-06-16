library(sjPlot)
library(psych)
library(tidyverse)
library(naniar)
library(caret)
library(plotly)
#library(flexclust)
#library(fmsb)
library(mgcv)
library(srvyr)
library(ggpubr)
library(factoextra)
library(FactoMineR)
library(dplyr)
library(dvmisc)

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

percentilerank<-function(x){
  rx<-rle(sort(x))
  smaller<-cumsum(c(0, rx$lengths))[seq(length(rx$lengths))]
  larger<-rev(cumsum(c(0, rev(rx$lengths))))[-1]
  rxpr<-smaller/(smaller+larger)
  rxpr[match(x, rx$values)]
}

pisa = `Pisa Russia`

view_df(pisa)

bulling_vars = c("ST038Q03NA", "ST038Q04NA", "ST038Q05NA", "ST038Q06NA", "ST038Q07NA", "ST038Q08NA")

bulling_vars_new = c("ST038Q04NA", "ST038Q05NA", "ST038Q06NA", "ST038Q07NA", "ST038Q08NA")



pisa_sel = 
  pisa %>%
  select(1:10, STRATUM, SUBNATIO, W_FSTUWT,
         contains("ST034"), #Sense of belonging at school ***
         contains('ST097'), #disciplinary climate, ***
         contains("ST213"), #teacher enthusiasm ***
         contains("ST206"), #Perception of cooperation at school ***
         contains("ST205"), # ***  Perception of competition at school - i.e., those students that perceive their school as competetive, tend to be victims of bullying
         contains("ST181"), #personal competetiveness ***,
         contains("ST188"), #Students’ self-efficacy -----
         contains("ST183"), #fear of failure/unconfidence ***
         contains("ST186"), #students' feelings: happy, joyful, cheerful (index of posiitve feelings), lively, proud, scared, miserable, afraid and sad
         ST016Q01NA, #life satisfaction scale (from 7 starts satisfied) ***
         contains("ST185"), #students' sense of meaning in life (Eudaemonia) ***
         contains("ST062"), #students' truancy and lateness ***
         REPEAT, # 0-non repeaters and 1 repeaters ????---- * include into model with cogn skills
         contains("ST208"), #goal-orientation -- -*
         contains("ST182"), #task-mastery -- ---
         contains("ST207"), #anti-bullying
         all_of(bulling_vars), 
         ST005Q01TA, ST006Q01TA, ST006Q02TA, ST019AQ01T, ST019BQ01T, ST019CQ01T,ST022Q01TA, 
         contains("MATH"), contains("READ"), contains("SCIE"), 
         BEINGBULLIED, #include for comparison
         AGE, PROGN, ST004D01T,
         HISCED, #highest level of parental education
         HISCED_D, HISEI, 
         UNDREM, METASUM, METASPAM, #meta cognitive vars
         IMMIG, BSMJ, 
         HOMEPOS, CULTPOSS, WEALTH, ESCS)%>% #!!!!!!! - include these variables as socio-economic model part
  #generate first set of variables - socio-demographic and economic
  mutate(Migrant = ifelse(IMMIG %in% c(2,3), 1, 0 ),
         Migrant_First_Generation = ifelse(IMMIG ==3, 1, 0 ),
         Migrant_Second_Generation = ifelse(IMMIG ==2, 1, 0 ),
         home_language = case_when(ST022Q01TA == 1 ~  "Russian",
                                   ST022Q01TA == 2 ~  "Foreign",
                                   TRUE ~ as.character(NA)),
         Sex = ST004D01T,
         life_satisfaction = ST016Q01NA/10) %>% #need to be included in psych model
  #now produce vars for cognitive block
  mutate(Mathematics = (PV1MATH+PV2MATH+PV3MATH+PV4MATH+PV5MATH+PV6MATH+PV7MATH+PV8MATH+PV9MATH+PV10MATH)/10,
         Reading = (PV1READ+PV2READ+PV3READ+PV4READ+PV5READ+PV6READ+PV7READ+PV8READ+PV9READ+PV10READ)/10,
         Science = (PV1SCIE+PV2SCIE+PV3SCIE+PV4SCIE+PV5SCIE+PV6SCIE+PV7SCIE+PV8SCIE+PV9SCIE+PV10SCIE)/10,
         Math_Ordinal = case_when(Mathematics < 420 ~ "Low performers",
                                  Mathematics >=420 & Mathematics <607 ~ "Medium performers", #607
                                  Mathematics >=607 ~ "High performers"), #607
         Reading_Ordinal = case_when(Reading < 407 ~ "Low performers",
                                     Reading >=497 & Reading <626 ~ "Medium performers", #626
                                     Reading >=626 ~ "High performers"), #626
         Science_Ordinal = case_when(Science < 410 ~ "Low performers",
                                     Science >=410 & Science <633 ~ "Medium performers", #633
                                     Science >=633 ~ "High performers")) %>%
  #we need to set all 5,7,8,9 to NAs before standardizing
  replace_with_na_all(condition = ~.x %in% c(5,7,8,9, 95, 97, 98, 99)) %>%
  #start new block of vars that are index-based
  mutate(bullying_raw = rowMeans(select(., bulling_vars_new), na.rm = T),
         bullying = scale(rowMeans(select(., bulling_vars_new), na.rm = T),center = TRUE, scale = TRUE),
         b_w = weights::stdz(bullying_raw, weight = W_FSTUWT),
         bullying_attitude = scale(rowMeans(select(., starts_with("ST207")), na.rm = T),center = TRUE, scale = TRUE),
         school_belonging = scale(rowMeans(select(., starts_with("ST034")), na.rm = T),center = TRUE, scale = TRUE),
         eudaemonia = scale(rowMeans(select(., starts_with("ST185")), na.rm = T),center = TRUE, scale = TRUE),
         disciplinary_climate = scale(rowMeans(select(., starts_with("ST097")), na.rm = T),center = TRUE, scale = TRUE),
         teacher_enthusiasm = scale(rowMeans(select(., starts_with("ST213")), na.rm = T),center = TRUE, scale = TRUE),
         perception_of_cooperation = scale(rowMeans(select(., starts_with("ST206")), na.rm = T),center = TRUE, scale = TRUE),
         perception_of_competition = scale(rowMeans(select(., starts_with("ST205")), na.rm = T),center = TRUE, scale = TRUE),
         competitiveness = scale(rowMeans(select(., starts_with("ST181")), na.rm = T),center = TRUE, scale = TRUE),
         fear_of_failure = scale(rowMeans(select(., starts_with("ST183")), na.rm = T),center = TRUE, scale = TRUE),
         truancy_and_lateness = scale(rowMeans(select(., starts_with("ST062")), na.rm = T),center = TRUE, scale = TRUE),
         goal_orientation = scale(rowMeans(select(., starts_with("ST208")), na.rm = T),center = TRUE, scale = TRUE),
         task_mastery = scale(rowMeans(select(., starts_with("ST182")), na.rm = T),center = TRUE, scale = TRUE), 
         #predictors of bullying 
         disciplinary_climate_positive = ifelse(disciplinary_climate > 0, 1, 0),
         school_env_coop = ifelse(perception_of_cooperation > 0, 1, 0),
         school_env_comp = ifelse( perception_of_competition > 0, 1, 0),
         school_belonging_high = cut(school_belonging, breaks = quantile(school_belonging, probs = c(0, 0.75, 1), na.rm = T), include.lowest = TRUE, labels = c(1, 0)),
         #index_of_positive_feelings = scale(rowMeans(select(., c("ST186Q05HA", "ST186Q01HA", "ST186Q03HA")), na.rm = T),center = TRUE, scale = TRUE),           scared = scale(ST186Q06HA ,center = TRUE, scale = TRUE),
         #model variables (bullying as predictor)
         joyfull = ifelse(ST186Q01HA==4, 1, 0), 
         happy = ifelse(ST186Q05HA==4, 1, 0), 
         cheerful = ifelse(ST186Q03HA==4, 1, 0),  
         miserable = ifelse(ST186Q10HA==4, 1,0), 
         afraid = ifelse(ST186Q02HA==4, 1, 0), 
         sad = ifelse(ST186Q08HA==4, 1, 0),  
         lively = ifelse(ST186Q07HA==4 ,1, 0),
         proud = ifelse(ST186Q09HA==4, 1, 0),
         life_satisfaction_low = ifelse(life_satisfaction <= quantile(life_satisfaction, probs = 0.25, na.rm = T), 1, 0),
         eudaemonia_high = ifelse(eudaemonia >= quantile(eudaemonia, probs = 0.75, na.rm = T), 1, 0),    
         competitiveness_low = ifelse(competitiveness <= quantile(competitiveness, probs = 0.25, na.rm = T), 1, 0),
         fear_of_failure_high = ifelse(fear_of_failure>=quantile(fear_of_failure, probs = 0.75, na.rm = T), 1, 0),
         goal_orientation_high = ifelse(goal_orientation >= quantile(goal_orientation, probs = 0.75, na.rm = T), 1, 0),
         task_mastery_high = ifelse(task_mastery >= quantile(task_mastery, probs = 0.75, na.rm = T), 1, 0),
         truancy_high = ifelse(task_mastery >= quantile(truancy_and_lateness, probs = 0.75, na.rm = T), 1, 0),
         bullying_attitude_high = ifelse(bullying_attitude >= quantile(competitiveness, probs = 0.75, na.rm = T), 1, 0),
         #other variables
         weight = W_FSTUWT/1000) %>%
  drop_na(bullying) %>%
  select(-c(starts_with("ST"), starts_with("PV")))

#summary(pisa_sel$b_w)
#summary(pisa_sel$bullying)


summary(pisa_sel$disciplinary_climate_low)

set.seed(124)
a2 = kmeans(pisa_sel$bullying, centers = 2, iter.max = 100)
pisa_sel$cluster2 = a2$cluster

set.seed(124)
a3 = kmeans(pisa_sel$b_w, centers = 2, iter.max = 100)
pisa_sel$cluster3 = a3$cluster

#summary(factor(pisa_sel$cluster2))

pisa_final =   pisa_sel %>%   mutate(Victim = ifelse(cluster2 == 2, 1, 0))


saveRDS(pisa_final, "Pisa_Transformed.rds")