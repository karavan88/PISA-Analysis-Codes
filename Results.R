library(haven)

#descriptive statistics
table = 
  pisa %>% 
  select(all_of(bulling_vars_new),W_FSTUWT) %>% 
  drop_na() %>%
  mutate(id = 1:nrow(.)) %>%
  gather(Bullying, Value, bulling_vars_new)%>%
  rename(weight = W_FSTUWT) %>%
  mutate(Value = case_when(Value == 1 ~ "Almost_never",
                           Value == 2 ~ "Per_year",
                           Value == 3 ~ "Per_month",
                           Value == 4 ~ "Per_week"),
         Value2 = Value) %>%
  spread(Value, Value2) %>%
  mutate_at(vars(contains("_")), ~ case_when(!is.na(.) ~ 1, TRUE ~ 0)) %>%
  as_survey_design(weight = weight) %>%
  group_by(Bullying) %>%
  summarise("Almost_Never" = survey_mean(Almost_never),
            "Per_year" = survey_mean(Per_year),
            "Per_month" = survey_mean(Per_month),
            "Per_week" = survey_mean(Per_week)) %>%
  select(-contains("_se")) 


#find the percentile for the mean value
quantile(pisa_final$b_w, probs =0.6955)

ggplot(pisa_final, aes(b_w))+
  geom_density(color="darkblue", fill="lightblue", aes(weight = W_FSTUWT))+
  geom_vline(aes(xintercept=mean(bullying, na.rm = T)), linetype="dashed", show.legend = F)+
  geom_vline(aes(xintercept=median(bullying, na.rm = T)), linetype="dotted", show.legend = F)+
  theme_minimal()+
  xlab("Index of Exposure to Bullying")+
  ylab("")

### check the difference between two indices
ggplot(pisa_final, aes(b_w, BEINGBULLIED))+
  geom_point(aes(weight = W_FSTUWT))

cor(pisa_final$b_w, pisa_final$BEINGBULLIED, method = "pearson", use = "complete.obs")
summary(lm(pisa_final$b_w~pisa_final$BEINGBULLIED))

prevalence = pisa_final %>% as_survey_design(weight = W_FSTUWT) %>% summarise(survey_mean(Victim))

#profiling

school = 
  read_sav("school_questionnaire.sav") %>% 
  filter(CNT == "RUS") %>% 
  select(CNTSCHID, SC001Q01TA) %>% 
  mutate(City = ifelse(SC001Q01TA==4, 1, 0),
         Large_City = ifelse(SC001Q01TA==5, 1, 0)) %>%
  as.data.frame()

victims = 
  pisa_final %>%
  mutate(Male = ifelse(Sex==2, 1, 0),
         Low_status = ifelse(ESCS<= quantile(ESCS, probs = 0.4, na.rm = T), 1, 0)) %>%
  filter(Victim == 1) %>%
  left_join(school) %>%
  as_survey_design(weight = W_FSTUWT) 

sex_male = victims %>% summarise(survey_mean(Male)) 
low_status = victims %>% summarise(survey_mean(Low_status, na.rm = T)) 
city = victims %>% summarise(survey_mean(City, na.rm = T)) 
large_city = victims %>% summarise(survey_mean(Large_City, na.rm = T)) 

#### -------------------------   MODELING
pisa_final <- readRDS("C:/Users/kavanesyan/OneDrive - UNICEF/R codes/PISA Analysis/Pisa_Transformed.rds")



##loop the regression

dependent_var = c("joyfull", "happy", "cheerful", "miserable", 
                  "afraid","sad", "lively", 'proud',
                  "life_satisfaction_low", "eudaemonia_high", "competitiveness_low", "fear_of_failure_high",
                  "goal_orientation_high", "task_mastery_high", "truancy_high", "bullying_attitude_high")

models <- lapply(dependent_var, function(x) {
  m = gam(substitute(i ~ Victim, list(i = as.name(x))), data = pisa_final, 
      method="REML", weights = weight)
})

coefs = c(models[[1]]$coefficients[2], models[[2]]$coefficients[2],
          models[[3]]$coefficients[2],models[[4]]$coefficients[2],
          models[[5]]$coefficients[2],models[[6]]$coefficients[2],
          models[[7]]$coefficients[2],models[[8]]$coefficients[2],
          models[[9]]$coefficients[2],models[[10]]$coefficients[2],
          models[[11]]$coefficients[2],models[[12]]$coefficients[2],
          models[[13]]$coefficients[2],models[[14]]$coefficients[2],
          models[[15]]$coefficients[2],models[[16]]$coefficients[2])

emotions = data.frame(dependent_var, coefs) %>% mutate(coefs = round(coefs, 2)) %>% rename(var = dependent_var)


#### preictors of bullying

#cognitive predictors
#no_high = pisa_final %>% mutate(low = ifelse(Reading_Ordinal == "low", 1,0))

cognitive_model = gam(Victim~Science_Ordinal+Math_Ordinal+Reading_Ordinal, data = pisa_final, method="REML", weights = W_FSTUWT)
summary(cognitive_model)

#prob_1 <- exp(coef(r)[1]+coef(r)[2])/(1+exp(coef(r)[1]+coef(r)[2]))
#prob_0 <- exp(coef(r)[1])/(1+exp(coef(r)[1]))

#prob_1 - prob_0


#school_env_model

school_env = gam(Victim~school_belonging_high+disciplinary_climate_positive+school_env_coop+
           school_env_comp, data = pisa_final, method="REML", weights = W_FSTUWT)
summary(school_env)
broom::tidy(school_env)
coef(summary(school_env))

marginal_effects = 
  
se = school_env$coefficients %>% as.data.frame %>% rownames_to_column(var = "var") 
se$coefs = se$.

lo = cognitive_model$coefficients %>% as.data.frame %>% rownames_to_column(var = "var") 
lo$coefs = lo$.

margins = 
  emotions %>%
  bind_rows(lo, se) %>%
  select(var, coefs) %>%
  filter(var != "Intercept") %>%
  mutate(coefs = round(coefs, 2))


write.csv(margins, "margins.csv", row.names = F)

