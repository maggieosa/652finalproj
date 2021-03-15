##### PLOT ONE #######

library(tidyverse)
library(haven)

# dataprep
weight2011 <- here::here("data", "2011_weight_history.XPT") %>%
  read_xpt()

dep2011 <-  here::here("data", "2011_depression_screener.XPT") %>%
  read_xpt()

demo2011 <- here::here("data", "2011_demographics.XPT") %>%
  read_xpt()

dep1 <- left_join(weight2011,dep2011)
dep <- left_join(dep1, demo2011)

#removing "refuse to answer"
dep <- dep %>%
  filter((WHD020 != 7777 & WHD020 != 9999), 
         (WHD045 != 77777 & WHD045 != 99999))

#new variables: depression mean 

dep <- dep %>%
  rowwise() %>%
  mutate(dpq = sum(c(DPQ010, DPQ020, DPQ030, DPQ040, DPQ050,
                     DPQ060, DPQ070, DPQ080,DPQ090, DPQ100)))%>%
  drop_na(dpq)


#actual-ideal weight discrepency variable
dep <- dep %>%
  mutate(dif_weight = WHD045 - WHD020)


#removing men 
dep_filtered <- dep %>%
  filter(RIAGENDR == 2,
         dif_weight <=0)%>%
  mutate(mean_dpq = mean(dpq)) %>%
  select(RIAGENDR,dif_weight,mean_dpq, SEQN, dpq)


#creating factor variable for actual-ideal weight

dep$dif_weight_fct <-Hmisc::cut2(dep$dif_weight,c(-125,-100, -75,
                                                  -50, -25))

dep$dif_weight_fct <- as.factor(dep$dif_weight_fct)


#renaming levels
dep_lev <- dep %>%
  mutate(dif_weight_fct = fct_recode(dif_weight_fct, 
                                     "More than\n125 lbs" = "[-240,-125)", 
                                     "100-125" = "[-125,-100)",
                                     "75-100" = "[-100, -75)",
                                     "50-75" = "[ -75, -50)",
                                     "25-50" = "[ -50, -25)",
                                     "0-25" = "[ -25, 100]"),
         dif_weight_fct= fct_relevel(dif_weight_fct, 
                                   "0-25", "25-50", "50-75", 
                                   "75-100", "100-125", "More than\n125 lbs"))



#mean by factor
plot1_df <-dep_lev %>%
  group_by(dif_weight_fct) %>%
  summarise(group_dpq = mean(dpq),
            group_se = sd(dpq)/sqrt(n()))



write_csv(plot1_df,
          file = here::here('data', 'plot1_df.csv'))

write_csv(dep_filtered,
          file = here::here('data', 'dep_filtered.csv'))


### PLOT TWO ###
#data prep
secure2015 <- here::here("data", "2015_food_security.XPT") %>%
  read_xpt()

dep2015 <-  here::here("data", "2015_depression_screener.XPT") %>%
  read_xpt()

secure_dep <- left_join(secure2015, dep2015)


secure_dep <- secure_dep %>%
  rowwise() %>%
  mutate(dpq_sum = sum(c(DPQ010, DPQ020, DPQ030, DPQ040, DPQ050,
                         DPQ060, DPQ070, DPQ080,DPQ090, DPQ100))) %>%
  drop_na(dpq_sum) 

secure_group <- secure_dep %>%
  group_by(FSDHH) %>%
  summarise(dpq_group = mean(dpq_sum)) %>%
  drop_na(FSDHH)



#factors
secure_dep$FSDHH <- as.factor(secure_dep$FSDHH)
secure_group$FSDHH <- as.factor(secure_group$FSDHH)

write_csv(secure_group,
          file = here::here('data', 'secure_group.csv'))

write_csv(secure_dep,
          file = here::here('data', 'secure_dep.csv'))


### PLOT THREE ####

#data prep

weight2005 <- here::here("data", "2005_weight_history.XPT") %>%
  read_xpt()

weight2007 <- here::here("data", "2007_weight_history.XPT") %>%
  read_xpt()

weight2009 <- here::here("data", "2009_weight_history.XPT") %>%
  read_xpt()

weight2011 <- here::here("data", "2011_weight_history.XPT") %>%
  read_xpt()

weight2013 <- here::here("data", "2013_weight_history.XPT") %>%
  read_xpt()

weight2015 <- here::here("data", "2015_weight_history.XPT") %>%
  read_xpt()

###foodinsecure


food2005 <- here::here("data", "2005_food_security.XPT") %>%
  read_xpt()

food2007 <- here::here("data", "2007_food_security.XPT") %>%
  read_xpt()

food2009 <- here::here("data", "2009_food_security.XPT") %>%
  read_xpt()

food2011 <- here::here("data", "2011_food_security.XPT") %>%
  read_xpt()

food2013 <- here::here("data", "2013_food_security.XPT") %>%
  read_xpt()

food2015 <- here::here("data", "2015_food_security.XPT") %>%
  read_xpt()

#joining
yr2005 <- left_join(weight2005, food2005)
yr2007 <- left_join(weight2007, food2007)
yr2009 <- left_join(weight2009, food2009)
yr2011 <- left_join(weight2011, food2011)
yr2013 <- left_join(weight2013, food2013)
yr2015 <- left_join(weight2015, food2015)

#adding years, narrowing data


yr2005<- yr2005 %>%
  add_column(year = 2005) %>%
  select(WHD080A, WHD080C, WHD080E,
         WHD080G, WHD080J, WHD080K,
         WHD080M, WHD080O, FSDHH, year, FSDAD)

yr2007 <- yr2007 %>%
  add_column(year = 2007)%>%
  select(WHD080A, WHD080C, WHD080E,
         WHD080G, WHD080J, WHD080K,
         WHD080M, WHD080O, FSDHH, year, FSDAD)

yr2009 <- yr2009 %>%
  add_column(year = 2009)%>%
  select(WHD080A, WHD080C, WHD080E,
         WHD080G, WHD080J, WHD080K,
         WHD080M, WHD080O, FSDHH, year, FSDAD)

yr2011 <- yr2011 %>%
  add_column(year = 2011)%>%
  select(WHD080A, WHD080C, WHD080E,
         WHD080G, WHD080J, WHD080K,
         WHD080M, WHD080O, FSDHH, year, FSDAD)

yr2013 <- yr2013 %>%
  add_column(year = 2013)%>%
  select(WHD080A, WHD080C, WHD080E,
         WHD080G, WHD080J, WHD080K,
         WHD080M, WHD080O, FSDHH, year, FSDAD)

yr2015 <- yr2015%>%
  add_column(year = 2015)%>%
  select(WHD080A, WHD080C, WHD080E,
         WHD080G, WHD080J, WHD080K,
         WHD080M, WHD080O, FSDHH, year, FSDAD)




full_yr <- yr2015 %>%
  add_row(yr2005) %>%
  add_row(yr2007) %>%
  add_row(yr2009) %>%
  add_row(yr2011) %>%
  add_row(yr2013)

#recoding: 1 = presence of ed behavior    
full_yr$WHD080A[which(!is.na(full_yr$WHD080A))] <-1    
full_yr$WHD080C[which(!is.na(full_yr$WHD080C))] <-1  
full_yr$WHD080E[which(!is.na(full_yr$WHD080E))] <-1  
full_yr$WHD080G[which(!is.na(full_yr$WHD080G))] <-1  
full_yr$WHD080J[which(!is.na(full_yr$WHD080J))] <-1  
full_yr$WHD080K[which(!is.na(full_yr$WHD080K))] <-1  
full_yr$WHD080M[which(!is.na(full_yr$WHD080M))] <-1  
full_yr$WHD080O[which(!is.na(full_yr$WHD080O))] <-1  

#create ed composite   
full_yr <- full_yr %>%
  rowwise() %>%
  mutate(ed_sum = sum(WHD080A, WHD080C, WHD080E,
                      WHD080G, WHD080J, WHD080K,
                      WHD080M, WHD080O, na.rm = T))

full_df <- full_yr %>%
  group_by(FSDHH, year) %>%
  summarise(ed_mean = mean(ed_sum, na.rm = T))

full_df <- full_df[-c(25:30),]

full_df$FSDHH<- as.factor(full_df$FSDHH)  


#testing model
full_yr$FSDHH <- as.factor(full_yr$FSDHH)  
full_yr <- full_yr %>%
  rowwise() %>%
  mutate(ed_mean = mean(ed_sum, na.rm = T))

#recoding factors, creating percentage
full_yr_grouped <- full_df %>%
  mutate(FSDHH = fct_recode(FSDHH,
                            "Full" = "1",
                            "Marginal" = "2",
                            "Low" = "3",
                            "Very Low" = "4"),
         ed_mean = ed_mean/5)

write_csv(full_yr_grouped,
          file = here::here('data', 'full_yr_grouped.csv'))
write_csv(full_df,
          file = here::here('data', 'full_df.csv'))

###PLOT FOUR###

#data prep
demo2015 <- here::here("data", "2015_demographics.XPT") %>%
  read_xpt()

weight2015 <- here::here("data", "2015_weight_history.XPT") %>%
  read_xpt()

food2015 <- here::here("data", "2015_food_security.XPT") %>%
  read_xpt()

foodwt15 <- left_join(weight2015, food2015)
race_full <- left_join(foodwt15, demo2015)

race_full <- race_full %>%
  select(WHD080A, WHD080C, WHD080E,
         WHD080G, WHD080J, WHD080K,
         WHD080M, WHD080O, FSDHH, FSDAD, 
         INDFMIN2, INDFMPIR, INDHHIN2, RIDRETH3, SEQN)

#recoding ED behaviors
race_full$WHD080A[which(!is.na(race_full$WHD080A))] <-1    
race_full$WHD080C[which(!is.na(race_full$WHD080C))] <-1  
race_full$WHD080E[which(!is.na(race_full$WHD080E))] <-1  
race_full$WHD080G[which(!is.na(race_full$WHD080G))] <-1  
race_full$WHD080J[which(!is.na(race_full$WHD080J))] <-1  
race_full$WHD080K[which(!is.na(race_full$WHD080K))] <-1  
race_full$WHD080M[which(!is.na(race_full$WHD080M))] <-1  
race_full$WHD080O[which(!is.na(race_full$WHD080O))] <-1  



race_summ <- race_full %>%
  group_by(RIDRETH3, FSDHH) %>%
  summarise(freq1 = sum(WHD080A, na.rm = T)/n(),
            freq2 = sum(WHD080C, na.rm = T)/n(),
            freq3 = sum(WHD080E, na.rm = T)/n(),
            freq4 = sum(WHD080G, na.rm = T)/n(),
            freq5 = sum(WHD080J, na.rm = T)/n(),
            freq6 = sum(WHD080K, na.rm = T)/n(),
            freq7 = sum(WHD080M, na.rm = T)/n(),
            freq8 = sum(WHD080O, na.rm = T)/n())

long <- race_summ %>%
  pivot_longer(!c(FSDHH, RIDRETH3), 
               names_to = "behavior", 
               values_to = "frequency")

#recoding factors 


long_plot_draft <- long %>%
  mutate_at(c("FSDHH", "RIDRETH3","behavior"), as.factor) %>%
  mutate(FSDHH = fct_recode(FSDHH,
                            "Full" = "1",
                            "Marginal" = "2",
                            "Low" = "3",
                            "Very Low" = "4"))

#recoding factors 
long_plot <- long %>%
  mutate_at(c("FSDHH", "RIDRETH3","behavior"), as.factor) %>%
  mutate(FSDHH = fct_recode(FSDHH,
                            "Full" = "1",
                            "Marginal" = "2",
                            "Low" = "3",
                            "Very Low" = "4"),
         RIDRETH3 = fct_recode(RIDRETH3, 
                               "Mexican American" = "1",
                               "Other Hispanic" = "2",
                               "Non-Hispanic White" = "3",
                               "Non-Hispanic Black" = "4",
                               "Non-Hispanic Asian" = "6",
                               "Other Race/Multiracial" = "7"),
         behavior = fct_recode(behavior,
                               "Ate Less Overall" = "freq1",
                               "Ate Less Fat" = "freq2",
                               "Skipped Meals" = "freq3",
                               "Liquid Diet" = "freq4",
                               "nonRx Diet Pills" = "freq5",
                               "Purging" = "freq6",
                               "Water Loading" = "freq7",
                               "Carb Restriction" = "freq8")) %>%
  drop_na()

write_csv(long_plot,
          file = here::here('data', 'long_plot.csv'))
write_csv(long_plot_draft,
          file = here::here('data', 'long_plot_draft.csv'))

###PLOT FIVE ###

#data prep

weight2005 <- here::here("data", "2005_weight_history.XPT") %>%
  read_xpt()

weight2007 <- here::here("data", "2007_weight_history.XPT") %>%
  read_xpt()

weight2009 <- here::here("data", "2009_weight_history.XPT") %>%
  read_xpt()

weight2011 <- here::here("data", "2011_weight_history.XPT") %>%
  read_xpt()

weight2013 <- here::here("data", "2013_weight_history.XPT") %>%
  read_xpt()

weight2015 <- here::here("data", "2015_weight_history.XPT") %>%
  read_xpt()

###demographics

demo2005 <- here::here("data", "2005_demographics.XPT") %>%
  read_xpt()

demo2007 <- here::here("data", "2007_demographics.XPT") %>%
  read_xpt()

demo2009 <- here::here("data", "2009_demographics.XPT") %>%
  read_xpt()

demo2011 <- here::here("data", "2011_demographics.XPT") %>%
  read_xpt()

demo2013 <- here::here("data", "2013_demographics.XPT") %>%
  read_xpt()

demo2015 <- here::here("data", "2015_demographics.XPT") %>%
  read_xpt()


#joining
year1 <- left_join(weight2005, demo2005)
year2 <- left_join(weight2007, demo2007)
year3 <- left_join(weight2009, demo2009)
year4 <- left_join(weight2011, demo2011)
year5 <- left_join(weight2013, demo2013)
year6 <- left_join(weight2015, demo2015)

#creating full years

year1<- year1 %>%
  add_column(year = 2005) %>%
  select(WHD080A, WHD080C, WHD080E,
         WHD080G, WHD080J, WHD080K,
         WHD080M, WHD080O, RIDRETH1, year)

year2 <- year2 %>%
  add_column(year = 2007)%>%
  select(WHD080A, WHD080C, WHD080E,
         WHD080G, WHD080J, WHD080K,
         WHD080M, WHD080O,RIDRETH1, year)

year3 <- year3 %>%
  add_column(year = 2009)%>%
  select(WHD080A, WHD080C, WHD080E,
         WHD080G, WHD080J, WHD080K,
         WHD080M, WHD080O, RIDRETH1, year)

year4 <- year4 %>%
  add_column(year = 2011)%>%
  select(WHD080A, WHD080C, WHD080E,
         WHD080G, WHD080J, WHD080K,
         WHD080M, WHD080O, RIDRETH1, year)

year5 <- year5 %>%
  add_column(year = 2013)%>%
  select(WHD080A, WHD080C, WHD080E,
         WHD080G, WHD080J, WHD080K,
         WHD080M, WHD080O, RIDRETH1, year)

year6 <- year6%>%
  add_column(year = 2015)%>%
  select(WHD080A, WHD080C, WHD080E,
         WHD080G, WHD080J, WHD080K,
         WHD080M, WHD080O, RIDRETH1, year)


race_weight <- year6 %>%
  add_row(year5) %>%
  add_row(year4) %>%
  add_row(year3) %>%
  add_row(year2) %>%
  add_row(year1)

#recoding: 1 = presence of ed behavior    
race_weight$WHD080A[which(!is.na(race_weight$WHD080A))] <-1    
race_weight$WHD080C[which(!is.na(race_weight$WHD080C))] <-1  
race_weight$WHD080E[which(!is.na(race_weight$WHD080E))] <-1  
race_weight$WHD080G[which(!is.na(race_weight$WHD080G))] <-1  
race_weight$WHD080J[which(!is.na(race_weight$WHD080J))] <-1  
race_weight$WHD080K[which(!is.na(race_weight$WHD080K))] <-1  
race_weight$WHD080M[which(!is.na(race_weight$WHD080M))] <-1  
race_weight$WHD080O[which(!is.na(race_weight$WHD080O))] <-1  


#create ed composite   
race_weight<- race_weight %>%
  rowwise() %>%
  mutate(ed_sum = sum(WHD080A, WHD080C, WHD080E,
                      WHD080G, WHD080J, WHD080K,
                      WHD080M, WHD080O, na.rm = T))

race_weight_group <- race_weight %>%
  group_by(RIDRETH1, year) %>%
  summarise(ed_mean_race = mean(ed_sum, na.rm = T))

race_weight_group$RIDRETH1 <- as.factor(race_weight_group$RIDRETH1)

race_weight_labeled <- race_weight_group %>%
  mutate(RIDRETH1 = fct_recode(RIDRETH1, 
                               "Mexican American" = "1",
                               "Other Hispanic" = "2",
                               "Non-Hispanic White" = "3",
                               "Non-Hispanic Black" = "4",
                               "Other Race/Multiracial" = "5"),
         ed_mean_race = ed_mean_race/5)

#reorder
race_weight_labeled$RIDRETH1<- 
  factor(race_weight_labeled$RIDRETH1,
         levels = c("Non-Hispanic Black",
                    "Other Hispanic",
                    "Mexican American",
                    "Non-Hispanic White",
                    "Other Race/Multiracial")) 

write_csv(race_weight_labeled,
          file = here::here('data', 'race_weight_labeled.csv'))
write_csv(race_weight_group,
          file = here::here('data', 'race_weight_group.csv'))


