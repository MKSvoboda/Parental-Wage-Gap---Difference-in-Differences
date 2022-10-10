# REPORT EDITING SCRIPT ROUGH


#loading packages


library(tidyverse)
library(vroom)
library(broom)
setwd("D:/Google Drive/MSc Applied Social Data Analysis/Social Data Science and Policy Analytics SOCM034/Assessment 3 Big Paper/wage gap")


###### loading data

# people with kids born after 5th and before 6th wave
#newborn5 <- vroom("D:/Desktop/DASS3 Data/UKDA-6614-tab/tab/ukhls_w6/f_newborn.tab", 
#                  col_select = c("pidp"))

# time stable characteristics of all people
#crosswave <- vroom("D:/Desktop/DASS3 Data/UKDA-6614-tab/tab/ukhls_wx/xwavedat.tab",
#                   col_select = c("pidp", "anychild_dv"))

#indresp1 <- vroom("D:/Desktop/DASS3 Data/UKDA-6614-tab/tab/ukhls_w1/a_indresp.tab",
#                  col_select = c("a_sex_dv", "a_birthy", "pidp","a_fimnlabgrs_dv"))

#indresp2 <- vroom("D:/Desktop/DASS3 Data/UKDA-6614-tab/tab/ukhls_w2/b_indresp.tab",
#                  col_select = c("pidp", "b_fimnlabgrs_dv"))

#indresp3 <- vroom("D:/Desktop/DASS3 Data/UKDA-6614-tab/tab/ukhls_w3/c_indresp.tab",
#                  col_select = c("pidp", "c_fimnlabgrs_dv"))

#indresp4 <- vroom("D:/Desktop/DASS3 Data/UKDA-6614-tab/tab/ukhls_w4/d_indresp.tab",
#                  col_select = c("pidp", "d_fimnlabgrs_dv"))

#indresp5 <- vroom("D:/Desktop/DASS3 Data/UKDA-6614-tab/tab/ukhls_w5/e_indresp.tab",
#                  col_select = c("pidp", "e_fimnlabgrs_dv"))

#indresp6 <- vroom("D:/Desktop/DASS3 Data/UKDA-6614-tab/tab/ukhls_w6/f_indresp.tab",
#                  col_select = c("pidp", "f_fimnlabgrs_dv"))

#indresp7 <- vroom("D:/Desktop/DASS3 Data/UKDA-6614-tab/tab/ukhls_w7/g_indresp.tab",
#                  col_select = c("pidp", "g_fimnlabgrs_dv"))

#indresp8 <- vroom("D:/Desktop/DASS3 Data/UKDA-6614-tab/tab/ukhls_w8/h_indresp.tab",
#                  col_select = c("pidp", "h_fimnlabgrs_dv"))

#indresp9 <- vroom("D:/Desktop/DASS3 Data/UKDA-6614-tab/tab/ukhls_w9/i_indresp.tab",
#                  col_select = c("pidp", "i_fimnlabgrs_dv"))

#indresp10 <- vroom("D:/Desktop/DASS3 Data/UKDA-6614-tab/tab/ukhls_w10/j_indresp.tab",
#                   col_select = c("pidp", "j_fimnlabgrs_dv"))

#indresp11 <- vroom("D:/Desktop/DASS3 Data/UKDA-6614-tab/tab/ukhls_w11/k_indresp.tab",
#                   col_select = c("pidp", "k_fimnlabgrs_dv"))


# joining datasets of people who had children between w5 and w6, creating a variable to show tht they are parents
joinedparents <- inner_join(newborn5,indresp1, by = "pidp") %>%
  inner_join(indresp2, by = "pidp") %>%
  inner_join(indresp3, by = "pidp") %>%
  inner_join(indresp4, by = "pidp") %>%
  inner_join(indresp5, by = "pidp") %>%
  inner_join(indresp6, by = "pidp") %>%
  inner_join(indresp7, by = "pidp") %>%
  inner_join(indresp8, by = "pidp") %>%
  inner_join(indresp9, by = "pidp") %>%
  inner_join(indresp10, by = "pidp") %>%
  inner_join(indresp11, by = "pidp")

joinedparents$parent <- 1

# newborn5 dataset is used to identify people who had a child between waves 5 and 6,
# now we need data to identify a similar number of people who are childless for control


joinedchildless <- inner_join(filter(crosswave, anychild_dv == 2),indresp1, by = "pidp") %>%
  inner_join(indresp2, by = "pidp") %>%
  inner_join(indresp3, by = "pidp") %>%
  inner_join(indresp4, by = "pidp") %>%
  inner_join(indresp5, by = "pidp") %>%
  inner_join(indresp6, by = "pidp") %>%
  inner_join(indresp7, by = "pidp") %>%
  inner_join(indresp8, by = "pidp") %>%
  inner_join(indresp9, by = "pidp") %>%
  inner_join(indresp10, by = "pidp") %>%
  inner_join(indresp11, by = "pidp")

joinedchildless$parent <- 0


# removing observations to establish the same maximum age in both parent and childless datasets
summary(joinedparents$a_birthy)
joinedchildless <- filter(joinedchildless, a_birthy > 1960)
summary(joinedchildless$a_birthy)
# Joining parent and childless datasets

joined <- full_join(joinedparents, joinedchildless)


########## removing duplicates

joined <- distinct(joined, pidp, .keep_all = TRUE)

#removing negative values
joined[joined < 0] <- NA

#########################################################################################
###reshaping into wider format

joinedlong <- joined %>%
  pivot_longer(c("a_fimnlabgrs_dv", "b_fimnlabgrs_dv", "c_fimnlabgrs_dv", "d_fimnlabgrs_dv", "e_fimnlabgrs_dv",
                 "f_fimnlabgrs_dv", "g_fimnlabgrs_dv", "h_fimnlabgrs_dv", "i_fimnlabgrs_dv", "j_fimnlabgrs_dv", "k_fimnlabgrs_dv"),
               names_to = "variable", values_to = "value") %>%
  separate(variable, into = c("wave", "variable"), sep = "_", extra = "merge") %>%
  pivot_wider(names_from = variable, values_from = value)


# recoding variables

joinedlong <- joinedlong %>%
  mutate(wave = case_when(
    wave == "a" ~ 1,
    wave == "b" ~ 2,
    wave == "c" ~ 3,
    wave == "d" ~ 4,
    wave == "e" ~ 5,
    wave == "f" ~ 6,
    wave == "g" ~ 7,
    wave == "h" ~ 8,
    wave == "i" ~ 9,
    wave == "j" ~ 10,
    wave == "k" ~ 11))

joinedlong <- joinedlong %>% mutate(sex = ifelse(a_sex_dv == 1, "male", ifelse(a_sex_dv == 2, "female", NA)))
joinedlong <- rename(joinedlong, income = fimnlabgrs_dv)

joinedlong <- joinedlong %>%
  mutate(time = case_when(
  wave >= 7 ~ 1,
  wave <= 5 ~ 0))

joinedlong <- joinedlong %>%
  mutate(time2 = case_when(
    wave >= 9 ~ 1,
    wave <= 5 ~ 0))

joinedlong$female <- ifelse(joinedlong$a_sex_dv == 1, 0, 1)

joinedlong$parent2 <- ifelse(joinedlong$parent == 0, "Childless", "Parents")


joinedlongnon0 <- joinedlong

joinedlongnon0$income[joinedlongnon0$income == 0] <- NA
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################

##### Descriptives #####
# frequencies
table(joined$parent)
table(joined$a_sex_dv)
summary(joinedlong$income)

# parents X childless
mean(filter(joinedlong, parent == 0)$income, na.rm = T)
mean(filter(joinedlong, parent == 1)$income, na.rm = T)

# men vs women total
mean(filter(joinedlong, female == 0)$income, na.rm = T)
mean(filter(joinedlong, female == 1)$income, na.rm = T)

# childless men vs childless women
mean(filter(joinedlong, time == 1, parent == 0 & female == 0)$income, na.rm = T)
mean(filter(joinedlong, time == 1, parent == 0 & female == 1)$income, na.rm = T)

# fathers vs mothers
mean(filter(joinedlong, time2 == 1, parent == 1 & female == 0)$income, na.rm = T)
mean(filter(joinedlong, time2 == 1, parent == 1 & female == 1)$income, na.rm = T)

##### Visualisation #####
hist(joinedlong$income,breaks = 100)

##############################################################################
jl2 <- joinedlong
jl2$parent2 <- "Total"
jl2$time <- 1
jlbarchart <- rbind(joinedlong, jl2)

ggplot(data = filter(jlbarchart, time == 1), aes(x = parent2, y = income, fill = sex)) + 
  geom_bar(stat = "summary", position = "dodge") + 
  
  theme(text = element_text(size = 15),plot.title = element_text(hjust = 0.5), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+ 
  
  labs(title = "Income by sex and parenthood",
       y = "Average monthly labour income", x = "Parent/Childless")
###############################################################################


ggplot(joinedlong, aes(x=wave, y=income)) +
  scale_x_continuous(breaks = 1:11) +
  
  stat_summary(data = joinedlong,aes(y = income, color = sex), fun=mean, geom="line", size = 1.5) +
  
  ggplot2::annotate("segment", x = 5.5,xend = 5.5, y =1000, yend = 2850 ,size = 1, colour = "black")+
  
  ggplot2::annotate("text", x = 5.5, y = 3000, size = 4, label = "Childbirth \n (treatment)") +
  
  labs(title = "Average income by gender \n and parenthood over time",
       x = "Wave of data colelction", y = "Average monthly labour income") +
  
  facet_wrap(~parent2) +
  
  scale_fill_manual(labels = c("male", "female"), values = c("#00BFC4","#F8766D")) +
  
  theme(text = element_text(size = 15),plot.title = element_text(hjust = 0.5), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        panel.border = element_rect(fill = NA, colour="black"),
        strip.background =  element_rect(fill = NA, colour = NA), strip.text =element_text(size = 14))





##### Analysis #####



modelparenthood <- lm(income ~ parent*time, data = joinedlong)
summary(modelparenthood)


modelmotherhoodpenalty <- lm(income ~ parent*time, data = filter(joinedlong, female == 1))
summary(modelmotherhoodpenalty)

modelfatherhoodpremium <- lm(income ~ parent*time, data = filter(joinedlong, female == 0))
summary(modelfatherhoodpremium)

confint(modelparenthood,level = 0.95)
confint(modelmotherhoodpenalty,level = 0.95)



##### Model diagnostics #####

par(mfrow = c(2, 2))
plot(modelmotherhoodpenalty)





##### normal qq plot tail testing

quantile(joinedlong$income, .95, na.rm = T) # = 4839

  
y <- lm(income ~ parent*time, data = filter(joinedlong, female == 1 & income < 4839))
summary(y)

par(mfrow = c(2, 2))
plot(y)
