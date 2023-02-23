# Load the relevant library
library(dplyr)
library(ggplot2)
library(tidyverse)

# Load the Race Weight data set
setwd("C:/Gardasil_project")
gardasil_data <- read.csv("C:/Gardasil_project/gardasil - data.csv")

################################################################################
################################################################################
##Data Managing:
race_white_black <- filter(gardasil_data, gardasil_data$Race <= 1)

# Calculate the percentage of each Completed group for each Race
race_white_black_summ <- race_white_black %>%
  group_by(Race, Completed) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  na.omit() %>% 
  mutate(Total = sum(n)) %>%
  mutate(Completed = if_else(Completed == 0, "No" , "Yes")) %>%
  mutate(Race = if_else(Race == 0, "White", "Black"))
  
race_white_black_summ$n[1]+race_white_black_summ$n[2]
race_white_black_summ$n[3]+race_white_black_summ$n[4]
race_white_black_summ$race_total <- c(731,731,443,443)
race_white_black_summ$race_percent <- (race_white_black_summ$n / race_white_black_summ$race_total)*100

write.csv(race_white_black_summ, "race.csv", row.names = F)
# Create a stacked bar plot with percentage labels
ggplot(race_white_black_summ, aes(x = Race, y = race_percent, fill = factor(Completed))) +
  geom_bar(stat = "identity") +
  scale_fill_discrete(name = "Completed") +
  labs(x = "Race", y = "Percentage") +
  geom_text(aes(label = sprintf("%.1f%%", race_percent)), position = position_stack(vjust = 0.5)) +
  theme_classic() +
  scale_x_discrete(breaks=c("Black","White"),
                   labels=c("Black(443)", "White(731)"))

# Odds ratio
(280/451) / (105/338)
# Comment: The odds of completion of white women are two times more likely than  
# the odds of completion of black women
# Upper OR
exp(log((280/451)/(105/338))) + 1.96*sqrt((1/280) + (1/451) + (1/105) + (1/338))
# Lower OR
exp(log((280/451)/(105/338))) - 1.96*sqrt((1/280) + (1/451) + (1/105) + (1/338))
################################################################################
################################################################################
##Data Managing:
gardasil_data$Location <- as.integer(gardasil_data$Location)
gardasil_data$location_cat <- if_else(gardasil_data$Location <= 2, "Suburban","Urban")

# Calculate the percentage of each Completed group for each Race
gd_location <- gardasil_data %>%
  drop_na(Completed) %>%
  group_by(location_cat, Completed) %>%
  summarise(count = n()) %>%
  mutate(Completed = if_else(Completed == 0, "No", "Yes"))

gd_location$count[1]+gd_location$count[2]
gd_location$count[3]+gd_location$count[4]
gd_location$loc_total <- c(962,962,449,449)
gd_location$loc_percent <- (gd_location$count / gd_location$loc_total)*100

write.csv(gd_location, "location.csv", row.names = F)
# Create a stacked bar plot with percentage labels
ggplot(gd_location, aes(x = location_cat, y = loc_percent, fill = factor(Completed))) +
  geom_bar(stat = "identity") +
  scale_fill_discrete(name = "Completed") +
  labs(x = "Location", y = "Percentage") +
  geom_text(aes(label = sprintf("%.1f%%", loc_percent)), position = position_stack(vjust = 0.5)) +
  theme_classic() +
  scale_x_discrete(breaks=c("Suburban","Urban"),
                   labels=c("Suburban(962)", "Urban(449)"))

# Odds ratio
(355/607) / (114/335)
# Comment: The odds of completion in suburban women are 72% more likely than  
# the odds of completion in urban women
# Upper OR
exp(log((355/607)/(114/335))) + 1.96*sqrt((1/355) + (1/607) + (1/114) + (1/335))
# Lower OR
exp(log((355/607)/(114/335))) - 1.96*sqrt((1/355) + (1/607) + (1/114) + (1/335))
################################################################################
################################################################################
table(gardasil_data$Age)
gardasil_data$AgeGroup <- if_else(gardasil_data$Age < 18, "Under 18","18 plus")
# Calculate the percentage of each Completed group for each Race
gd_age <- gardasil_data %>%
  drop_na(Completed) %>%
  group_by(AgeGroup, Completed) %>%
  summarise(count = n()) %>%
  mutate(Completed = if_else(Completed == 0, "No", "Yes"))

gd_age$count[1]+gd_age$count[2]
gd_age$count[3]+gd_age$count[4]
gd_age$age_total <- c(766,766,645,645)
gd_age$age_percent <- (gd_age$count / gd_age$age_total)*100

write.csv(gd_age, "age.csv", row.names = F)
# Create a stacked bar plot with percentage labels
ggplot(gd_age, aes(x = AgeGroup, y = age_percent, fill = factor(Completed))) +
  geom_bar(stat = "identity") +
  scale_fill_discrete(name = "Completed") +
  labs(x = "Age", y = "Percentage") +
  geom_text(aes(label = sprintf("%.1f%%", age_percent)), position = position_stack(vjust = 0.5)) +
  theme_classic() +
  scale_x_discrete(breaks=c("18 plus","Under 18"),
                   labels=c("18 plus(766)", "Under 18(645)"))

# Odds ratio
(237/408)/(232/534)
# Comment: The odds of completion in under 18 women are 34% more likely than  
# the odds of completion in 18 plus women

# Upper OR
exp(log((237/408)/(232/534))) + 1.96*sqrt((1/237) + (1/408) + (1/232) + (1/534))
# Lower OR
exp(log((237/408)/(232/534))) - 1.96*sqrt((1/237) + (1/408) + (1/232) + (1/534))
################################################################################
################################################################################

table(gardasil_data$InsuranceType)

# Calculate the percentage of each Completed group for each Race
gd_insurance <- gardasil_data %>%
  drop_na(Completed) %>%
  group_by(InsuranceType, Completed) %>%
  summarise(count = n()) %>%
  mutate(Completed = if_else(Completed == 0, "No", "Yes")) 

gd_insurance$InsuranceType<- c("Medical assistance", "Medical assistance", 
                               "Private payer", "Private payer", "Hospital based",
                               "Hospital based", "Military", "Military")
gd_insurance$count[1]+gd_insurance$count[2]
gd_insurance$count[3]+gd_insurance$count[4]
gd_insurance$count[5]+gd_insurance$count[6]
gd_insurance$count[7]+gd_insurance$count[8]
gd_insurance$ins_total <- c(274,274,723,723,84,84,330,330)
gd_insurance$ins_percent <- (gd_insurance$count / gd_insurance$ins_total)*100

write.csv(gd_insurance, "Insurance.csv", row.names = F)

# Create a stacked bar plot with percentage labels
ggplot(gd_insurance, aes(x = InsuranceType, y = ins_percent, fill = factor(Completed))) +
  geom_bar(stat = "identity") +
  scale_fill_discrete(name = "Completed") +
  labs(x = "Insurance", y = "Percentage") +
  geom_text(aes(label = sprintf("%.1f%%", ins_percent)), position = position_stack(vjust = 0.5)) +
  theme_classic() +
  scale_x_discrete(labels = c("Hospital based(84)","Medical assistance(274)","Military(330)","Private payer(723)"))


gd_insurance2 <- as.matrix(
  gd_insurance %>% 
  select(InsuranceType, Completed, count) %>%
  pivot_wider(names_from = InsuranceType, values_from = count) %>%
  select(`Medical assistance`, `Private payer`, `Hospital based`, `Military`)
)
# Hypothesis:
# H0: p1 = p2 = p3 = p4
# Ha: At least two proportions are different

chisq.test(gd_insurance2, correct = FALSE)

# Comment: The corresponding chi-square test indicates X2 = 31.061 and P = 0.0000,
# indicating that completion rate is different for at least two of the four insurance types.

################################################################################
################################################################################
#follow-up question
gardasil_data$Completed <- as.integer(gardasil_data$Completed)
gardasil_data$Completed_m1 <- if_else(gardasil_data$Shots < 3,0,1)
gardasil_data$Completed_m2 <- if_else(gardasil_data$Shots < 2,0,1)

# Calculate the percentage of each Completed group for each Race
gd_shots <- gardasil_data %>%
  group_by(Shots, Completed_m1) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  na.omit() %>% 
  mutate(Total = sum(n)) %>%
  mutate(Completed_m1 = if_else(Completed_m1 == 0, "No" , "Yes")) %>%
  mutate(Shots = if_else(Shots == 1, "1st Shots", 
                        if_else(Shots == 2,"2nd Shots","3rd Shots")))

gd_shots1 <- data.frame(shots = c('1st & 2nd Shots','3rd Shots'),
                       Completed = c('no','yes'),
                       n = c(gd_shots$n[1]+gd_shots$n[2],gd_shots$n[3]),
                                         total = c(1413,1413)) 
gd_shots1$Shots_percent <- (gd_shots1$n / gd_shots1$total)*100

# Create a stacked bar plot with percentage labels
ggplot(gd_shots1, aes(x = shots, y = Shots_percent, fill = factor(Completed))) +
  geom_bar(stat = "identity") +
  scale_fill_discrete(name = "Completed") +
  labs(x = "Shots", y = "Percentage") +
  geom_text(aes(label = sprintf("%.1f%%", Shots_percent)), position = position_fill(vjust = 30)) +
  theme_classic() +
  scale_x_discrete(breaks=c("1st & 2nd Shots","3rd Shots"),
                   labels=c("1st & 2nd Shots(876)","3rd Shots(537)"))

# Calculate the percentage of each Completed group for each Race
gd_shots <- gardasil_data %>%
  group_by(Shots, Completed_m2) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  na.omit() %>% 
  mutate(Total = sum(n)) %>%
  mutate(Completed_m1 = if_else(Completed_m2 == 0, "No" , "Yes")) %>%
  mutate(Shots = if_else(Shots == 1, "1st Shots", 
                         if_else(Shots == 2,"2nd Shots","3rd Shots")))

gd_shots2 <- data.frame(shots = c('1st Shots','2nd Shots & 3rd Shots'),
                       Completed = c('no','yes'),
                       n = c(gd_shots$n[1],gd_shots$n[2]+gd_shots$n[3]),
                       total = c(1413,1413)) 
gd_shots2$Shots_percent <- (gd_shots2$n / gd_shots2$total)*100

# Create a stacked bar plot with percentage labels
ggplot(gd_shots2, aes(x = shots, y = Shots_percent, fill = factor(Completed))) +
  geom_bar(stat = "identity") +
  scale_fill_discrete(name = "Completed") +
  labs(x = "Shots", y = "Percentage") +
  geom_text(aes(label = sprintf("%.1f%%", Shots_percent)), position = position_fill(vjust = 25)) +
  theme_classic() +
  scale_x_discrete(breaks=c("1st Shots","2nd Shots & 3rd Shots"),
                   labels=c("1st Shpts(440)","2nd Shots & 3rd Shots(973)"))


exp(log(1.72)) + 1.96*sqrt((1/607) + (1/355) + (1/335) + (1/114))
exp(log(1.72)) - 1.96*sqrt((1/607) + (1/355) + (1/335) + (1/114))

#If the The modern Gardasil have 3 doses then 38.0% woman ware completed the shots. 
#If the The modern Gardasil have 2 doses then 68.9% woman ware completed the shots.
#Hypothetically, The association with completion will be improved if these women just need two shots.
