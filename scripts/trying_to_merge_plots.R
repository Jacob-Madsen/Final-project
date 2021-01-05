
source("~/Final project/scripts/data_organisation.R")

library(reshape)


#class and other race in 1981
class_other_race_81 <- data_frame_81 %>% 
  count(class_81,avoid_other_race) %>% 
  group_by(class_81) %>% 
  mutate(percent = (n / sum(n)) * 100) %>%
  filter(avoid_other_race != "not checked") %>% 
  ungroup() 


plot_81 %>% 
  ggplot(aes(x = class_81, y = percent, fill = avoid_other_race)) +
  geom_bar(stat = "identity", position = "dodge") + 
  scale_x_discrete(drop = FALSE)




#class and minority sects in 1981
class_minority_sects_81 <- data_frame_81 %>% 
  count(class_81,avoid_minority_sects) %>% 
  group_by(class_81) %>% 
  mutate(percent = (n / sum(n)) * 100) %>%
  filter(avoid_minority_sects != "not checked") %>% 
  ungroup()


filter(avoid_minority_sects != "not checked") %>% 
  ggplot(aes(x = class_81, y = percent, fill = avoid_minority_sects)) +
  geom_bar(stat = "identity", position = "dodge") + 
  scale_x_discrete(drop = FALSE)



#i should somehow merge class_other_race_81 and class_minority_sects_81 to be able to plot them together

#something like this
ggplot() + geom_bar(aes(fill = avoid_minority_sects), stat = "identity", position = "dodge")

#or this

plot_81 + 
  geom_bar(aes(fill = avoid_minority_sects), stat = "identity", position = "dodge")

#in the guide https://stackoverflow.com/questions/38629269/ggplot-geom-bar-combining-in-one-graph it says that i should use melt function with the reshape package, so i have installed that one

socio_economic_81 <- melt(class_minority_sects_81, class_other_race_81, id.vars="class_81") #it cant melt it - i have difficulties uunderstanding the melt function

#maybe the problem is that it is two different dataframes and in the guide it is two plots from one dataframe?

#in my original data_frame_81 i havent the percentage information and so on, so i cant use that one to melt and plot





###############################################
#i also tried this. Dont mind the typos. In kinda works for merging dataframes together, but not really in a long format

#Overview of the allow to work question

#P2.6
#1990
owerview_1 <- data_frame_90 %>% 
  count(allow_work_90) 

#P2.7
#1999
owerview_2 <- data_frame_99 %>% 
  count(allow_work_99) 


#P2.8
#2008
owerview_3 <- data_frame_08 %>% 
  count(allow_work_08) 


#P2.9
#2017
owerview_4 <- data_frame_17 %>% 
  count(allow_work_17) 

merge(owerview_1, owerview_2, by = "row.names", all = TRUE)



###############################################################

plot_1 <- data_frame_99 %>% 
  count(allow_work_99) %>% 
  mutate(percent = (n / sum(n)) * 100) %>% 
  mutate(year = "1999") %>% 
  filter(allow_work_99 != "NA") %>% 
  rename(allow_work = allow_work_99)

plot_2 <- data_frame_08 %>% 
  count(allow_work_08) %>% 
  mutate(percent = (n / sum(n)) * 100)%>% 
  filter(allow_work_08 != "NA") %>% 
  mutate(year = "2008") %>% 
  rename(allow_work = allow_work_08)

plot_1_2_combined <- rbind(plot_1,plot_2)

plot_1_2_combined %>% 
  ggplot(aes(x = fct_rev(allow_work), y = percent, fill = fct_rev(year))) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  scale_fill_viridis_d(breaks = rev, direction = -1) +
  labs(title = "Respondents' oppinion on foreign workers taking jobs 1999 compared to 2008",
       x = "Oppinion",
       y = "Percentage of answers",
       fill = "Year") +
  theme_replace() +
  theme(plot.title = element_text(hjust = 1.7, vjust=0),
        axis.text.x = element_text(colour = "black", size = 15, angle = 0,
                                   hjust = 0.5, vjust = 0.5),
        axis.text.y = element_text(colour = "black", size = 15),
        text = element_text(size = 16))



#######################################################
plot_4_27 <- data_frame_81 %>% 
  count(sex_81,avoid_foreign_workers) %>% 
  group_by(sex_81) %>% 
  mutate(percent = (n / sum(n)) * 100) %>%
  mutate(year = "1981") %>% 
  ungroup() %>% 
  filter(avoid_foreign_workers != "not checked") %>% 
  rename(sex = sex_81,
         avoid__immigrants_foreign_workers = avoid_foreign_workers)

plot_4_28 <- data_frame_90 %>% 
  count(sex_90,avoid_foreign_workers) %>% 
  group_by(sex_90) %>% 
  mutate(percent = (n / sum(n)) * 100) %>%
  mutate(year = "1990") %>% 
  ungroup() %>% 
  filter(avoid_foreign_workers != "not checked")%>% 
  rename(sex = sex_90,
         avoid__immigrants_foreign_workers = avoid_foreign_workers)

plot_4_29 <- data_frame_99 %>% 
  count(sex_99,avoid_immigrants) %>% 
  group_by(sex_99) %>% 
  mutate(percent = (n / sum(n)) * 100) %>%
  mutate(year = "1999") %>% 
  ungroup() %>% 
  filter(avoid_immigrants != "not checked") %>% 
  filter(avoid_immigrants != "dont know")%>% 
  rename(sex = sex_99,
         avoid__immigrants_foreign_workers = avoid_immigrants)

plot_4_30 <- data_frame_08 %>% 
  count(sex_08,avoid_immigrants) %>% 
  group_by(sex_08) %>% 
  mutate(percent = (n / sum(n)) * 100) %>%
  mutate(year = "2008") %>% 
  ungroup() %>% 
  filter(avoid_immigrants != "not checked") %>%
  filter(avoid_immigrants != "dont know")%>% 
  rename(sex = sex_08,
         avoid__immigrants_foreign_workers = avoid_immigrants)

plot_4_31 <- data_frame_17 %>% 
  count(sex_17,avoid_immigrants_foreign_workers) %>% 
  group_by(sex_17) %>% 
  mutate(percent = (n / sum(n)) * 100) %>%
  mutate(year = "2017") %>% 
  ungroup() %>% 
  filter(avoid_immigrants_foreign_workers != "not checked") %>%
  filter(avoid_immigrants_foreign_workers != "dont know")%>% 
  rename(sex = sex_17,
         avoid__immigrants_foreign_workers = avoid_immigrants_foreign_workers)

plot_4_27_31_combined <- rbind(plot_4_27,plot_4_28,plot_4_29,plot_4_30,plot_4_31)

plot_4_27_31_combined %>% 
  ggplot(aes(x = year, y = percent, fill = sex)) +
  geom_bar(stat = "identity", position = "dodge")+
  labs(title = "Respondents' sex compared to wish to avoid \nforeign workers 1981 to 2017",
       x = "Year",
       y = "Percentage of answers checked 'avoid foreign workers'",
       fill = "Sex") +
  theme_replace() +
  theme(axis.text.x = element_text(colour = "black", size = 15, angle = 0,
                                   hjust = 0.5, vjust = 0.5),
        axis.text.y = element_text(colour = "black", size = 15),
        text = element_text(size = 16))



################################
plot_4_10 <- data_frame_81 %>% 
  count(proud_81,avoid_foreign_workers) %>% 
  group_by(proud_81) %>% 
  mutate(percent = (n / sum(n)) * 100) %>%
  mutate(year = "1981") %>% 
  ungroup() %>% 
  filter(avoid_foreign_workers != "not checked") %>% 
  filter(avoid_foreign_workers != "dont know") %>%
  filter(proud_81 != "undetermined") %>%
  filter(proud_81 != "NA")%>% 
  rename(proud = proud_81,
         avoid_immigrants_foreign_workers = avoid_foreign_workers)

plot_4_10_1 <- data_frame_99 %>% 
  count(proud_99,avoid_immigrants) %>% 
  group_by(proud_99) %>% 
  mutate(percent = (n / sum(n)) * 100) %>%
  mutate(year = "1999") %>% 
  ungroup() %>% 
  filter(avoid_immigrants != "not checked") %>%
  filter(avoid_immigrants != "dont know") %>%
  filter(proud_99 != "NA") %>% 
  filter(proud_99 != "undetermined") %>% 
  rename(proud = proud_99,
         avoid_immigrants_foreign_workers = avoid_immigrants)


plot_4_11 <- data_frame_17 %>% 
  count(proud_17,avoid_immigrants_foreign_workers) %>% 
  group_by(proud_17) %>% 
  mutate(percent = (n / sum(n)) * 100) %>%
  mutate(year = "2017") %>% 
  ungroup() %>% 
  filter(avoid_immigrants_foreign_workers != "not checked") %>%
  filter(avoid_immigrants_foreign_workers != "dont know") %>%
  filter(proud_17 != "NA") %>% 
  filter(proud_17 != "undetermined")%>% 
  rename(proud = proud_17,
         avoid_immigrants_foreign_workers = avoid_immigrants_foreign_workers)


plot_4_10_11_combined <- rbind(plot_4_10,plot_4_10_1,plot_4_11)

plot_4_10_11_combined %>% 
  ggplot(aes(x = year, y = percent, fill = proud)) +
  geom_bar(stat = "identity", position = "dodge")+
  labs(title = "Respondents' proudness of denmark compared to \nwish to avoid foreign workers or immigrants 1981 to 2017",
       x = "Year",
       y = "Percentage of answers checked 'avoid foreign workers'",
       fill = "Proudness of \nDenmark") +
  theme_replace() +
  theme(axis.text.x = element_text(colour = "black", size = 15, angle = 0,
                                   hjust = 0.5, vjust = 0.5),
        axis.text.y = element_text(colour = "black", size = 15),
        text = element_text(size = 16))



#####################################

plot_1_1 <- data_frame_81 %>% 
  count(trust_ppl_81) %>% 
  mutate(percent = (n / sum(n)) * 100) %>% 
  mutate(year = "1981") %>% 
  rename(trust_ppl = trust_ppl_81)


plot_1_2 <- data_frame_90 %>% 
  count(trust_ppl_90) %>% 
  mutate(percent = (n / sum(n)) * 100)%>% 
  mutate(year = "1990") %>% 
  rename(trust_ppl = trust_ppl_90)


plot_1_3 <- data_frame_99 %>% 
  count(trust_ppl_99) %>% 
  mutate(percent = (n / sum(n)) * 100)%>% 
  mutate(year = "1999") %>% 
  rename(trust_ppl = trust_ppl_99)


plot_1_4 <- data_frame_08 %>% 
  count(trust_ppl_08) %>% 
  mutate(percent = (n / sum(n)) * 100)%>% 
  mutate(year = "2008") %>% 
  rename(trust_ppl = trust_ppl_08)


plot_1_5 <- data_frame_17 %>% 
  count(trust_ppl_17) %>% 
  mutate(percent = (n / sum(n)) * 100)%>% 
  mutate(year = "2017") %>% 
  rename(trust_ppl = trust_ppl_17)


plot_1_1_5_combined <- rbind(plot_1_1,plot_1_2,plot_1_3,plot_1_4,plot_1_5)


plot_1_1_5_combined %>% 
  ggplot(aes(x = year, y = percent, fill = trust_ppl)) +
  geom_bar(stat = "identity", position = "dodge")+
  labs(title = "Respondents' trust in other people",
       x = "Year",
       y = "Percentage of answers",
       fill = "Trust in other people") +
  theme_replace() +
  theme(axis.text.x = element_text(colour = "black", size = 15, angle = 0,
                                   hjust = 0.5, vjust = 0.5),
        axis.text.y = element_text(colour = "black", size = 15),
        text = element_text(size = 16))
