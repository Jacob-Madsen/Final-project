
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
