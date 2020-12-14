source("~/Final project/scripts/data_organisation.R")


###############################
###############################
#Simple comparisons of levels in different coloumns

#P2.1
#Trust in people and reluctance to having foregin workers as neighbours
#1981
data_frame_81 %>%
  filter(!is.na(avoid_foreign_workers)) %>%
  group_by(trust_ppl_81) %>%
  count(avoid_foreign_workers, sort = TRUE) %>% 
  ggplot(aes(x = trust_ppl_81, y = n, fill = avoid_foreign_workers)) +
  geom_bar(stat = "identity", position = "dodge")+
  geom_text(aes(label=round(n, digits = 2)), vjust=1.6, color="black", size=3.5)

#1990
data_frame_90 %>%
  filter(!is.na(avoid_foreign_workers)) %>%
  group_by(trust_ppl_90) %>%
  count(avoid_foreign_workers, sort = TRUE)%>% 
  ggplot(aes(x = trust_ppl_90, y = n, fill = avoid_foreign_workers)) +
  geom_bar(stat = "identity", position = "dodge")+
  geom_text(aes(label=round(n, digits = 2)), vjust=1.6, color="black", size=3.5)

#1981
data_frame_99 %>%
  filter(!is.na(avoid_immigrants)) %>%
  group_by(trust_ppl_99) %>%
  count(avoid_immigrants, sort = TRUE)%>% 
  ggplot(aes(x = trust_ppl_99, y = n, fill = avoid_immigrants)) +
  geom_bar(stat = "identity", position = "dodge")+
  geom_text(aes(label=round(n, digits = 2)), vjust=1.6, color="black", size=3.5)

#1981
data_frame_08 %>%
  filter(!is.na(avoid_immigrants)) %>%
  group_by(trust_ppl_08) %>%
  count(avoid_immigrants, sort = TRUE) %>% 
  ggplot(aes(x = trust_ppl_08, y = n, fill = avoid_immigrants)) +
  geom_bar(stat = "identity", position = "dodge")+
  geom_text(aes(label=round(n, digits = 2)), vjust=1.6, color="black", size=3.5)

#1981
data_frame_81 %>%
  filter(!is.na(avoid_foreign_workers)) %>%
  group_by(trust_ppl_81) %>%
  count(avoid_foreign_workers, sort = TRUE)


summary(data_frame_81)