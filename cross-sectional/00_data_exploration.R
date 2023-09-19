head(rdt_hh)
check = rdt_hh %>%  group_by(Ward, RESULT) %>%  summarise(test_result= n()) %>%  filter(RESULT !="NOT DONE", RESULT !="Indeterminate") %>% 
  ungroup() %>% 
  group_by(Ward) %>% 
  mutate(percent = test_result/sum(test_result))


check = rdt_hh %>%  group_by(Age) %>%  summarise(test_result= n()) %>% ungroup() %>% mutate(percent = test_result/sum(test_result) *100)

plot(check$Age, check$percent)


check2 <- rdt_hh %>%  mutate(age_group = case_when(Age <=5 ~ "U5",
                                                   Age <= 10 ~'U10',
                                                   Age <=17  ~ 'U17',
                                                   Age <=40 ~ 'U40',
                                                   TRUE ~ 'over40'))

plot(check$age_group, check$percent)

check3 = check2 %>%  group_by(RESULT, age_group) %>%  summarise(test_result= n()) %>%  filter(RESULT !="NOT DONE", RESULT !="Indeterminate") %>% 
  ungroup() %>% 
  group_by(age_group) %>% 
  mutate(percent = test_result/sum(test_result))


ggplot(check3, aes(x=age_group, y=percent, fill=RESULT))+
  geom_col()+
  #facet_wrap(~age_group)+
  theme_manuscript()

