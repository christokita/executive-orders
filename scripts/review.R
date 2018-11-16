rm(list=ls())

library(stringr)
library(dplyr)

load("output/eosCuratedWithS&TQueryAndManualTopicTags.RData")

eos <- select(eos, -text, -title.plus.text)

eos$topic.comb <- paste0(eos$topic, " - ", eos$topic.sub)

a <- eos %>%
  group_by(topic, topic.sub) %>%
  tally() %>%
  arrange(desc(n))

sum(eos$st.flag)
sum(eos$st.topic.flag)
sum(eos$st.topic[!is.na(eos$st.topic) ])

missed <- filter(eos, !st.flag & st.topic)

#missed$title

missed.stats <- missed %>% 
  group_by(topic.sub) %>%
  tally() %>%
  arrange(desc(n))

missed2 <- filter(eos, !st.topic.flag & st.topic)

#missed2$title

missed2.stats <- missed2 %>% 
  group_by(topic.sub) %>%
  tally() %>%
  arrange(desc(n))

missed3 <- filter(eos, st.topic.flag & !st.topic)

missed3.stats <- missed3 %>% 
  group_by(topic.sub) %>%
  tally() %>%
  arrange(desc(n))


write.csv(missed3, "output/tst.csv")



eos <- arrange(eos, desc(stMatchRatio))

write.csv(eos, "output/a.csv")

