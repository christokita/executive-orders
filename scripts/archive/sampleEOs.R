rm(list = ls())

library(dplyr)

load("output/eosCuratedWithS&TQuery.RData")

##############################
# Create Sample
##############################

eos.unique <- eos %>%
  select(title, year, president, st.query.topic, matchWords, 
         word.count, stMatchRatio, eoMatches.out, eoMatches.in, link) %>%
  as.data.frame() %>%
  unique()

#group stats
eos.unique %>%
  group_by(st.query.topic) %>%
  tally()

groupings <- eos.unique %>% 
  group_by(st.query.topic)

stSample <- sample_n(groupings, size = 200)
stSample <- as.data.frame(stSample)

##############################
# Save Output
##############################

write.csv(x    = stSample, 
          file = "output/sampleEOs.csv", 
          row.names = FALSE)

