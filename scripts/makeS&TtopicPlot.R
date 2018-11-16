rm(list = ls())

library(stringr)
library(dplyr)
library(tidyr)

load("output/eosCuratedWithS&TQuery.RData")

# remove EOs that have not yet been given a manual topic
eos <- eos %>%
  select(-text, -title.plus.text) %>%
  filter(!is.na(st.manual.topic))

# pare down S&T query matches
eos[eos$stMatchRatio < 0.01, ]$st.query.topic <- FALSE
eos[!grepl("scien|OSTP|PCAST|NSTC|NSF", eos$matchWords), ]$st.query.topic <- FALSE

# total number of EOs per president
totals <- eos %>%
  group_by(president) %>%
  tally()

names(totals) <- c("president", "total")

st.tm.stats <- eos %>%
  filter(st.tm.topic) %>%
  group_by(president) %>%
  tally() %>%
  merge(totals) %>%
  mutate(ratio = n / total)

library(ggplot2)

p <- ggplot(st.tm.stats, aes(x = president, y = ratio)) +
  geom_point()
p

##
#Assign Groups
##

eos$st.group <- "Non-S&T"

eos[eos$st.tm.topic, ]$st.group <- "Topic Model"

eos[eos$st.manual.topic, ]$st.group <- 
  paste0(eos[eos$st.manual.topic, ]$st.group, "; Manual Tag")

eos[eos$st.query.topic, ]$st.group <- 
  paste0(eos[eos$st.query.topic, ]$st.group, "; Query")

eos$st.group <- str_replace_all(eos$st.group, "Non-S&T; ", "")

eos2 <- eos %>%
  mutate(st.group2 = str_split(string  = st.group, 
                               pattern = "; ")) %>%
  unnest(st.group2)

st.group.stats <- eos2 %>%
  group_by(president, st.group2) %>%
  tally() %>%
  merge(totals) %>%
  mutate(ratio = n / total) %>%
  filter(st.group2 != "Non-S&T")

q <- ggplot(st.group.stats) +
  geom_point(aes(x = president, y = ratio, 
                 group = st.group2, color = st.group2)) +
  geom_line(aes(x = president, y = ratio, 
                group = st.group2, color = st.group2))
q

write.csv(x    = eos, 
          file = "tst.csv", 
          row.names = FALSE)

st.group.stats2 <- eos %>%
  group_by(st.group) %>%
  tally() 


