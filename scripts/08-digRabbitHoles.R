rm(list = ls())

library(dplyr)
library(ggplot2)
library(stringr)
library(reshape2)
library(tidyr)

load("output/eosCuratedWithS&TQuery.RData")

CABINET <- c( "DOE"  , "HHS" , "DOD", "EPA", 
              "State", "DOTr", "DOJ", "DOI", 
              "USDA" , "DOC" , "DOL", "HUD", 
              "DOT"  , "DOEd", "VA" , "DHS")

SCI <- c("OSTP", "PCAST", "NSF", 
         "NASA", "DOE"  , "NIH")

# c("NSTC", "NOAA", "NIST") #could add these to SCI

OTHER <- c("OMB", "NSC")

#remove unnecessary fields
eos <- eos %>%
  select(-text) %>%
  arrange(desc(eoMatches.all.num))

#break out row with query matches
eos2 <- eos %>%
  mutate(matchWord = str_split(string  = matchWords, 
                             pattern = "; ")) %>%
  unnest(matchWord) %>%
  mutate(matchWord = str_replace_all(matchWord, "__", ""))

#number of EOs by topic, by agency
topic.summary <- eos2 %>% 
  #filter(matchWord %in% c(CABINET, SCI)) %>%
  filter(matchWord %in% SCI) %>%
  group_by(matchWord, topic) %>% 
  tally() %>%
  as.data.frame() %>%
  dcast(topic ~ matchWord, fill = 0)

#top ten topics, by agency
topic.summary$TOTAL <- rowSums(topic.summary[ , 2:ncol(topic.summary)])
topic.summary <- arrange(topic.summary, desc(TOTAL))
top.ten <- topic.summary[1:10, ]
other <- topic.summary[11:nrow(topic.summary), ]
other <- colSums(other[ , 2:ncol(other)])
other <- c("Other", other)
top.ten <- rbind(top.ten, other)

for(i in 1:ncol(top.ten)){
  top.ten[ , i] <- paste0(as.character(top.ten[ , i]), "|")
}

write.csv(x = top.ten, 
          file = "output/top.ten.topics.csv",
          row.names = FALSE)

#top 3 cited EOs by agency
cite.summary <- eos2 %>% 
  #filter(matchWord %in% c(CABINET, SCI)) %>%
  filter(matchWord %in% SCI) %>%
  mutate(president = str_replace_all(president, "[0-9][0-9]-", "")) %>%
  mutate(meta = paste0(title, " (", president, ", ", num )) %>%
  select(matchWord, eoMatches.all.num, meta) %>%
  arrange(matchWord, eoMatches.all.num) %>%
  group_by(matchWord) %>% 
  filter(min_rank(desc(eoMatches.all.num)) < 4) %>%
  as.data.frame() %>%
  mutate(meta = paste0(meta, ", ", eoMatches.all.num, ")")) %>%
  select(-eoMatches.all.num) %>%
  group_by(matchWord) %>%
  summarise(top3 = paste0(meta, collapse = "; "))

#top 3 cited EOs by agency, with Gov. Ops. EOs removed
cite.summary2 <- eos2 %>% 
  #filter(matchWord %in% c(CABINET, SCI)) %>%
  filter(matchWord %in% SCI) %>%
  filter(topic != "Government Operations") %>%
  mutate(president = str_replace_all(president, "[0-9][0-9]-", "")) %>%
  mutate(meta = paste0(title, " (", president, ", ", num )) %>%
  select(matchWord, eoMatches.all.num, meta) %>%
  arrange(matchWord, eoMatches.all.num) %>%
  group_by(matchWord) %>% 
  filter(min_rank(desc(eoMatches.all.num)) < 4) %>%
  as.data.frame() %>%
  mutate(meta = paste0(meta, ", ", eoMatches.all.num, ")")) %>%
  arrange(desc(eoMatches.all.num)) %>%
  select(-eoMatches.all.num) %>%
  group_by(matchWord) %>%
  summarise(top3 = paste0(meta, collapse = "; <br> "))

write.csv(x = cite.summary2, 
          file = "output/top.three.cited.csv", 
          row.names = FALSE)

#agency co-citation frequency matrix
freq <- eos2 %>%
  #filter(topic != "Government Operations") %>%
  #filter(topic %in% c("Energy", "Environment", "Health", "Defense", "Science, Space, Technology, and Communications")) %>%
  #filter(st.tm.topic) %>%
  select(num, matchWord) %>%
  #filter(!grepl("^[a-z]+", matchWord)) %>%
  mutate(count = 1) %>%
  dcast(num ~ matchWord, fill = 0) %>%
  select(-Var.2)

rownames(freq) <- freq$num
freq <- select(freq, -num)
freq <- as.matrix(freq)

#boxpolot of number of agencies
totals <- rowSums(freq[, colnames(freq) %in% c(CABINET, SCI)])
lots.o.agencies <- totals[totals > 9]
boxplot(totals)
summary(totals)

totals2 <- rowSums(freq[, colnames(freq) %in% SCI])
boxplot(totals2)
summary(totals2)

#a <- eos[eos$num %in% names(lots.o.agencies), ]$num

co <- t(freq) %*% freq
co <- as.data.frame(co)

co2 <- co

for(i in 1:ncol(co)){
  
  co2[ , i] <- round(co[ , i] * 100 / max(co[ , i]), 1)
  
}

#co3 <- co2[names(co2) %in% c(CABINET, SCI), names(co2) %in% SCI]
co3 <- co2[names(co2) %in% c(CABINET, SCI), names(co2) %in% c(CABINET, SCI)]

write.csv(x    = co3,
          file = "output/agency.cooccurance.csv",
          row.names = FALSE)
