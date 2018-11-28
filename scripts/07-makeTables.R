rm(list = ls())

library(stringr)
library(dplyr)

load("output/eosCuratedWithS&TQuery.RData")

eos <- eos %>%
  arrange(president) %>%
  mutate(president = str_replace_all(string = president, 
                                     pattern = "[0-9]+-", 
                                     replacement = ""),
         stMatchRatio = round(stMatchRatio, 3)*100)

##############################
#OSTP EOs
##############################


eos.ostp <- eos %>%
  arrange(desc(year)) %>%
  filter(ostp) %>%
  select(num, title, link, year, president, eoMatches.out, eoMatches.in, matchWords)

write.csv(x    = eos.ostp,
          file = paste0("output/ostp.eos.csv"),
          row.names = FALSE)



##############################
#Tables for Old Paper
##############################


PRESENTATION <- FALSE

if(PRESENTATION){
  
  NROW <- 5
  FILENAME <- "output/plots/presentation/"
  
  eos$title <- paste0(eos$title, " (", eos$president, ")")
  
} else{
  
  NROW <- 10
  FILENAME <- "output/plots/"
  
  eos$title <- paste0(eos$title, " (", eos$president, ", ", eos$num, ")")
  
}

##############################
#Query Analysis
##############################

top.st.ratio <- eos %>%
  arrange(desc(stMatchRatio)) %>%
  select(title, stMatchRatio, word.count) %>%
  head(n = NROW)

names(top.st.ratio) <- c("Title", "% S&T Words", "Word Count")

write.csv(x    = top.st.ratio,
          file = paste0(FILENAME, "top.st.ratio.csv"),
          row.names = FALSE)


##############################
#Topic Model Analysis
##############################

tm <- read.csv(file = "Topic Modeling/output/EO_Topic_k80_DocTopMatrix.csv",
               stringsAsFactors = FALSE)

names(tm)[1] <- "num"

top.st.tm <- tm %>%
  select(num, t.33) %>% #t.33 = science topic
  merge(eos) %>%
  arrange(desc(t.33)) %>% 
  select(title, t.33, word.count) %>%
  head(n = NROW)

names(top.st.tm) <- c("Title", "% S&T Topic", "Word Count")

write.csv(x    = top.st.tm, 
          file = paste0(FILENAME, "top.st.tm.csv"),
          row.names = FALSE)


##############################
#Network Analysis
##############################

top.st.out.degree <- eos %>%
  arrange(desc(eoMatches.out.num)) %>%
  select(title, eoMatches.out.num, stMatchRatio, word.count) %>%
  filter(!str_detect(title, fixed("EURATOM", ignore_case = TRUE))) %>%
  filter(stMatchRatio > 2) %>%
  head(n = NROW)

names(top.st.out.degree) <- c("Title", "# EOs Mentioned", "% S&T Words", "Word Count")

write.csv(x    = top.st.out.degree,
          file = paste0(FILENAME, "top.st.out.degree.csv"),
          row.names = FALSE)

top.st.in.degree <- eos %>%
  arrange(desc(eoMatches.in.num)) %>%
  select(title, eoMatches.in.num, stMatchRatio, word.count) %>%
  filter(!str_detect(title, fixed("EURATOM", ignore_case = TRUE))) %>%
  filter(stMatchRatio > 2) %>%
  head(n = NROW)

names(top.st.in.degree) <- c("Title", "# EOs Mentioned", "% S&T Words", "Word Count")

write.csv(x    = top.st.in.degree,
          file = paste0(FILENAME, "top.st.in.degree.csv"),
          row.names = FALSE)


