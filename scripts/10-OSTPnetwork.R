#
# OSTP Network 
#

rm(list = ls())

library(dplyr)
library(ggplot2)
library(ggraph)
source("scripts/000-multiplotFunction.R")

# Load data
load("output/eosCuratedWithS&TQuery.RData")

# Read in unpublished EOs
unpublishedEOs <- read.csv("data/UnpublishedEOs.csv", 
                           header = FALSE, 
                           stringsAsFactors = FALSE)
unpublishedEOs <- unpublishedEOs$V1


####################
# Prepare dataframe
####################
# Narrow down dataframe to necessary columns
eos <- 
  eos %>%
  select(num, title, year, president, party, 
         eoMatches.out, eoMatches.out.num, eoMatches.in.num, eoMatches.all.num)

# Load OSTP EOs
ostpEos <- read.csv("output/ostp.eos.csv", header = TRUE)

# get OSTP EO numbers
ostp <- as.character(ostpEos$num)

# Flag OSTP EOs
eos <-
  eos %>% 
  mutate(is.ostp = num %in% ostp)

# Split out eoMatches list into separate rows
eos2 <- eos %>%
  mutate(eoMatch = str_split(string  = eoMatches.out, 
                             pattern = "; ")) %>%
  unnest(eoMatch) %>% 
  mutate(eoMatch = str_trim(eoMatch))


##############################
# Create edgelist and nodelist
##############################
# Edgelist
eoEdgelist <- 
  eos2 %>%
  select(num, eoMatch) %>%
  filter(eoMatch != "") %>% 
  filter(!eoMatch %in% unpublishedEOs) %>% 
  distinct()
names(eoEdgelist) <- c("Source", "Target")

# Nodelist
eoNodelist <-
  eos2 %>%
  select(-eoMatch, -eoMatches.out) %>%
  distinct() %>%
  mutate(num = str_trim(num)) %>% 
  mutate(Id = num) %>%
  mutate(Label = num) %>%
  mutate(presidentNum = as.numeric(str_extract(string  = president, 
                                               pattern = "[0-9]+"))) %>%
  mutate(president = gsub("[-0-9]", "", president)) %>% 
  mutate(president = gsub("G. W. Bush", "W. Bush", president))

# Create color coding for Obama OSTP, Obama non-OSTP, other OSTP, other non-OSTP
eoNodelist$Category <- NA
eoNodelist$Category[eoNodelist$president == "Obama" & eoNodelist$is.ostp == TRUE] <- "Obama_OSTP"
eoNodelist$Category[eoNodelist$president == "Obama" & eoNodelist$is.ostp == FALSE] <- "Obama_non"
eoNodelist$Category[eoNodelist$president != "Obama" & eoNodelist$is.ostp == TRUE] <- "Other_OSTP"
eoNodelist$Category[eoNodelist$president != "Obama" & eoNodelist$is.ostp == FALSE] <- "Other_non"


##############################
# Write to csv
##############################
write.csv(eoEdgelist, 
          file = "output/OSTP-edgelist.csv", 
          row.names = FALSE)
write.csv(eoNodelist, 
          file = "output/OSTP-nodetable.csv", 
          row.names = FALSE)


