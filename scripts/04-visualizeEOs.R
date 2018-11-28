##################################################################################################
#
# Visualize EO network 
#
##################################################################################################

rm(list = ls())
source("scripts/000-utilityFunctionsEO.R")

# Load packages
required.packages <- c("dplyr",
                       "stringr",
                       "tidyr",
                       "igraph")
LoadPackages(required.packages = required.packages)

# Load dataset
load("output/eosCuratedWithS&TQuery.RData")

# Read in unpublished EOs
unpublishedEOs <- read.csv("data/UnpublishedEOs.csv", 
                           header = FALSE, 
                           stringsAsFactors = FALSE)
unpublishedEOs <- unpublishedEOs$V1

##############################
# Prepare dataframe
##############################
# Select columns of interest
eos <- 
  eos %>%
  select(num, title, year, president, party, 
         eoMatches.out, eoMatches.all.num,
         st.query.topic, stMatch.count, stMatchRatio, 
         st.tm.topic, topic.broad,
         pcast, pcast.first.deg, ostp, ostp.first.deg)

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
  mutate(president = gsub(pattern = ".*\\s([A-Za-z]+)$", 
                          replacement = "\\1", 
                          x = president, perl = TRUE)) 

# Create categories for pcast/ostp connection type
eoNodelist$pcast_flag <- "NA"
eoNodelist$pcast_flag[eoNodelist$pcast == TRUE] <- "PCAST"
eoNodelist$pcast_flag[eoNodelist$pcast == TRUE] <- "PCAST-FirstDeg"

eoNodelist$ostp_flag <- "NA"
eoNodelist$ostp_flag[eoNodelist$ostp == TRUE] <- "OSTP"
eoNodelist$ostp_flag[eoNodelist$ostp.first.deg == TRUE] <- "OSTP-FirstDeg"

# Remove uncessary columns
eoNodelist <-
  eoNodelist %>% 
  select(Id, Label, num, title, year, president, party, eoMatches.all.num, 
         st.query.topic, stMatch.count, stMatchRatio, st.tm.topic,
         topic.broad, ostp_flag, pcast_flag, presidentNum)


##############################
# Write to csv
##############################
write.csv(eoEdgelist, 
          file = "output/EO-edgelist.csv", 
          row.names = FALSE)
write.csv(eoNodelist, 
          file = "output/EO-nodetable.csv", 
          row.names = FALSE)

