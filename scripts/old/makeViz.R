rm(list=ls())

source("scripts/PopulateSbuApp.R")
library(stringr)
library(dplyr)

load("output/eoMatchesCurated.RData")

rm(eos)

eos2[is.na(eos2$Target.Label), ]$Target.Label <- eos2[is.na(eos2$Target.Label), ]$Target
#eos2$Source.Label <- paste0(eos2$Source, "-", eos2$Source.Label)

eos2 <- eos2 %>%

edgelist <- eos2 %>%
  filter(Target != 0) %>%
  select(Source, Target) %>%
  filter(Source != "12553") %>%
  unique()

ig <- graph.edgelist(el=as.matrix(edgelist))

stats <- eos2 %>%
  filter(Target != 0) %>%
  group_by(Source) %>%
  tally() %>%
  arrange(desc(n))

names(stats) <- c("Source", "Size")

node.table <- eos2 %>%
  filter(Target != 0) %>%
  filter(Source != "12553") %>% #remove outlier
  filter(!duplicated(Source)) %>% #need to investigate this
  select(-Target.Label, -Target.Year, -Target, -Source.Html, -Source.Text, -Source.Year, -OSTP.Mentioned) %>%
  unique() %>%
  merge(stats)

names(node.table) <- c("Id", "Title", "Size")

node.table <- arrange(node.table, desc(Size))

#my.json <- PopulateApp(node.table = node.table, igraph.object = ig, color = "Broad.Category", size = "Size", edge.color = "#777", attributes = attributes)

my.json <- PopulateApp(node.table = node.table, igraph.object = ig, size = "Size", edge.color="#777")

write(my.json,file="viz_app/data.json")

