rm(list = ls())

library(dplyr)
library(ggplot2)
library(stringr)
library(reshape2)
library(tidyr)

load("output/eosCuratedWithS&TQuery.RData")

###

PRESENTATION  <- FALSE

PALETTE <- c("#E69F00", "#56B4E9", "#009E73", 
             "#0072B2", "#D55E00", "#CC79A7")

if(PRESENTATION){
  
  FILEPATH <- "output/plots/presentation/"
  LINETHICKNESS <- 2
  POINTSIZE     <- 4
  LABELSIZE     <- 20
  XLABEL        <- ""
  
} else{
  
  FILEPATH <- "output/plots/"
  LINETHICKNESS <- 0.5
  POINTSIZE     <- 1
  LABELSIZE     <- 16
  XLABEL        <- ""
  
}

THEME <- theme(
  axis.text.x  = element_text(size = LABELSIZE, angle = 90, hjust = 1, vjust = 0.5),
  axis.text.y  = element_text(size = LABELSIZE),
  axis.title.x = element_text(size = LABELSIZE, vjust = 1),
  axis.title.y = element_text(size = LABELSIZE, vjust = 1),
  axis.line = element_line(color = "black", size = 0.8),
  panel.grid.major = element_line(colour = "grey75"),
  panel.grid.minor = element_line(colour = "grey95")
) 

###

#shorten president names
eos <- eos %>%
  arrange(president) %>%
  mutate(president = str_replace_all(president, "[0-9]+-", ""))

eos$president <- factor(eos$president, levels = unique(eos$president))


#create a new row for each matched word
eos2 <- eos %>%
  mutate(matchWord = str_split(string  = matchWords, 
                               pattern = "; ")) %>%
  unnest(matchWord)

#calculate number of EOs per query per president
st.stats <- eos2 %>% 
  arrange(president) %>%
  group_by(matchWord, president) %>%
  tally() %>%
  as.data.frame()

st.stats <- st.stats %>%
  dcast(matchWord ~ president, fill = 0) %>%
  melt(value.name = "n")

#calculate number of EOs per president
president.stats <- eos %>%
  group_by(president) %>%
  tally() %>%
  as.data.frame()

names(president.stats) <- c("president", "total")

names(st.stats) <- c("matchWord", "president", "n")

st.stats <- st.stats %>%
  merge(president.stats) %>%
  mutate(percent = n / total)

st.stats <- st.stats %>%
  mutate(matchWord = str_replace_all(string       = matchWord,
                                     pattern      = "__",
                                     replacement = ""))

st.stats$percent <- st.stats$percent * 100


##############################
#Word Count Boxplot
##############################

p <- ggplot(eos) +
  theme_bw(base_size = 14) +
  geom_boxplot(aes(x = president, y = word.count)) +
  scale_y_continuous(limits = c(0, 20000),
                     breaks = seq(0, 20000, 5000),
                     expand = c(0, 0)) +
  THEME + 
  xlab(XLABEL) +
  ylab("Word Count") + #add blank line breaks to help with spacing
  ggtitle("")

if(PRESENTATION){
  p <- p + 
  ggtitle("") + 
  theme(legend.position="none")
}

p

ggsave(plot     = p, 
       width    = 9, 
       height   = 6, 
       units    = "in", 
       filename = paste0(FILEPATH, "word.count.boxplots.png"))

##############################
# Average EOs per Year per President
##############################

eo.stats <- eos %>%
  group_by(president) %>%
  tally() %>%
  as.data.frame()

yrs <- eos %>% 
  group_by(president) %>%
  summarise(yrs = length(levels(as.factor(year))) - 1)


eo.stats <- merge(eo.stats, yrs)
eo.stats$avg <- eo.stats$n/ eo.stats$yrs
eo.stats$err <- 100 / eo.stats$n

err <- select(eo.stats, -n, -yrs, -avg)

st.stats <- merge(st.stats, err)

p <- ggplot(eo.stats, aes(x = factor(president), y = avg)) +
  theme_bw(base_size = 14) +
  geom_bar(stat = "identity") +
  scale_y_continuous(limits = c(0, 140),
                     breaks = seq(0, 140, 20),
                     expand = c(0, 0)) +
  THEME + 
  xlab(XLABEL) +
  ylab("Average # of Executive Orders Per Year") + #add blank line breaks to help with spacing
  ggtitle("")

if(PRESENTATION){
  p <- p + 
    ggtitle("") + 
    theme(legend.position="none")
}


p

ggsave(plot     = p, 
       width    = 9, 
       height   = 6, 
       units    = "in", 
       filename = paste0(FILEPATH, "avg.eo.per.year.png"))


##############################
#EOP Offices
##############################

pres <- c("OSTP", "PCAST", "NSTC")
plot.df <- filter(st.stats, matchWord %in% pres)

p <- ggplot(plot.df, aes(x = president, 
                         y = percent, 
                         group = matchWord)) + 
  geom_line(aes(color = matchWord), 
            size = LINETHICKNESS) +
#   geom_point(aes(color = matchWord, 
#                  size  = n,
#                  shape = matchWord)) +
  geom_point(aes(color = matchWord), 
             size = POINTSIZE) +
  theme_bw(base_size = 14) +
#   geom_errorbar(aes(ymin = percent - err, 
#                     ymax = percent + err, 
#                     width = 0.1)) +
  scale_colour_manual(name = "Agency",
                      values = PALETTE) +
  scale_shape_discrete(name = "Agency") +
  scale_y_continuous(limits = c(0, 15),
                     breaks = seq(0, 15, 5),
                     expand = c(0, 0)) +
  scale_size_continuous(name = "# of EOs",
                        range = c(2, 5)) +
  THEME +
  xlab(XLABEL) +
  ylab("% of Executive Orders") +
  ggtitle("Mentions of White House Offices Over Time")

if(PRESENTATION){
  p <- p + 
    ggtitle("") + 
    theme(legend.position="none")
} else{
  p <- p +  
    geom_point(aes(color = matchWord, 
                   size  = n,
                   shape = matchWord)) 
}

p

ggsave(plot     = p, 
       width    = 9, 
       height   = 6, 
       units    = "in", 
       dpi = 300,
       filename = paste0(FILEPATH, "eop.offices.png"))

##############################
#STEAM Topics
##############################

steam <- c("science/research", "technology", "engineering", "art", "math")
plot.df <- filter(st.stats, matchWord %in% steam)

p <- ggplot(plot.df, aes(x = president, 
                         y = percent, 
                         group = matchWord)) + 
  geom_line(aes(color = matchWord), 
            size = LINETHICKNESS) +
  geom_point(aes(color = matchWord), 
             size = POINTSIZE) +
  theme_bw(base_size = 14) +
  scale_colour_manual(name = "Topic",
                      values = PALETTE) +
  scale_shape_discrete(name = "Topic") +
  scale_y_continuous(limits = c(0, 35),
                     breaks = seq(0, 35, 5),
                     expand = c(0, 0)) +
  scale_size_continuous(name = "# of EOs",
                        range = c(2, 5)) +
  THEME +
  xlab(XLABEL) +
  ylab("% of Executive Orders") +
  ggtitle("Mentions of STEAM Topics Over Time")

if(PRESENTATION){
  p <- p + 
    ggtitle("") + 
    theme(legend.position = "none")
} else{
  p <- p +  
    geom_point(aes(color = matchWord, 
                   size  = n,
                   shape = matchWord)) 
}


p

ggsave(plot     = p, 
       width    = 9, 
       height   = 6, 
       units    = "in", 
       dpi = 300,
       filename = paste0(FILEPATH, "steam.topics.png"))

##############################
#Non-STEAM S&T Topics
##############################

non.steam <- c("innovation", "computing", "university", "energy")
plot.df <- filter(st.stats, matchWord %in% non.steam)

p <- ggplot(plot.df, aes(x = president, 
                         y = percent, 
                         group = matchWord)) + 
  geom_line(aes(color = matchWord), 
            size = LINETHICKNESS) +
  geom_point(aes(color = matchWord), 
             size = POINTSIZE) +
  theme_bw(base_size = 14) +
  scale_colour_manual(name = "Topic",
                      values = PALETTE) +
  scale_shape_discrete(name = "Topic") +
  scale_y_continuous(limits = c(0, 20),
                     breaks = seq(0, 20, 5),
                     expand = c(0, 0)) +
  scale_size_continuous(name = "# of EOs",
                        range = c(2, 5)) +
  THEME +
  xlab(XLABEL) +
  ylab("% of Executive Orders") +
  ggtitle("Mentions of Other S&T Topics Over Time")

if(PRESENTATION){
  p <- p + 
    ggtitle("") + 
    theme(legend.position = "none")
} else{
  p <- p +  
    geom_point(aes(color = matchWord, 
                   size  = n,
                   shape = matchWord)) 
}


p

ggsave(plot     = p, 
       width    = 9, 
       height   = 6, 
       units    = "in", 
       dpi = 300,
       filename = paste0(FILEPATH, "non.steam.topics.png"))

##############################
# Non S&T Topics
##############################

non.st.topics <- c("security", "environment", "health", "education")
plot.df <- filter(st.stats, matchWord %in% non.st.topics)

p <- ggplot(plot.df, aes(x = president, 
                         y = percent, 
                         group = matchWord)) +
  geom_line(aes(color = matchWord),
            size = LINETHICKNESS) +
  geom_point(aes(color = matchWord), 
             size = POINTSIZE) +
  theme_bw(base_size = 14) +
  scale_colour_manual(name = "Topic",
                      values = PALETTE) +
  scale_shape_discrete(name = "Topic") +
  scale_y_continuous(limits = c(0, 60),
                     breaks = seq(0, 60, 10),
                     expand = c(0, 0)) +
  scale_size_continuous(name = "# of EOs",
                        range = c(2, 5)) +
  THEME +
  xlab(XLABEL) +
  ylab("% of Executive Orders") +
  ggtitle("Mentions of Non-S&T Topics Over Time")

if(PRESENTATION){
  p <- p + 
    ggtitle("") + 
    theme(legend.position = "none")
} else{
  p <- p +  
    geom_point(aes(color = matchWord, 
                   size  = n,
                   shape = matchWord)) 
}


p

ggsave(plot     = p, 
       width    = 9, 
       height   = 6, 
       units    = "in", 
       filename = paste0(FILEPATH, "non.s&t.topics.png"))


##############################
# DOE History
##############################

doe <- c("AEC", "ERDA", "NRC", "DOE")
plot.df <- filter(st.stats, matchWord %in% doe) 

p <- ggplot(plot.df, aes(x = president, 
                         y = percent, 
                         group = matchWord)) + 
  geom_line(aes(color = matchWord),
            size = LINETHICKNESS) +
  geom_point(aes(color = matchWord), 
             size = POINTSIZE) +
  theme_bw(base_size = 14) +
  scale_colour_manual(name = "Agency",
                      values = PALETTE) +
  scale_shape_discrete(name = "Agency") +
  scale_y_continuous(limits = c(0, 20),
                     breaks = seq(0, 20, 5),
                     expand = c(0, 0)) +
  scale_size_continuous(name = "# of EOs",
                        range = c(2, 5)) +
  THEME +
  xlab(XLABEL) +
  ylab("% of Executive Orders") +
  ggtitle("Department of Energy History")

if(PRESENTATION){
  p <- p + 
    ggtitle("") + 
    theme(legend.position = "none")
}  else{
  p <- p +  
    geom_point(aes(color = matchWord, 
                   size  = n,
                   shape = matchWord)) 
}


p

ggsave(plot     = p, 
       width    = 9, 
       height   = 6, 
       units    = "in", 
       filename = paste0(FILEPATH, "doe.history.png"))

##############################
# EE Agencies
##############################

ee <- c("EPA", "NOAA", "USGS", "USDA")
plot.df <- filter(st.stats, matchWord %in% ee)

p <- ggplot(plot.df, aes(x = president, 
                         y = percent, 
                         group = matchWord)) +
  geom_line(aes(color = matchWord), 
            size = LINETHICKNESS) +
  geom_point(aes(color = matchWord), 
             size = POINTSIZE) +
  theme_bw(base_size = 14) +
  scale_colour_manual(name = "Agency",
                      values = PALETTE) +
  scale_shape_discrete(name = "Agency") +
  scale_y_continuous(limits = c(0, 20),
                     breaks = seq(0, 20, 5),
                     expand = c(0, 0)) +
  scale_size_continuous(name = "# of EOs",
                        range = c(2, 5)) +
  THEME +
  xlab(XLABEL) +
  ylab("% of Executive Orders") +
  ggtitle("Mentions of Energy/Environment Agencies Over Time")

if(PRESENTATION){
  p <- p + 
    ggtitle("") + 
    theme(legend.position = "none")
} else{
  p <- p +  
    geom_point(aes(color = matchWord, 
                   size  = n,
                   shape = matchWord)) 
}


p

ggsave(plot     = p, 
       width    = 9, 
       height   = 6, 
       units    = "in", 
       dpi      = 300,
       filename = paste0(FILEPATH, "main.e&e.agencies.png"))

##############################
# Main S&T Agencies
##############################

sf <- c("NSF", "NIH", "NASA", "DOE", "OSTP", "PCAST")
plot.df <- filter(st.stats, matchWord %in% sf)

p <- ggplot(plot.df, aes(x = president, 
                         y = percent, 
                         group = matchWord)) +
  geom_line(aes(color = matchWord),
            size = LINETHICKNESS) +
  geom_point(aes(color = matchWord), 
             size = POINTSIZE) +
  theme_bw(base_size = 14) +
  scale_colour_manual(name = "Agency",
                      values = PALETTE) +
  scale_shape_discrete(name = "Agency") +
  scale_y_continuous(limits = c(0, 20),
                     breaks = seq(0, 20, 5),
                     expand = c(0, 0)) +
  scale_size_continuous(name = "# of EOs",
                        range = c(2, 5)) +
  THEME +
  xlab(XLABEL) +
  ylab("% of Executive Orders") +
  ggtitle("Mentions of S&T Agencies Over Time")

if(PRESENTATION){
  p <- p + 
    ggtitle("") + 
    theme(legend.position="none")
}  else{
  p <- p +  
    geom_point(aes(color = matchWord, 
                   size  = n,
                   shape = matchWord)) 
}


p

ggsave(plot     = p, 
       width    = 9, 
       height   = 6, 
       units    = "in", 
       filename = paste0(FILEPATH, "main.s&t.agencies.png"))

##############################
# Non-S&T Agencies Sample
##############################

non.st.agencies <- c("DOD", "DOJ", "DHS", "CIA")
plot.df <- filter(st.stats, matchWord %in% non.st.agencies)

p <- ggplot(plot.df, aes(x = president, 
                         y = percent, 
                         group = matchWord)) +
  geom_line(aes(color = matchWord),
            size = LINETHICKNESS) +
  geom_point(aes(color = matchWord), 
             size = POINTSIZE) +
  theme_bw(base_size = 14) +
  scale_colour_manual(name = "Agency",
                      values = PALETTE) +
  scale_shape_discrete(name = "Agency") +
  scale_y_continuous(limits = c(0, 30),
                     breaks = seq(0, 30, 5),
                     expand = c(0, 0)) +
  scale_size_continuous(name = "# of EOs",
                        range = c(2, 5)) +
  THEME +
  xlab(XLABEL) +
  ylab("% of Executive Orders") +
  ggtitle("Mentions of Non-S&T Agencies Over Time")

if(PRESENTATION){
  p <- p + 
    ggtitle("") + 
    theme(legend.position="none")
}  else{
  p <- p +  
    geom_point(aes(color = matchWord, 
                   size  = n,
                   shape = matchWord)) 
}


p

ggsave(plot     = p, 
       width    = 9, 
       height   = 6, 
       units    = "in", 
       filename = paste0(FILEPATH, "non.s&t.agencies.png"))


##############################
# Prevalence of S&T-related Topics over Time
##############################
  
# president.st.stats <- eos %>%
#   select(president, year, num, stMatchRatio) %>%
#   unique() %>%
#   group_by(president) %>%
#   summarize(mean.score = mean(stMatchRatio)) %>%
#   as.data.frame() %>%
#   arrange(president)
# 
# max.score <- max(eos$stMatchRatio)
# 
# president.st.stats$mean.score <- president.st.stats$mean.score / max.score
# 
# year.st.stats <- eos %>%
#   select(president, year, num, stMatchRatio) %>%
#   unique() %>%
#   group_by(year) %>%
#   summarize(mean.score = mean(stMatchRatio)) %>%
#   as.data.frame() %>%
#   arrange(year)
# 
# year.st.stats$mean.score <- year.st.stats$mean.score / max.score
# 
# year.st.stats$year <- as.numeric(year.st.stats$year)
# 
# p <- ggplot(year.st.stats, aes(x = year,
#                                y = mean.score)) +
#   #  geom_line(aes(group = 1), size = 1) +
#   geom_point(size = 3) +
#   theme_bw(base_size = 14) +
#   scale_x_continuous(breaks = seq(1945, 2015, 5),
#                      expand = c(0, 1)) +
#   scale_y_continuous(limits = c(0, 0.1),
#                      breaks = seq(0, 0.1, 0.02),
#                      expand = c(0, 0)) +
#   THEME +
#   xlab("\nYear") +
#   ylab("Mean S&T Word Proportion (renormalized)\n") + #add blank line breaks to help with spacing
#   ggtitle("")
# 
# if(PRESENTATION){p <- p + ggtitle("")}
# 
# p
# 
# ggsave(plot     = p, 
#        width    = 9, 
#        height   = 6, 
#        units    = "in", 
#        dpi      = 300,
#        filename = paste0(FILEPATH, "s&t.topic.prevalence.png"))

##############################
# Prevalence of S&T Topics By President
##############################

# p <- ggplot(president.st.stats, aes(x = president, 
#                                     y = mean.score)) + 
#   #  geom_line(aes(group = 1), size = 1) +
#   geom_point(size = 3) +
#   theme_bw(base_size = 14) +
#   scale_y_continuous(limits = c(0, 0.06),
#                      breaks = seq(0, 0.06, 0.01),
#                      expand = c(0, 0)) +
#   THEME +
#   xlab("\nYear") +
#   ylab("Mean S&T Word Proportion (renormalized)\n") + #add blank line breaks to help with spacing 
#   ggtitle("")
# 
# if(PRESENTATION){
#   p <- p + 
#     ggtitle("") + 
#     theme(legend.position = "none")
# }
# 
# 
# p
# 
# ggsave(plot     = p, 
#        width    = 9, 
#        height   = 6, 
#        units    = "in", 
#        dpi      = 300,
#        filename = paste0(FILEPATH, "president.s&t.prevalence.png"))
# 
# }

##############################
# Prevalence of S&T Topics By President (Method 2)
##############################

# # remove EOs that have not yet been given a manual topic
# eos2 <- eos %>%
#   select(-text, -title.plus.text) %>%
#   filter(!is.na(st.manual.topic))
# 
# # pare down S&T query matches
# eos2[eos2$stMatchRatio < 0.01, ]$st.query.topic <- FALSE
# #eos2[!grepl("scien|OSTP|PCAST|NSTC|NSF", eos2$matchWords), ]$st.query.topic <- FALSE
# 
# # total number of EOs per president
# totals <- eos2 %>%
#   group_by(president) %>%
#   tally()
# 
# names(totals) <- c("president", "total")
# 
# st.tm.stats <- eos2 %>%
#   filter(st.tm.topic) %>%
#   group_by(president) %>%
#   tally() %>%
#   merge(totals) %>%
#   mutate(ratio = n / total)
# 
# ##
# #Assign Groups
# ##
# 
# eos2$st.group <- "Non-S&T"
# 
# eos2[eos2$st.tm.topic, ]$st.group <- "Topic Model"
# 
# eos2[eos2$st.manual.topic, ]$st.group <- 
#   paste0(eos2[eos2$st.manual.topic, ]$st.group, "; Manual Tag")
# 
# eos2[eos2$st.query.topic, ]$st.group <- 
#   paste0(eos2[eos2$st.query.topic, ]$st.group, "; Query")
# 
# eos2$st.group <- str_replace_all(eos2$st.group, "Non-S&T; ", "")
# 
# eos3 <- eos2 %>%
#   mutate(st.group2 = str_split(string  = st.group, 
#                                pattern = "; ")) %>%
#   unnest(st.group2)
# 
# st.group.stats <- eos3 %>%
#   group_by(president, st.group2) %>%
#   tally() %>%
#   merge(totals) %>%
#   mutate(ratio = n / total) %>%
#   filter(st.group2 != "Non-S&T")
# 
# st.group.stats$ratio <- st.group.stats$ratio*100
# 
# p <- ggplot(st.group.stats) +
#   geom_point(aes(x = president, 
#                  y = ratio, 
#                  group = st.group2, 
#                  color = st.group2,
#                  size = POINTSIZE)) +
#   geom_line(aes(x = president, 
#                 y = ratio, 
#                 group = st.group2, 
#                 color = st.group2,
#                 size = LINETHICKNESS)) +
#   theme_bw(base_size = 14) +
#   THEME +
#   xlab(XLABEL) +
#   ylab("% of Executive Orders") +
#   ggtitle("")
# 
# if(PRESENTATION){
#   p <- p + 
#     ggtitle("") #+ 
#     #theme(legend.position = "none")
# } else{
#   p <- p +  
#     geom_point(aes(color = st.group2, 
#                    size  = n,
#                    shape = st.group2)) 
# }
# 
# p
# 
# ggsave(plot     = p,
#        width    = 9,
#        height   = 6,
#        units    = "in",
#        dpi      = 300,
#        filename = paste0(FILEPATH, "president.s&t.prevalence2.png"))
