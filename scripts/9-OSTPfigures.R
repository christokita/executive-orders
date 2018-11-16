#
# OSTP EO Figures
#
rm(list = ls())

library(dplyr)
library(ggplot2)
source("scripts/000-multiplotFunction.R")

##### Load and manipulate data #####
# Load data
load("output/eosCuratedWithS&TQuery.RData")

# Flag other agencies of interest
eos <- eos %>% 
  mutate(agency1 = grepl("__PCAST__", matchWords),
         agency2 = grepl("__NSF__", matchWords))

##### PLOT: Percent OSTP EOs by President ######
# Create data table 
# All EOs
prezOSTPall <- eos %>% 
  select(president, ostp, agency1, agency2) %>%
  mutate(number = as.numeric(gsub("[^0-9]", "", president)),
         president = as.factor(gsub("[-0-9]", "", president))) %>% 
  mutate(president = gsub("G. W. Bush", "W. Bush", president)) %>% 
  group_by(president, number) %>% 
  summarise(total = length(ostp),
            ostp = sum(ostp),
            agency1 = sum(agency1),
            agency2 = sum(agency2)) 
prezOSTPall <- prezOSTPall %>% 
  mutate(percentOSTP = ostp / total * 100,
         percentAGENCY1= agency1 / total * 100,
         percentAGENCY2 = agency2 / total * 100)
prezOSTPall$president <- factor(prezOSTPall$president, levels = prezOSTPall$president[order(prezOSTPall$number)])

# Only long EOs
# prezOSTP <- eos %>% 
#   filter(shortEo == FALSE) %>% 
#   select(president, ostp, omb, epa) %>%
#   mutate(number = as.numeric(gsub("[^0-9]", "", president)),
#          president = as.factor(gsub("[-0-9]", "", president))) %>% 
#   mutate(president = gsub("G. W. Bush", "W. Bush", president)) %>% 
#   group_by(president, number) %>% 
#   summarise(total = length(ostp),
#             ostp = sum(ostp),
#             omb = sum(omb),
#             epa = sum(epa)) 
# prezOSTP <- prezOSTP %>% 
#   mutate(percentOSTP = ostp / total * 100,
#          percentOMB = omb / total * 100,
#          percentEPA = epa / total * 100)
# prezOSTP$president <- factor(prezOSTP$president, levels = prezOSTP$president[order(prezOSTP$number)])

# Plot all
gg_ostpPercALL <- ggplot(data = prezOSTPall, aes(x = president)) +
  geom_line(aes(y = percentAGENCY1), 
            group = 1, 
            colour = "#bdbdbd", 
            size = 1) +
  geom_line(aes(y = percentAGENCY2), 
            group = 1, 
            colour = "#737373", 
            size = 1) +
  geom_line(aes(y = percentOSTP), 
            group = 1, 
            colour = "#de2d26", 
            size = 1) +
  theme_classic(base_size = 12) +
  scale_y_continuous(breaks = seq(0, 30, 2), limit = c(0, 14)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 8)) +
  ylab("% Executive Orders")
gg_ostpPercALL

ggsave("output/plots/OSTP_percentbyPresidentALL.png", dpi = 300, width = 4, height = 3)

# Plot only long
# gg_ostpPerc <- ggplot(data = prezOSTP, aes(x = president, y = percentOSTP)) +
#   geom_line(group = 1, colour = "#de2d26", size = 1) +
#   theme_classic(base_size = 12) +
#   scale_y_continuous(breaks = seq(0, 14, 2), limits = c(0, 14)) +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1),
#         axis.title.x = element_blank(),
#         axis.title.y = element_text(size = 8)) +
#   ylab("% OSTP Executive Orders")
# gg_ostpPerc
# 
# ggsave("output/plots/OSTP_percentbyPresident.png", dpi = 300, width = 4, height = 3)


##### PLOT: Total EOs by President ######
# Plot all
gg_totEoALL <- ggplot(data = prezOSTPall, aes(x = president, y = total)) +
  geom_line(group = 1, colour = "#3182bd", size = 1) +
  theme_classic(base_size = 12) +
  scale_y_continuous(breaks = seq(0, 1000, 200), limits = c(0, 1000)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 8)) +
  ylab("Total Executive Orders")
gg_totEoALL

ggsave("output/plots/OSTP_totalEOsALL.png", dpi = 300, width = 4, height = 3)

# Plot long only
# gg_totEo <- ggplot(data = prezOSTP, aes(x = president, y = total)) +
#   geom_line(group = 1, colour = "#3182bd", size = 1) +
#   theme_classic(base_size = 12) +
#   scale_y_continuous(breaks = seq(0, 1000, 200), limits = c(0, 1000)) +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1),
#         axis.title.x = element_blank(),
#         axis.title.y = element_text(size = 8)) +
#   ylab("Total Executive Orders")
# gg_totEo
# 
# ggsave("output/plots/OSTP_totalEOs.png", dpi = 300, width = 4, height = 3)

# multigrid plot
png(filename = "output/plots/OSTP_totalByPercALL.png", width = 8, height = 3, units = "in", res = 300)
multiplot(gg_totEoALL, gg_ostpPercALL, cols = 2)
dev.off()

# png(filename = "output/plots/OSTP_totalByPerc.png", width = 8, height = 3, units = "in", res = 300)
# multiplot(gg_totEo, gg_ostpPerc, cols = 2)
# dev.off()
