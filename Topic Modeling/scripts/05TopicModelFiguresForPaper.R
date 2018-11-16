##################################################################################################
#
# Topic Model graphs for paper
#
##################################################################################################

rm(list  = ls())

# Load topic models
load("Topic Modeling/data/EO_models.Rdata")
source("scripts/000-utilityFunctionsEO.R")

# Load packages
required.packages <- c("dplyr",
                       "igraph", 
                       "devtools", #for installing ida TM pkgs
                       "idaTopicModels",
                       "idaMakeDtm",
                       "reshape2",
                       "ggplot2")
LoadPackages(required.packages = required.packages)


###################################
# Coherence plot
###################################
# Standard plot
plot(metric_mat$k, metric_mat$coherence, type = "o")#always gets right number of topics in simulated data

# coordinates of topic model selected
selected_model <- metric_mat[metric_mat$k == 80,]

# ggplot version
ggCoherence <- ggplot(metric_mat) +
  geom_line(aes(x = k, y = coherence), 
            size = 1.25) +
  geom_point(aes(x = k, y = coherence), 
             data = selected_model,
             size = 5, 
             colour = "red") +
  geom_point(aes(x = k, y = coherence), 
             size = 5, 
             shape = 1) +
  scale_y_continuous(limit = c(0.189, 0.215), 
                     expand = c(0, 0)) +
  scale_x_continuous(breaks = seq(20, 200, 20)) +
  theme_bw(base_size = 16) +
  theme(axis.line = element_line(size = 0.8, color = "black"),
        axis.text = element_text(size = 13),
        panel.grid.major = element_line(colour = "grey70")) +
  ylab("Probabilistic Coherence\n") +
  xlab("\nk")



ggCoherence

ggsave(file = "Topic Modeling/output/plots/CoherencePlot.png",
       width = 8,
       height = 8,
       dpi = 300)





