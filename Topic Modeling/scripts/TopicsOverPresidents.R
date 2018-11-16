##################################################################################################
#
# Topic Modeling Executive Orders -- Plotting Topic over Time
#
##################################################################################################

rm(list=ls())

library(dplyr)
library(igraph)
library(plyr)
library(ggplot2)
library(reshape2)

##################################################################################################
# Initial data load 
##################################################################################################
# Load ST t.40 dtm
load(file = "Topic Modeling/output/ST40dtm")

# Load President EO range dataframe
presRanges <- read.csv(file = "data/PresidentOrderRanges.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)


##################################################################################################
# Assign president to EO
##################################################################################################
# Create EO ranges
presRanges$President <- paste0(presRanges$Number, " - ", presRanges$President)

pres <- apply(presRanges, 1, function(x){
  
  start <- x[2]
  end <- x[3]
  eoRange <- start:end
  
  out <- data.frame(president=rep(x[1], length(eoRange)), num=eoRange, stringsAsFactors=F)
})
pres <- ldply(pres)

# Merge DTM rownames and President
eos <- data.frame(num = as.integer(row.names(ST40.doc.topic)))
eos <- merge(eos, pres, by = "num", all.x = T)

# Reassign DTM rownames as presidents
row.names(ST40.doc.topic) <- eos$president

# DTM to dataframe for dplyr
ST40.doc.topic <- as.data.frame(ST40.doc.topic)
ST40.doc.topic$President <- row.names(ST40.doc.topic)

# Aggregate
presTopicSum <- 
  ST40.doc.topic %>%
  group_by(President) %>%
  summarise_each(funs(sum))

presTopicMean <- 
  ST40.doc.topic %>%
  group_by(President) %>%
  summarise_each(funs(mean))


##################################################################################################
# Plot
##################################################################################################
# Select topics of interest
presTopicSum <-
  presTopicSum %>%
  select(President, t.9, t.12, t.21, t.23, t.28)

presTopicMean <-
  presTopicMean %>%
  select(President, t.9, t.12, t.21, t.23, t.28)

# Melt to long form
presTopicSum <- melt(data = presTopicSum, id.vars = "President")
presTopicMean <- melt(data = presTopicMean, id.vars = "President")
  
# Plot
sums <- ggplot(data = presTopicSum , aes(x = President, y = value, group = variable)) +
  geom_line(aes(color = variable)) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ylab("Weighted Sum of Topic per Executive Order") +
  ggtitle("S&T Topics in Executive Orders (1950 - Present)")

sums


means <- ggplot(data = presTopicMean , aes(x = President, y = value, group = variable)) +
  geom_line(aes(color = variable) , size = 1) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ylab("Mean Proportion of Topic per Executive Order") +
  ggtitle("S&T Topics in Executive Orders (1950 - Present)")

means
