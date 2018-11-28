rm(list = ls())

library(dplyr)
library(stringr)

load("output/eosCuratedWithS&TQuery.RData")

eos$title.plus.num <- paste0(eos$num, ": ", eos$title)
eos$num <- str_replace_all(eos$num, "-[A-Z]", "")
eos <- arrange(eos, as.numeric(num))
eos$matchWords <- str_replace_all(eos$matchWords, "__", "")

eos <- select(eos, president, num, title, matchWords, word.count, stMatchRatio, link)

eos$label <- paste0(eos$president, ": ", eos$title, " (" , eos$num, ")")


nasa  <- eos[grepl("NASA", eos$matchWords), ]
nsf   <- eos[grepl("NSF", eos$matchWords), ]
pcast <- eos[grepl("PCAST", eos$matchWords), ]
nstc  <- eos[grepl("NSTC", eos$matchWords), ]
doe   <- eos[grepl("AEC|ERDA|DOE", eos$matchWords), ]
ostp <- eos[grepl("OSTP", eos$matchWords), ]

topics <- read.csv(file = "data/nasa.eo.topics.csv", stringsAsFactors = FALSE)

nasa.table <- merge(nasa.table, topics)


write.csv(nasa, file = "output/nasaTable.csv", row.names = FALSE)
write.csv(nsf, file = "output/nsfTable.csv", row.names = FALSE)
write.csv(ostp, file = "output/ostpTable.csv", row.names = FALSE)
write.csv(pcast, file = "output/pcastTable.csv", row.names = FALSE)


eos <- arrange(eos, desc(stMatchRatio))

write.csv(eos, file = "all.csv", row.names = F)

stats <- nasa.table %>%
  group_by(president, topic) %>%
  tally() %>%
  as.data.frame()


library(ggplot2)

p <- ggplot(stats, aes(x = president, y = n)) + 
  geom_line() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
p



