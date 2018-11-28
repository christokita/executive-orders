rm(list = ls())

library(stringr)
library(dplyr)

load("output/eosCuratedWithS&TQuery.RData")

eos <- arrange(eos, desc(stMatchRatio))

eos <- select(eos, -text, -title.plus.text, -matchWords.num, -stMatch.count)


tm <- read.csv(file = "Topic Modeling/output/EO_Topic_k80_DocTopMatrix.csv",
              stringsAsFactors = FALSE)


#t33 = science topic
#t76 = energy topic

sci.docs <- tm[tm$t.33 > 0 | 
               tm$t.76 > 0, ]$X

eos.tm <- filter(eos, num %in% sci.docs)

write.csv(x = eos.tm, 
          file = "output/topic.model.revew.csv", 
          row.names = FALSE)

top300 <- head(eos, n = 300)

not.in.sci.topic <- filter(top300, !(num %in% sci.docs))

not.in.sci.topic$energy.flag <- grepl("energy|nuclear|AEC|ERDA|DOE", not.in.sci.topic$matchWords)

not.in.sci.topic$medical.flag <- grepl("medical", not.in.sci.topic$matchWords)

not.in.sci.topic <- arrange(not.in.sci.topic, 
                            desc(energy.flag), 
                            desc(medical.flag), 
                            desc(stMatchRatio))


write.csv(x = not.in.sci.topic, 
          file = "output/st.eos.not.in.sci.topic.csv",
          row.names = FALSE)