## making data table 
library(data.table)
pubtable = data.table(
    ID = c("Zoo Miami", "Lion County Safari", "Palm Beach Zoo", "Naples Zoo", 
           "Mote Marine Laboratory and Zoo", "Brevard Zoo", "Seaworld Orlando",
           "Disney World", "Discovery Cove", "Florida Aquarium", 
           "Sea Life Orlando Aquarium", "Busch Gardens Tampa Bay", "Zoo Tampa", 
           "Central Florida Zoo and Botanical Gardens", 
           "St. Augustine Alligator Farm", "Santa Fe College Teaching Zoo", 
           "Lubee Bat Conservancy", "Jacksonville Zoo and Gardens", 
           "White Oak Conservation Center", "Lemur Conservation Foundation", 
           "Natural Encounters Inc."),
    Threatend_Florida_Species_only = c(3,0,1,0,65,2,5,3,0,10,0,2,15,5,3,0,0,2,6,0,0),
    Conservation_Publications_only = c(7,0,3,2,176,1,0,2,1,7,0,1,0,0,0,0,0,3,7,1,0),
    Both = c(0,0,1,0,132,3,0,1,0,12,0,1,1,3,0,2,0,0,1,0,0),
    Neither = c(28,0,24,15,828,20,26,153,0,30,0,70,11,1,9,5,9,21,103,5,3),
    Total_Publications = c(38,0,29,17,1201,26,31,159,1,50,0,74,27,9,12,7,9,26,117,6,3)
)
pubtable[is.na(pubtable)] = 0
pubtable
ID <- c("Zoo Miami", "Lion Safari", "Palm Beach Zoo", "Naples Zoo", 
        "Mote Marine", "Brevard Zoo", "Seaworld Orlando",
        "Disney World", "Discovery Cove", "Florida Aquarium", 
        "Sea Life Orlando Aquarium", "Busch Gardens", "Zoo Tampa", 
        "Central Florida Zoo + Gardens", 
        "St. Augustine Alligator Farm", "Santa Fe  Teaching Zoo", 
        "Lubee Bat", "Jacksonville Zoo + Gardens", 
        "White Oak", "Lemur Conservation Foundation", 
        "Natural Encounters Inc.")
TFSonly <- c(3,0,1,0,65,2,5,3,0,1,0,2,15,5,3,0,0,2,6,0,0)
CPonly <- c(7,0,3,2,176,1,0,2,1,7,0,1,0,0,0,0,0,3,7,1,0)
Both <- c(0,0,1,0,132,3,0,1,0,12,0,1,1,3,0,2,0,0,1,0,0)
Neither <- c(28,0,24,15,828,20,26,153,0,30,0,70,11,1,9,5,9,21,103,5,3)
TP <- c(38,0,29,17,1201,26,31,159,1,50,0,74,27,9,12,7,9,26,117,6,3)

pubdataframe <- data.frame(ID, TFSonly, CPonly, Both, Neither, TP) 
pubdataframe
percentTFSonly <- (pubdataframe[,2]/pubdataframe[,6])*100
percentTFSonly
percentCPonly <- (pubdataframe[,3]/pubdataframe[,6])*100
percentCPonly
percentboth <- (pubdataframe[,4]/pubdataframe[,6])*100
percentboth
percentneither <- (pubdataframe[,5]/pubdataframe[,6])*100
percentneither

pubdataframe <- data.frame(ID, TFSonly, CPonly, Both, Neither,
                           TP, percentTFSonly, percentCPonly, percentboth, 
                           percentneither) 
pubdataframe[is.na(pubdataframe)] = 0
pubdataframe

value <-c(pubdataframe$TFSonly, pubdataframe$CPonly, pubdataframe$Both)
type <- c(rep("bTFSonly",21),rep("aCPonly",21),rep("cBoth",21))
IDorder <- rep(ID,3)
totalsdata <- data.frame(IDorder, value, type)
totalsdata

percentvalue <-c(pubdataframe$percentTFSonly, pubdataframe$percentCPonly, 
                 pubdataframe$percentboth,pubdataframe$percentneither)
percentvalue
percenttype <-c(rep("bPercentTFSonly",21),rep("aPercentCPonly",21),
                rep("cPercentboth",21),rep("dPercentneither",21))
percenttype
IDorderpercent <- rep(ID,4)
IDorderpercent
percentsdata <- data.frame(IDorderpercent, percentvalue, percenttype)
percentsdata
library(tidyr)

## making the graphs
library(ggplot2)
plot <- ggplot(totalsdata, aes(IDorder, value, fill=type))
plot <- plot + geom_bar(stat = "identity", position = position_dodge(width=0.8), width = 0.7) + theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  geom_text(aes(label=value), position=position_dodge(width=0.7), vjust=-0.5, size = 2.5, fontface = "bold")+
  xlab("Institution")+ ylab("Number of Publications")+ 
  scale_fill_manual(values = c("magenta1", "plum1", "mediumorchid"), 
                    labels=c("Conservation Only", "Florida Threatened Species Only",
                             "Conservation and\nFlorida Threatened Species"))+
  theme(legend.title = element_blank(),
        legend.text = element_text(color = "black", size = 8),
        legend.position = "top")+
  theme(plot.margin = margin(0.5,0.5,0.5,0.5, "cm"))
plot 


plot2 <- ggplot(percentsdata, aes(IDorderpercent, percentvalue, fill=percenttype))
plot2 <- plot2 + geom_bar(stat = "identity", position = "stack") + theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  xlab("Institution")+ ylab("Percent of Total Publications")+ 
  scale_fill_manual(values = c("magenta1", "plum1", "mediumorchid","red4"), 
                    labels=c("Conservation\nOnly", "Florida Threatened\nSpecies Only",
                             "Conservation and\nFlorida Threatened\nSpecies",
                             "Neither"))+
  theme(legend.title = element_blank(), 
        legend.text = element_text(color = "black", size = 8),
        legend.position = "top")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(plot.margin = margin(0.5,0.5,0.5,0.5, "cm"))
plot2

plot3 <- ggplot(pubdataframe, aes(ID, TP))
plot3 <- plot3 + geom_bar(stat = "identity", fill = "darkmagenta") + theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  geom_text(aes(label=TP), position=position_dodge(width=0.95), vjust=-0.25, size = 3)+
  xlab("Institution")+ ylab(" Total Publications")+ 
  theme(plot.margin = margin(0.5,0.5,0.5,0.5, "cm"))
plot3

