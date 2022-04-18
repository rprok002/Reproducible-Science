## making data table 
library(data.table)
pubtable = data.table(
    ID = c("Zoo Miami", "Lion County Safari", "Palm Beach Zoo", "Naples Zoo", 
           "Mote Marine Laboratory and Zoo", "Brevard Zoo", "Seaworld Orlando",
           "The Seas", "Disney World", "Discovery Cove", "Florida Aquarium", 
           "Sea Life Orlando Aquarium", "Busch Gardens Tampa Bay", "Zoo Tampa", 
           "Central Florida Zoo and Botanical Gardens", 
           "St. Augustine Alligator Farm", "Santa Fe College Teaching Zoo", 
           "Lubee Bat Conservancy", "Jacksonville Zoo and Gardens", 
           "White Oak Conservation Center", "Lemur Conservation Foundation", 
           "Natural Encounters Inc."),
    Threatend_Florida_Species = c(3,0,2,0,197,5,5,0,4,0,13,0,3,16,8,3,2,0,2,7,0,0),
    Conservation_Publications = c(7,0,4,2,308,4,0,0,3,1,9,0,2,1,3,0,2,0,3,8,1,0),
    Total_Publications = c(38,0,29,17,1201,26,31,0,159,1,50,0,74,27,9,12,7,9,26,117,6,3),
    Percent_Florida_Species = percentTFS,
    Percent_Conservation = percentCP
)
pubtable[is.na(pubtable)] = 0
pubtable
ID <- c("Zoo Miami", "Lion Safari", "Palm Beach Zoo", "Naples Zoo", 
        "Mote Marine", "Brevard Zoo", "Seaworld Orlando",
        "The Seas", "Disney World", "Discovery Cove", "Florida Aquarium", 
        "Sea Life Orlando Aquarium", "Busch Gardens", "Zoo Tampa", 
        "Central Florida Zoo + Gardens", 
        "St. Augustine Alligator Farm", "Santa Fe  Teaching Zoo", 
        "Lubee Bat", "Jacksonville Zoo + Gardens", 
        "White Oak", "Lemur Conservation Foundation", 
        "Natural Encounters Inc.")
TFS <- c(3,0,2,0,197,5,5,0,4,0,13,0,3,16,8,3,2,0,2,7,0,0)
CP <- c(7,0,4,2,308,4,0,0,3,1,9,0,2,1,3,0,2,0,3,8,1,0)
TP <- c(38,0,29,17,1201,26,31,0,159,1,50,0,74,27,9,12,7,9,26,117,6,3)

pubtable
pubdataframe <- data.frame(ID, TFS, CP, TP, percentTFS, percentCP) 
pubdataframe
percentTFS <- pubdataframe[,2]/pubdataframe[,4]
percentTFS
percentCP <- pubdataframe[,3]/pubdataframe[,4]
percentCP
pubdataframe[is.na(pubdataframe)] = 0
pubdataframe

library(tidyr)
pubdataframeTFSCP <- gather(pubdataframe, event, total , TFS:CP)
pubdataframeTFSCP
## making the graphs
library(ggplot2)
plot <- ggplot(pubdataframeTFSCP, aes(ID, total, fill=event))
plot <- plot + geom_bar(stat = "identity", position = position_dodge(width=0.7)) + theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  geom_text(aes(label=total), position=position_dodge(width=0.7), vjust=-0.25, size = 3)+
  xlab("Institution")+ ylab("Number of Publications")+ 
  scale_fill_manual(values = c("chartreuse3", "maroon2"), name = "Legend", labels=c("Conservation", "Florida Threatened Species"))+
  theme(legend.title = element_text(color = "black", size = 9), 
        legend.text = element_text(color = "black", size = 8))+
  ggtitle("Number of Conservation and Florida Threatened Species Publications \n by Institution")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(plot.margin = margin(0.5,1,1,1, "cm"))
plot 

