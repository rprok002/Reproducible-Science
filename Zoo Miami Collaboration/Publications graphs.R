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
    Total_Publications = c(38,0,29,17,1201,26,31,0,159,1,50,0,74,27,9,12,7,9,26,117,6,3)
)
pubtable
ID <- c("Zoo Miami", "Lion County Safari", "Palm Beach Zoo", "Naples Zoo", 
        "Mote Marine Laboratory and Zoo", "Brevard Zoo", "Seaworld Orlando",
        "The Seas", "Disney World", "Discovery Cove", "Florida Aquarium", 
        "Sea Life Orlando Aquarium", "Busch Gardens Tampa Bay", "Zoo Tampa", 
        "Central Florida Zoo and Botanical Gardens", 
        "St. Augustine Alligator Farm", "Santa Fe College Teaching Zoo", 
        "Lubee Bat Conservancy", "Jacksonville Zoo and Gardens", 
        "White Oak Conservation Center", "Lemur Conservation Foundation", 
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
