## load data table
data <- read.csv(file.choose())

## Filter (F = frog number, T = trial type, N = trial number)
F1T1N1 <- data[which(data$Frog.Number == 1 & data$Trial.Type == 1 & data$Trial.Number == 1),]
F1T1N1

## Random sample
F1T1N1s <- F1T1N1[sample(nrow(F1T1N1), 180),]
F1T1N1s

## Filter by location
F1T1N1c <- F1T1N1s[which(F1T1N1s$Location == 1),]
F1T1N1c
F1T1N1e <- F1T1N1s[which(F1T1N1s$Location == 2),]
F1T1N1e
Control <- sum(F1T1N1c$Number.of.Seconds)
Experiment <- sum(F1T1N1e$Number.of.Seconds)

Location <- c("C", "E")
Seconds <- c(Control,Experiment)
F1T1N1sum <- data.frame(Location, Seconds)
F1T1N1sum
