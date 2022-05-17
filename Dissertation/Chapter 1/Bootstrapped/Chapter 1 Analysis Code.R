## load data table
data <- read.csv(file.choose())

## Filter (F = frog number, T = trial type, N = trial number)
F1T1N1 <- data[which(data$Frog.Number == 1 & data$Trial.Type == 1 & data$Trial.Number == 1),]
F1T1N1
F1T1N2 <- data[which(data$Frog.Number == 1 & data$Trial.Type == 1 & data$Trial.Number == 2),]
F1T1N2

## Random sample
F1T1N1s <- F1T1N1[sample(nrow(F1T1N1), 180),]
F1T1N1s
F1T1N2s <- F1T1N2[sample(nrow(F1T1N2), 180),]
F1T1N2s

## Filter by location
F1T1N1c <- F1T1N1s[which(F1T1N1s$Location == 1),]
F1T1N1c
F1T1N1e <- F1T1N1s[which(F1T1N1s$Location == 2),]
F1T1N1e
F1T1N2c <- F1T1N2s[which(F1T1N2s$Location == 1),]
F1T1N2c
F1T1N2e <- F1T1N2s[which(F1T1N2s$Location == 2),]
F1T1N2e

## New dataframe with sums
Control <- c(sum(F1T1N1c$Number.of.Seconds), sum(F1T1N2c$Number.of.Seconds))
Control
Experiment <- c(sum(F1T1N1e$Number.of.Seconds), sum(F1T1N2e$Number.of.Seconds))
Experiment

Frognumber <- c(1,1,1,1)
Trialtype <- c(1,1,1,1)
Trialnumber <- c(1,1,2,2)
Location <- c("C", "C", "E", "E")
Seconds <- c(Control,Experiment)
Seconds
F1T1sum <- data.frame(Frognumber, Trialtype, Trialnumber, Location, Seconds)
F1T1sum

t.test <- t.test(Seconds~Location, data = F1T1sum, paired = TRUE)
t.test
