## Chapter 1 Analysis Loop 
library(gtools)
library(glue)
## load data table
data <- read.csv(file.choose())


new.df <- data.frame(matrix(NA,    # Create empty data frame
                            nrow = 1,
                            ncol = 5))

names(new.df) <- c('Frognumber', 'Trialtype', 'Trialnumber', 'Location', 'Seconds')
  
  
for( a in unique(data$Frog.Number)){
  for( b in unique(data$Trial.Type)){
    #print(b)
    for( c in unique(data$Trial.Number)){
      #print(c)
      
      ## Filter (F = frog number, T = trial type, N = trial number)
      F1T1N1 <- data[which(data$Frog.Number == a & data$Trial.Type == b & data$Trial.Number == c),]
      
      ## Random sample
      F1T1N1s <- F1T1N1[sample(nrow(F1T1N1), 180, replace = TRUE),]
      
      ## Filter by location
      F1T1N1c <- F1T1N1s[which(F1T1N1s$Location == 1),]
      F1T1N1e <- F1T1N1s[which(F1T1N1s$Location == 2),]
      
      ## New dataframe with sums
      Control <- c(sum(F1T1N1c$Number.of.Seconds)/180)
      Experiment <- c(sum(F1T1N1e$Number.of.Seconds)/180)
 
      # Make the DF
      l.df <- data.frame( 'Frognumber',
        'Trialtype',
        'Trialnumber',
        'Location' <- c("C", "E"),
        'Seconds')
 
      l.df$X.Frognumber. <- a
      l.df$X.Trialtype. <- b
      l.df$X.Trialnumber. <- c
      l.df$X.Seconds.<- c(Control,Experiment)
  
      names(l.df) <- c('Frognumber', 'Trialtype', 'Trialnumber', 'Location', 'Seconds')
   
      new.df <- smartbind(new.df, l.df)
      }}}

new.df <- na.omit(new.df)
print(new.df)
## subset control and experiment for each frog, trial type and trial number

newF1T1N1c <- new.df[which(new.df$Frognumber == 1 & new.df$Trialtype == 1 & new.df$Trialnumber == 1 & new.df$Location == 'C'),]
newF1T1N1c
newF1T1N1e <- new.df[which(new.df$Frognumber == 1 & new.df$Trialtype == 1 & new.df$Trialnumber == 1 & new.df$Location == 'E'),]
newF1T1N1e
## means for simulated control and experiment proportions/totals
mean(as.numeric(newF1T1N1c$Seconds))
mean(as.numeric(newF1T1N1e$Seconds))

## 95% confidence intervals for simulations (p = means for simulated control and experiment proportions, n = number of simulations)
marginF1T1N1c <- qnorm(0.975)*sqrt(mean(as.numeric(newF1T1N1c$Seconds))*(1-mean(as.numeric(newF1T1N1c$Seconds)))/10)
mean(as.numeric(newF1T1N1c$Seconds)) - marginF1T1N1c
mean(as.numeric(newF1T1N1c$Seconds)) + marginF1T1N1c
glue ("({mean(as.numeric(newF1T1N1c$Seconds)) - marginF1T1N1c}, {mean(as.numeric(newF1T1N1c$Seconds)) + marginF1T1N1c})")
marginF1T1N1e <- qnorm(0.975)*sqrt(mean(as.numeric(newF1T1N1e$Seconds))*(1-mean(as.numeric(newF1T1N1e$Seconds)))/10)
mean(as.numeric(newF1T1N1e$Seconds)) - marginF1T1N1e
mean(as.numeric(newF1T1N1e$Seconds)) + marginF1T1N1e
glue ("({mean(as.numeric(newF1T1N1e$Seconds)) - marginF1T1N1e}, {mean(as.numeric(newF1T1N1e$Seconds)) + marginF1T1N1e})")
## If confidence interval includes the true proportions, gives me confidence that the proportion would have come out even with a better study 

