# ==================================================================
# title: clean data script
# description: 
# ==================================================================


#test script
library(testthat)
library(readr)

#source in functions to be tested
source('stat133-hws-fall17/hw04/code/functions.R')

#read CSV file with the raw raw
raw <- read.csv('stat133-hws-fall17/hw04/data/rawdata/rawscores.csv')

#sink
sink('stat133-hws-fall17/hw04/output/summary-rawscores.txt')
str(raw)
for (i in 1:16){
  summary_stats(unlist(raw[i]))
  print_stats(unlist(raw[i]))
}
sink()

#replace all missing values NA with zero
for (i in 1:16){
  flag <- is.na(raw[[i]])
  raw[[i]][flag] <- 0
}

#rescale quizzes
raw['QZ1'] <- rescale100(raw['QZ1'],0, 12)
raw['QZ2'] <- rescale100(raw['QZ2'],0, 18)
raw['QZ3'] <- rescale100(raw['QZ3'],0, 20)
raw['QZ4'] <- rescale100(raw['QZ4'],0, 20)

raw['Test1'] <- rescale100(raw['EX1'], 0, 80)
raw['Test2'] <- rescale100(raw['EX2'], 0, 90)
#raw['Lab'] <- score_lab(raw['ATT'])

x <- rep(0, nrow(raw))
for (i in 1:nrow(raw)){
  x[i] <- score_lab(raw[['ATT']][i])
}

raw['Lab'] <- x

#add Homework and Quiz column
a <- rep(0, nrow(raw))
b <- rep(0, nrow(raw))
for (i in 1:nrow(raw)){
  a[i] <- score_homework(as.numeric(raw[i,1:9]))
  b[i] <- score_quiz(as.numeric(raw[i,11:14]))}
raw$Homework <-  a
raw$Quiz <-  b

#add Overall column
c <- rep(0, nrow(raw))
for (i in 1:nrow(raw)){
  c[i] <- (0.1*raw[[i, "Lab"]] + 0.3*raw[[i, 'Homework']] + 0.15*raw[[i, 'Quiz']]+ 0.2*raw[[i, 'Test1']] + 0.25*raw[[i, 'Test2']])
}
raw$Overall <-  c
#raw$Overall = as.numeric(raw$Overall)

#add Grade column
d <- rep(0, nrow(raw))
for (i in 1:nrow(raw)){
  if(raw$Overall[i]<50){d[i] <- 'F'}
  else if (raw$Overall[i]>=50 & raw$Overall[i]<60){d[i] <- 'D'}
  else if (raw$Overall[i]>=60 & raw$Overall[i]<70){d[i] <- 'C-'}
  else if (raw$Overall[i]>=70 & raw$Overall[i]<77.5){d[i] <- 'C'}
  else if (raw$Overall[i]>=77.5 & raw$Overall[i]<79.5){d[i] <- 'C+'}
  else if (raw$Overall[i]>=79.5 & raw$Overall[i]<82){d[i] <- 'B-'}
  else if (raw$Overall[i]>=82 & raw$Overall[i]<86){d[i] <- 'B'}
  else if (raw$Overall[i]>=86 & raw$Overall[i]<88){d[i] <- 'B+'}
  else if (raw$Overall[i]>=88 & raw$Overall[i]<90){d[i] <- 'A-'}
  else if (raw$Overall[i]>=90 & raw$Overall[i]<95){d[i] <- 'A'}
  else if (raw$Overall[i]>=95 & raw$Overall[i]<=100){d[i] <- 'A+'}
}
raw$Grade <- d

#summary and print stats to export 
coln = c("Lab", "Homework", "Quiz", "EX1", "EX2", "Overall")
filen = c("Lab", "Homework", "Quiz", "Test1", "Test2", "Overall")
for (i in 1:length(coln)){
  colname = coln[i]
  filename = filen[i]
  sink(paste("stat133-hws-fall17/hw04/output/", filename, "-stats.txt"))
  summary_stats(unlist(raw[,filename]))
  print_stats(summary_stats(unlist(raw[,filename])))
  sink()
}




write.csv(raw, file = "stat133-hws-fall17/hw04/data/cleandata/cleanscores.csv")

