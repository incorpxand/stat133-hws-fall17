## Clean Data Script

# This script cleans data from the rawscores CSV in order to prepare the data
# for use in this homework.

library(readr)

rawscores <- read_csv("C:\\Users\\Xander\\Documents\\College Yo\\Fall Semester 2017\\Stat 133\\stat133\\stat133-hws-fall17\\hw04\\data\\rawdata\\rawscores.csv")


sink("C:\\Users\\Xander\\Documents\\College Yo\\Fall Semester 2017\\Stat 133\\stat133\\stat133-hws-fall17\\hw04\\output\\summary-rawscores.txt")
str(rawscores)
sink()

for (j in 1:16) {
  rawscores[ , j][is.na(rawscores[ , j])] <- 0
  }
rawscores

rescale100(rawscores$QZ1, 0, 12)
rescale100(rawscores$QZ2, 0, 18)
rescale100(rawscores$QZ3, 0, 20)
rescale100(rawscores$QZ4, 0, 20)

Test1 <- rescale100(rawscores$EX1, 0, 80)
Test1

Test2 <- rescale100(rawscores$EX2, 0, 90)
Test2

rawmatrix = as.data.frame(lapply(rawscores, as.numeric))

home_frame <- matrix(ncol = 8, nrow = nrow(rawmatrix))
for (row in 1:nrow(rawmatrix)) {
  home_frame[row, ] <- as.numeric(drop_lowest(rawmatrix[row, 1:9]))
}

home_frame

avg_frame <- matrix(ncol = 1, nrow = nrow(home_frame))

for (row in 1:nrow(home_frame)) {
  avg_frame[row, ] <- as.numeric(mean(home_frame[row, 1:8]))
}

avg_frame

rawmatrix$Homework <- avg_frame
rawmatrix

lab_frame <- matrix(ncol = 1, nrow = nrow(home_frame))

for (row in 1:nrow(home_frame)) {
  lab_frame[row, ] <- as.numeric(score_lab(rawmatrix[row, 10]))
}

lab_frame

rawmatrix$labscore <- lab_frame

quiz_frame <- matrix(ncol = 3, nrow = nrow(rawmatrix))
for (row in 1:nrow(rawmatrix)) {
  quiz_frame[row, ] <- as.numeric(drop_lowest(rawmatrix[row, 11:14]))
}
quiz_frame

avg_quiz <- matrix(ncol = 1, nrow = nrow(quiz_frame))

for (row in 1:nrow(home_frame)) {
  avg_quiz[row, ] <- as.numeric(mean(quiz_frame[row, 1:3]))
}
avg_quiz

rawmatrix$Quiz <- avg_quiz

rescale100(avg_quiz, 0, 20)

overall_score <- 0.1 * rawmatrix$labscore + 0.3 * rawmatrix$Homework + .15 * rescale100(avg_quiz, 0, 20) + .2 * rescale100(rawmatrix$EX1, 0, 80) + .25 * rescale100(rawmatrix$EX2, 0, 90)

rawmatrix$Overall <- overall_score

grades <- matrix(ncol = 1, nrow = nrow(rawmatrix))

for (row in 1:nrow(rawmatrix)) {
  if (rawmatrix$Overall[row, 1] < 50) {
    grades[row, 1] = 'F'
  } else if (rawmatrix$Overall[row, 1] >= 50 & rawmatrix$Overall[row, 1] < 60) {
    grades[row, 1] = 'D'
  } else if (rawmatrix$Overall[row, 1] >= 60 & rawmatrix$Overall[row, 1] < 70) {
    grades[row, 1] = 'C-'
  } else if (rawmatrix$Overall[row, 1] >= 70 & rawmatrix$Overall[row, 1] < 77.5) {
    grades[row, 1] = 'C'
  } else if (rawmatrix$Overall[row, 1] >= 77.5 & rawmatrix$Overall[row, 1] < 79.5) {
    grades[row, 1] = 'C+'
  } else if (rawmatrix$Overall[row, 1] >= 79.5 & rawmatrix$Overall[row, 1] < 82) {
    grades[row, 1] = "B-"
  } else if (rawmatrix$Overall[row, 1] >= 82 & rawmatrix$Overall[row, 1] < 86) {
    grades[row, 1] = 'B'
  } else if (rawmatrix$Overall[row, 1] >= 86 & rawmatrix$Overall[row, 1] < 88) {
    grades[row, 1] = 'B+'
  } else if (rawmatrix$Overall[row, 1] >= 88 & rawmatrix$Overall[row, 1] < 90) {
    grades[row, 1] = 'A-'
  } else {
    grades[row, 1] = 'A'
  }
}

rawmatrix$Grade <- grades

sink("C:\\Users\\Xander\\Documents\\College Yo\\Fall Semester 2017\\Stat 133\\stat133\\stat133-hws-fall17\\hw04\\output\\lab-stats.txt")
summary_stats(rawmatrix$labscore)
print_stats(rawmatrix$labscore)
sink()

sink("C:\\Users\\Xander\\Documents\\College Yo\\Fall Semester 2017\\Stat 133\\stat133\\stat133-hws-fall17\\hw04\\output\\homework-stats.txt")
summary_stats(rawmatrix$Homework)
print_stats(rawmatrix$Homework)
sink()

sink("C:\\Users\\Xander\\Documents\\College Yo\\Fall Semester 2017\\Stat 133\\stat133\\stat133-hws-fall17\\hw04\\output\\quiz-stats.txt")
summary_stats(rawmatrix$Quiz)
print_stats(rawmatrix$Quiz)
sink()

sink("C:\\Users\\Xander\\Documents\\College Yo\\Fall Semester 2017\\Stat 133\\stat133\\stat133-hws-fall17\\hw04\\output\\Test1-stats.txt")
summary_stats(rawmatrix$EX1)
print_stats(rawmatrix$EX1)
sink()

sink("C:\\Users\\Xander\\Documents\\College Yo\\Fall Semester 2017\\Stat 133\\stat133\\stat133-hws-fall17\\hw04\\output\\Test2-stats.txt")
summary_stats(rawmatrix$EX2)
print_stats(rawmatrix$EX2)
sink()

sink("C:\\Users\\Xander\\Documents\\College Yo\\Fall Semester 2017\\Stat 133\\stat133\\stat133-hws-fall17\\hw04\\output\\overall-stats.txt")
summary_stats(rawmatrix$Overall)
print_stats(rawmatrix$Overall)
sink()

sink("C:\\Users\\Xander\\Documents\\College Yo\\Fall Semester 2017\\Stat 133\\stat133\\stat133-hws-fall17\\hw04\\output\\summary-cleanscores.txt")
str(rawmatrix)
sink()

sink("C:\\Users\\Xander\\Documents\\College Yo\\Fall Semester 2017\\Stat 133\\stat133\\stat133-hws-fall17\\hw04\\data\\cleandata\\cleanscores.csv")
rawmatrix
sink()
