# test script
library(testthat)

# source in functions to be tested
source('functions.R')

sink('Users/Xander/Documents/College Yo/Fall Semester 2017/Stat 133/stat133/stat133-hws-fall17/hw04/output')
test_file('tests.R')
sink()
