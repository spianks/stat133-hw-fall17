# ==================================================================
# title: tester-sript
# description: sink tests.R
# ==================================================================
#test script
library(testthat)

#source in functions to be tested
source('stat133-hws-fall17/hw04/code/functions.R')


sink('stat133-hws-fall17/hw04/output/test-reporter.txt')
test_file('stat133-hws-fall17/hw04/code/tests.R')
sink()


