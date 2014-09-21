#================================================================
# File:         mortalityratesplot.R   
# Name:         Programming Assignment 3: Hospital Quality 
# Desc:         This file plots the 30-day mortality rates for 
#               heart attack patients
# Auth:         Ryan Samo
# Date:         9-20-2014
#================================================================

#Read the outcome data into R via the read.csv function and look at the first few rows.
outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
head(outcome)

#Check the number of columns
ncol(outcome)

#Check the number of rows
nrow(outcome)

#See the names of each of the columns
names(outcome)

#Make a simple histogram of the 30-day death rates from heart attack (column 11 in the outcome dataset)
outcome[, 11] <- as.numeric(outcome[, 11])
hist(outcome[, 11])