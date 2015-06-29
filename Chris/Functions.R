########################################################################
#						Functions .R
########################################################################
#
#
#
#			###			Description					###
#
#	Functions contains various generally aplicable methods/functions
#	that I desire to have scripted and available in a tested
#	and reliable manner.  Anything that can be refactored should
#	eventually find it's way here.
#		
#			###			By							###
#
#	Written by Christopher Carlson 
#	
#			###			For							###
#
#	Written initially for Western Michigan University's Summer 1 2015
# 		Semester course, CS 5950 - Machine Learning. 
#
#			###			Date						###
#
#	Monday June 29, 2015
#
#			INDEX
#
#	0. Preload Necessary Libraries
#
#	1. "Round-Mean" - round_mean(X, digits)
#
#	2. "Ceiling-Mean" - ceil_mean(X)
#
#	3. "Floor-Mean" - floor(X)
#
#	4. "Print-Summary" - print_summary(X)
#
#########													  ##########



## Load Libraries
library('randomForest')
library(rpart)
library(plyr)
##

##	"Round-Mean" 
## Find the mean of some collection X and then round the result to 
## digits places.  Used with apply functions.
round_mean <- function(X, digits)
{
	round(mean(X), digits)
}

## "Ceiling-Mean" 
## Find the mean of collection X and then round the result up to the
## nearest integer value.
ceil_mean <- function(X)
{
	ceiling(mean(X))
}

## "Floor-Mean" 
## Find the mean of collection X and then round the result down to the
## nearest integer value.
floor_mean <- function(X)
{
	floor(mean(X))
}

## "Print Summary"
## Print the summary of item X.
print_summary <- function(X)
{
	print(summary(X))
}












































