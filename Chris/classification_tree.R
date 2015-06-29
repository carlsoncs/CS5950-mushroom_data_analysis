source('Functions.R')
########################################################################
#						random_forest.R
########################################################################
#
#
#
#			###			Description					###
#
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
#########													  ##########


# "Grow Tree"
# Function to generate a model given a data frame object from the 
# mushrooms data set.
grow_tree <- function(data_frame) {
	rpart(edibility~., data=data_frame, method="class")
}

# "Test Fit"
# A function to generate a prediction for some data given a model.
test_fit <- function(fit, test_data) {
	pred <- data.frame(predict(fit, test_data, type='prob'))
	
	results <- array("p", nrow(test_data))
	
	for(i in 1:nrow(results))
	{
		if(pred[i, ]$e > pred[i, ]$p)
		{
			results[i] = "e"
		}
	}
	results
}

# "Get Confustion Matrix" 
# A function to compare the predictions with the labels and
# to construct a confusion matrix based on the comparison 
# results.
get_conf_matrix <- function(pred, test_data) {

	pp = 0 # counter for poisonous mushrooms predicted as poisonous
	pe = 0 # counter for poisonous mushrooms predicted as edible
	ep = 0 # counter for edible mushrooms predicted as poisonous
	ee = 0 # counter for edible mushrooms predicted as edible
	
	for(i in 1:nrow(pred))
	{
		if(pred[i] == test_data$edibility[i])
		{
			if(pred[i] == 'e')
			{
				ee = ee+1
			}
			else
			{
				pp = pp+1
			}
		}
		else
		{
			if(pred[i] == 'e')
			{
				pe = pe+1
			}	
			else
			{
				ep = ep+1
			}	
		}
	}
	conf_matrix <- matrix(c(ee, ep, pe, pp), nrow=2, ncol=2)
	rownames(conf_matrix)<-c("e", "p")
	colnames(conf_matrix)<-c("e", "p")
	conf_matrix
}



##					MAIN LOGIC					##




# This matrix will hold the final results after running the complete
# cross validation.
confusion_matrix_averages <- vector("list", (length(folds)-1))

# In this section of the code, the goal is to cross-validate over each
# of the folds.  So for i in nFolds, it will make the i'th fold the
# testing data, and it will build a tree from each of the remaining 
# folds.  Then it will test the fit of each tree on the test fold.  
#
# The results of each test are added to a list, and finally at the 
# end all the average test perfomance is calculated and reported.  
for( i in 1:(length(folds)))
{
	# Name i'th fold 'test' and add the 
	# remaining folds to a list called 'train'
	test  <- folds[[i]]
	train <- folds[c(seq(1:(length(folds))))[-i]]

	# First generate the models for each of the training data lists.
	fits  <- lapply(train, FUN=grow_tree)
	
	# Next generate predictions from each model using the test data.
	preds <- lapply(fits, FUN=test_fit, test_data=test)
	
	# Generate a list of confusion matrices.
	conf_mats <- lapply(preds, FUN=get_conf_matrix, test_data=test)
	
	# Add the average confusion matrix for this iteration to the 
	# confusion_matrix_averages list.
	confusion_matrix_averages[[i]]<- apply(simplify2array(conf_mats),
			c(1,2), mean)
	
	
	### SKIP THIS SECTION ###
	
### This Section was used to generate plots of the trees produced
### by rpart, but for this data the trees themselves are not highly
### informative so this section is skipped.  

#	par(mfrow=c(1,2), xpd=NA)
#	plot(fit, uniform=TRUE, main="Classification Tree for Mushroom Edibility")
#	text(fit, use.n=TRUE, all=TRUE, cex=.8)
#	print(fit$variable.importance)
	
#	print('\nPress Enter to Prune the Tree')
#	readline()
#	post(fit, file=paste0("results_temp/fit_", i), title="Classification Tree for Mushroom Edibility")

#	pfit <- prune(fit, cp=fit$cptable[which.min(fit$cptable[,"xerror"]), "CP"])

#	plot(pfit, uniform=TRUE, main="Pruned Classification Tree for Mushroom Edibility")
#	text(pfit, use.n=TRUE, all=TRUE, cex=.8)
#	post(pfit, file=paste0("results_temp/pfit_", i))

}
### END SKIPPED SECTION ###


# Finally, generate a single confusion matrix from all the average 
# confusion matrices and print it to the screen.
final_confusion_matrix <- apply(simplify2array(confusion_matrix_averages),
						  c(1,2), ceil_mean)
print(final_confusion_matrix)
