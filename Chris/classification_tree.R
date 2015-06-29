source('Functions.R')
########################################################################
#						classification_tree.R
########################################################################
#
#
#
#			###			Description					###
#	Runs the classification tree algorithm k-folds time and performs
#	k-folds cross validation on the results.  Writes the results to 
#	the file "classification_tree_cv_results.txt" as confusion matrices.
#		
#			###			By							###
#
#	Written by Christopher Carlson 
#	
#			###			For							###
#
#	Written initially for Western Michigan University's Summer 1 2015
# 	Semester course, CS 5950 - Machine Learning. 
#
#########													  ##########


# This matrix will hold the final results after running the complete
# cross validation.
confusion_matrix_averages <- vector("list", (length(folds)-1))

results_file <- "results/tree_classification.txt"
write("Beginning Classification Tree", results_file, append=TRUE)

fits <- list()
preds <- list()
conf_mats <- list()

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
	
	for( j in 1:(length(folds)))
	{
		if(i==1 && j==1)
		{	
			train <- folds[[2]]
			j = j+1
		}
		else
		{
		if(j==1)
		 {
			 train <- folds[[1]]
		 }
		else if(j==i)
		 {}
		else
		 {
			 train <- merge(train, folds[[j]])
		 }
		}
 	}
	

	# First generate the models for each of the training data lists.
	fits[i]  <- grow_tree(train)
	
	# Next generate predictions from each model using the test data.
	preds[i] <- test_tree(fits[i], test)
	
	# Generate a list of confusion matrices.
	conf_mats[i] <- gen_tree_conf_mat(preds[i], test)
	
}
# Write the results averages to the results file.
write_message("Results of n-Folds CV:\n", results_file)
lapply(conf_mats, FUN=write_confusion_matrix, r_file=results_file)


# Finally, generate a single confusion matrix from all the average 
# confusion matrices and write it to results.
final_confusion_matrix <- apply(simplify2array(conf_mats),
						  c(1,2), mean)
write_message(paste0("Overall Test Error for ", length(folds), 
					"-folds cross validation:\n"), results_file)
write_confusion_matrix(final_confusion_matrix, results_file)

			  
