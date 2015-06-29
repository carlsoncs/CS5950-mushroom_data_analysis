source('Functions.R')
########################################################################
#						random_forest.R
########################################################################
#
#
#
#			###			Description					###
#	Runs k-folds cross validation on random forest classifier for 
#	mushroom data k times and displayes the results in the file 
# 	'rand_forest_results.txt'.  (This is major overkill because
#	the package 'random-forest' conducts it's own cross validation
#	as a part of model generation, but I am doing it explicitly to
# 	demonstrate the results over k-folds first hand.)
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

results_file='results/rf_results.txt'

# This matrix will hold the final results after running the complete
# cross validation.
confusion_matrix_averages <- vector("list", (length(folds)-1))



# Begin results file.
write("\t\tBEGIN RANDOM FOREST RESULTS\n", file=results_file,
	  ncolumns = 1, append=FALSE)
			
			
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
	fits  <- lapply(train, FUN=grow_forest, test_data=test)

	# Generate a list of confusion matrices.
	conf_mats <- lapply(fits, FUN=get_rf_conf_mat)
	
	# Add the average confusion matrix for this iteration to the 
	# confusion_matrix_averages list.
	confusion_matrix_averages[[i]]<- apply(simplify2array(conf_mats),
			c(1,2), mean)
	
	# Print the confusion matrices to the file.
	write_message(paste0("Iteration ", i, ":\n"), results_file)
	lapply(conf_mats, FUN=write_confusion_matrix, r_file=results_file)
			
 }
 
# Write the average confusion matrices to the results file.
write_message("Averages of each CV iteration:\n", results_file)
lapply(confusion_matrix_averages, FUN=write_confusion_matrix, r_file=results_file)
 
# Finally, generate a single confusion matrix from all the average 
# confusion matrices and write it to file.
final_confusion_matrix <- apply(simplify2array(confusion_matrix_averages),
						  c(1,2), mean)
write_message(paste0("Average Confusion Matrix from ", length(folds), 
					"-folds cross validation:\n"), results_file)
write_confusion_matrix(final_confusion_matrix, results_file)

