library(rpart)
library(plyr)	

# Function to generate a fit given a data frame object from the 
# mushrooms data set.

grow_tree <- function(data_frame) {

	rpart(edibility~., data=data_frame, method="class")

}

get_fold <- function(list_of_folds, skip) {

	list_of_folds[[integer_value]]
}

print_summary <- function(item) {
	print(summary(item))
}

test_fit <- function(fit, test_data) {
	predict(fit, test_data, type='class')

}

for( i in 1:length(folds))
{
	# To Improve Code Clarity, name i 'test' and the rest 'train'
	test  <- folds[[i]]
	train <- folds[c(seq(1:length(folds)))[-i]]
	
	fit <- grow_tree(test)
	
	fits  <- llply(train, FUN=grow_tree)
	preds <- llply(fits, FUN=test_fit, test_data=test)
	
	lapply(preds, FUN=print_summary)
	indi <- test_fit(fit, test)
	print("")
	print("Individual summary")
	summary(indi)
	print("Names Indi")
	names(indi)
	
	
	par(mfrow=c(1,2), xpd=NA)
	plot(fit, uniform=TRUE, main="Classification Tree for Mushroom Edibility")
	text(fit, use.n=TRUE, all=TRUE, cex=.8)
	print(fit$variable.importance)
	
	print('\nPress Enter to Prune the Tree')
	readline()
	post(fit, file=paste0("results_temp/fit_", i), title="Classification Tree for Mushroom Edibility")

	pfit <- prune(fit, cp=fit$cptable[which.min(fit$cptable[,"xerror"]), "CP"])

	plot(pfit, uniform=TRUE, main="Pruned Classification Tree for Mushroom Edibility")
	text(pfit, use.n=TRUE, all=TRUE, cex=.8)
	post(pfit, file=paste0("results_temp/pfit_", i))

	print("\nPress Enter to run next fold")
	readline()
}
