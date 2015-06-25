library(rpart)

for( i in 1:length(folds))
{
   fit <- rpart(edibility~., data=folds[[i]], method="class")

   printcp(fit)
	 plotcp(fit)


	 plot(fit, uniform=TRUE, main="Classification Tree for Mushroom Edibility")
   text(fit, use.n=TRUE, all=TRUE, cex=.8)
	 readline()
	 post(fit, file=paste0("results_temp/fit_", i), title="Classification Tree for Mushroom Edibility")

	 pfit <- prune(fit, cp=fit$cptable[which.min(fit$cptable[,"xerror"]), "CP"])

   plot(pfit, uniform=TRUE, main="Pruned Classification Tree for Mushroom Edibility")
   text(pfit, use.n=TRUE, all=TRUE, cex=.8)
	 post(pfit, file=paste0("results_temp/pfit_", i))
	 readline()
}
