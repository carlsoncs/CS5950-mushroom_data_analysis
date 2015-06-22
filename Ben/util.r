mush = read.csv("agaricus-lepiota.data");

cmp = function(data, cIndex) {
	print(paste("colname:", names(data)[cIndex], " [edible,poisonous]"))
	print(summary(data[which(data$class=="e"),cIndex]))
	print(summary(data[which(data$class=="p"),cIndex]))
}


