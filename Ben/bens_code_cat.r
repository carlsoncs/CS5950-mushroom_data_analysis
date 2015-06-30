### init.R

library(MASS)
mush0 = read.csv("mydata.csv")
mush = mush0[,-17]


### util.R

cmp = function(data, cIndex) {
	print(paste("colname:", names(data)[cIndex], " [edible,poisonous]"))
	print(summary(data[which(data$class=="e"),cIndex]))
	print(summary(data[which(data$class=="p"),cIndex]))
}


### myclass.R

#myclass = rep("?", nrow(mush))

#for (i in 1:nrow(mush))
#{
#	if (mush[i,]$odor %in% c("a","l") | mush[i,]$spore.print.color %in% c("b","o","u","y"))
#	{
#		myclass[i] = "e"
#	}
#	else if (mush[i,]$odor %in% c("c","f","m","p","s","y") | mush[i,]$spore.print.color %in% c("r"))
#	{
#		myclass[i] = "p"
#	}
#}

myclass = ifelse(mush$odor %in% c("a","l") | mush$spore.print.color %in% c("b","o","u","y"), "e",
	ifelse(mush$odor %in% c("c","f","m","p","s","y") | mush$spore.print.color %in% c("r"), "p",
		ifelse(mush$spore.print.color != "w", "e",
			ifelse(mush$gill.size=="b", "e",
				ifelse(mush$gill.spacing=="c" | mush$stalk.surface.above.ring=="k", "p",
					ifelse(mush$population=="c", "p",
"?"
					)
				)
			)
		)
	)
)
myclass = factor(myclass)
foo = mush[which(myclass=="?"),]

print("---------------------------------------------------------")
for (i in (1:ncol(mush))[-c(1,17)])
{
	cmp(foo,i)
}

print(table(myclass, mush$class))

### br.R

start = Sys.time()
print(start)

# Constants
nCol = ncol(mush)
index = 1:nrow(mush)
k = 5

# Feature value summary table setup. 
setsize = 1:nCol
for (i in 1:nCol) 
{ 
	setsize[i] = length(unique(mush[,i])) 
}
maxFcnt = max(setsize)

# Results structures. 
preds = data.frame(mush$class,mush$class,mush$class,mush$class,mush$class,rep(0,nrow(mush)))
names(preds) = c("real", "nbc", "lda", "qda", "log.reg", "fold")

# Set of test features. (not including class)
testFeatureSet = 2:nCol

for (fold in 0:(k-1))
{
	# Partition Data for NBC
	test = mush[which(index %% k == fold),]
	train = mush[which(index %% k != fold),]
	traine = train[which(train$class == "e"),]
	trainp = train[which(train$class == "p"),]
	
	# Initialize results structures. 
	test.preds = preds[which(index %% k == fold),]
	test.preds$fold = fold
	test.preds$real = test$class
	
	pe = log(nrow(traine)) # P(edible)
	pp = log(nrow(trainp)) # P(poisonous)

	# Create concise matrix of log(P(feature value|class)) estimates
	pfe = matrix(nrow=nCol, ncol=maxFcnt)
	pfp = matrix(nrow=nCol, ncol=maxFcnt)
	eFeatValLogit = matrix(nrow=nCol, ncol=maxFcnt)
	for (c in 1:nCol)
	{
		l = levels(mush[1,c])
		for (v in 1:length(l))
		{
			valueTotal = max(log(sum(train[,c] == l[v])), 0) * 100
			pfe[c,v] = max(log(sum(traine[,c] == l[v])) , -valueTotal)
			pfp[c,v] = max(log(sum(trainp[,c] == l[v])) , -valueTotal)
		}
	}
	# Logit(ish) Estimate for numerical methods
	eFeatValLogit = pfe - pfp # Matrix subtraction
	
	# Create probabalistic data table. 
	mush.eprob = data.frame(mush[,1],rep(list(rep(-1,nrow(mush))),nCol))
	names(mush.eprob) = names(mush)
	for (c in 2:nCol)
	{
		# Fancy vector method for setting a column of values at a time. 
		mush.eprob[,c] = eFeatValLogit[(as.integer(mush[,c])-1) * nCol + c]
	}
	test.eprob = mush.eprob[which(index %% k == fold),]
	train.eprob = mush.eprob[which(index %% k != fold),]
	
	# NBC
	res = rep("?", nrow(test))
	for (ti in 1:nrow(test))
	{
		pi.e = pe + sum(pfe[(as.integer(test[ti,testFeatureSet])-1)*nCol + testFeatureSet])
		pi.p = pp + sum(pfp[(as.integer(test[ti,testFeatureSet])-1)*nCol + testFeatureSet])
		
		res[ti] = ifelse(pi.e > pi.p, "e", "p")
	}
	test.preds$nbc = factor(res, levels=levels(mush[1,1]))
	
	f = as.formula(paste("class~",paste(names(mush)[testFeatureSet], collapse="+")))
	# LDA
	lda.fit = lda(f, data=train.eprob)
	test.preds$lda = predict(lda.fit, test.eprob)$class

	# QDA
	qda.fit = qda(f, data=train.eprob)
	test.preds$qda = predict(qda.fit, test.eprob)$class
	
	# Logistic Regression
	glm.fit = glm(f, family=binomial, data=train.eprob)
	glm.prob = predict(glm.fit, test.eprob, type="response")
	test.preds$log.reg = factor(ifelse(glm.prob < .5, "e", "p"), levels=levels(mush[1,1]))
	
	preds[which(index %% k == fold),] = test.preds
	
}
print(table(preds$nbc, preds$real))
print(table(preds$lda, preds$real))
print(table(preds$qda, preds$real))
print(table(preds$log.reg, preds$real))
print(Sys.time() - start)


### nbc.R

mylog = log

index = 1:nrow(mush)
k = 5
caution = .5
cautionOff = 0
cautionRange = .01
setsize = 1:ncol(mush)
for (i in 1:ncol(mush)) 
{ 
	setsize[i] = length(unique(mush[,i])) 
}
maxFcnt = max(setsize)

pred = factor(c("e", "p"))
real = factor(c("e", "p"))
confuse = table(pred, real)
confuse[,] = 0

coef = matrix(nrow=nrow(mush), ncol=2)
pprob = rep(0, nrow(mush))
eprob = pprob
realClass = rep('?', nrow(mush))
coefi = 0

for (fold in 0:(k-1))
{
	test = mush[which(index %% k == fold),]
	train = mush[which(index %% k != fold),]
	traine = train[which(train$class == "e"),]
	trainp = train[which(train$class == "p"),]

	pe = mylog(nrow(traine))# * 2
	pp = mylog(nrow(trainp))# * 2

	pfe = matrix(nrow=ncol(mush), ncol=maxFcnt)
	pfp = pfe
	for (c in 1:ncol(mush))
	{
		l = levels(mush[1,c])
		for (v in 1:length(l))
		{
			emptyWeight = 0#-max(mylog(sum(train[,c] == l[v])), 0)
			pfe[c,v] = max(mylog(sum(traine[,c] == l[v])), emptyWeight)
			pfp[c,v] = max(mylog(sum(trainp[,c] == l[v])), emptyWeight)
		}
	}
	
	res = rep("?", nrow(test))
	for (ti in 1:nrow(test))
	{
		pi.e = pe 
		pi.p = pp 
		#print(paste(pi.e, pi.p))
		#for (fi in 2:ncol(mush))
		for (fi in c(6,21))
		{
			pi.e = pi.e + pfe[fi,as.integer(test[ti,fi])]
			pi.p = pi.p + pfp[fi,as.integer(test[ti,fi])]
		}
		
		res[ti] = #ifelse(
			#abs(pi.e - pi.p) < cautionRange
			#abs(.5 - pi.e / (pi.e + pi.p)) < cautionRange
			#, "?",
			ifelse(pi.e * (1-caution) > pi.p * caution, "e", "p")
		#)
		
		#coef[coefi,] = c(pi.e / (pi.e + pi.p), ifelse(res[ti] == test[ti,1], 1, 0))
		pprob[coefi] = pi.e
		eprob[coefi] = pi.p
		realClass[coefi] = test[ti,1]
		coefi = coefi + 1
	}
	resf = factor(res)#, levels=c(1,2,3), labels=c("e","p","?"))
	
	curTab = table(res, test$class)
	print(curTab)
	confuse = confuse + curTab
}
print(confuse)


