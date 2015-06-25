mylog = log

index = 1:nrow(mush)
k = 10
caution = .5
setsize = 1:ncol(mush)
for (i in 1:ncol(mush)) 
{ 
	setsize[i] = length(unique(mush[,i])) 
}
maxFcnt = max(setsize)

pred = factor(c("e", "p"))
real = pred
confuse = table(pred, real)
confuse[,] = 0

coef = matrix(nrow=nrow(mush), ncol=2)
coefi = 0

for (fold in 0:(k-1))
{
	test = mush[which(index %% k == fold),]
	train = mush[which(index %% k != fold),]
	traine = train[which(train$class == "e"),]
	trainp = train[which(train$class == "p"),]

	pe = mylog(nrow(traine)) * 2
	pp = mylog(nrow(trainp)) * 2

	pfe = matrix(nrow=ncol(mush), ncol=maxFcnt)
	pfp = pfe
	for (c in 1:ncol(mush))
	{
		l = levels(mush[1,c])
		for (v in 1:length(l))
		{
			emptyWeight = -max(mylog(sum(train[,c] == l[v])), 0) * 100
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
		for (fi in 2:ncol(mush))
		{
			pi.e = pi.e + pfe[fi,as.integer(test[ti,fi])]
			pi.p = pi.p + pfp[fi,as.integer(test[ti,fi])]
		}
		res[ti] = ifelse(pi.e * (1-caution) > pi.p * caution, "e", "p")
		
		coef[coefi,] = c(pi.e / (pi.e + pi.p), ifelse(res[ti] == test[ti,1], 1, 0))
		coefi = coefi + 1
	}
	resf = factor(res)
	
	curTab = table(res, test$class)
	confuse = confuse + curTab
}
print(confuse)
