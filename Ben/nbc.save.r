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

