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
