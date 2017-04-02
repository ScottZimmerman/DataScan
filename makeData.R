n <- 1000

V <- as.factor(c(rep("A",0.3*n),rep("B",0.5*n),rep("C",0.2*n)))

W <- rnorm(n,5,3)

X <- rbinom(n,1,0.5)*(W < 3)

Y <- (X==1)*runif(n)+(W*0.2)*(X==0)

Z <- 0.2*X + 0.1*W + 0.5*W*X*(V=="A") + 0.4*X*W*Y

df <- data.frame(V,W,X,Y,Z)
write.csv(df,"data.csv",row.names=FALSE)

#Give each variable a random missingness between 0 and 10%
propsMissing <- 0.01 + 0.09*runif(ncol(df))
i <- 1
for(v in names(df)){
	naVec <- rbinom(n,1,propsMissing[[i]])
	df[which(naVec==1),v] <- NA
	i <- i + 1
	print(naVec)
	print(df)
}


write.csv(df,"data_missing_at_random.csv",row.names=FALSE)

#Give a high missingness to higher values of Z
zNonRandom <- rbinom(n,1,0.4)*(df[["Z"]] > 1.5)
zAssign <- (zNonRandom==1)
df[which(zAssign==1),"Z"] <- NA

write.csv(df,"data_missing_not_at_random.csv",row.names=FALSE)