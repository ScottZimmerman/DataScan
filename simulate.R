library("rjson")
library("MASS")
library("psych")
library("gtools")

#Simulate data based on the abstract data representation created by scan.R
#Based on: https://www.r-bloggers.com/modelling-dependence-with-copulas-in-r/

#options
n <- 1000

#Read in data
json_file <- "output_data.json"
json_data <- fromJSON(paste(readLines(json_file), collapse=""))

covMx <- json_data$cov
d = sqrt(length(covMx))
covMx <- matrix(nrow=d, ncol=d, covMx)

norm_data <- mvrnorm(n,mu=rep(0,d),Sigma=covMx,empirical=T)
unif_data <- data.frame(pnorm(norm_data))
#pairs.panels(unif_data)

varNames = paste0("X",1:d)
final_data <- data.frame(matrix(nrow=n,ncol=d))

names(unif_data) <- varNames
names(final_data) <- varNames

make_CDF <- list(
	discrete = function(marginal){
		cdf <- data.frame(matrix(nrow=length(marginal),ncol=2))
		names(cdf) <- c("X", "p(x<=X)")
		cumul <- 0
		index <- 0

		for(element in names(marginal)){
			cumul <- cumul + marginal[[element]]
			index <- index + 1
			cdf[index, 1] <- element
			cdf[index, 2] <- cumul
		}
		
		return(cdf)
	},
	continuous = function(marginal){
		nPercentiles <- length(marginal)
		cdf <- data.frame(matrix(nrow=nPercentiles,ncol=2))
		names(cdf) <- c("X", "p(x<=X)")
		cumul <- 0
		index <- 0

		for(element in names(marginal)){
			cumul <- index/(nPercentiles-1)
			index <- index + 1
			cdf[index, 1] <- marginal[[element]]
			cdf[index, 2] <- cumul
		}
		return(cdf)	
	}
)

inverse_CDF <- function(var){
	varType <- json_data$types[[var]]
	marginal <- json_data$marginals[[var]]
	if(varType != "continuous"){
		varType <- "discrete"
	}
	cdf <- make_CDF[[varType]](marginal)
	print(cdf)
	binSearchRange <- c(1,nrow(cdf))
	varCol <- rep(NA,n)
	for(i in 1:n){
		cdf_index <- binsearch(
			function(x){
				return(cdf[x,2])
			},
			binSearchRange,
			target=unif_data[i,var]
		)

		if(cdf_index$flag == "Between Elements"){
			if(varType == "continuous"){
				varCol[[i]] <- (cdf[cdf_index$where[[1]],1] + cdf[cdf_index$where[[2]],1])/2
			}else{
				varCol[[i]] <- cdf[cdf_index$where[[2]],1]
			}
		}else{
			varCol[[i]] <- cdf[cdf_index$where,1]
		}
	}
	return(varCol)
}

#Apply the inverse marginals
for(var in varNames){
	final_data[,var] <- inverse_CDF(var)
}
names(unif_data) <- paste0("U_",names(unif_data))
print(cbind(unif_data,final_data))