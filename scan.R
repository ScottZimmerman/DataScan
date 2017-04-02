library("rjson")

#Given a CSV file as input
# determines variable types
# calculates marginal distributions
# and calculates the covariance matrix 
# calculates missingness for each variable
# and the covariance of a "missingness" matrix
# all variable and category names are removed

#Upload the output json file into the simulation website
# 

args <- commandArgs(trailingOnly=TRUE)

if(length(args) != 1){
	stop("USAGE: Rscript scan.R <dataFileName>")
}
dataFileName <- args[1]

#If running in Rstudio: fill in and uncomment the following line and run everything below this line
#dataFileName <- "FILL_ME_IN.csv"

categoricalCutoff <- 30
nQuantiles <- 100

data <- read.csv(dataFileName)
names(data) <- paste0("X",1:ncol(data))

determineType <- function(v){
	result <- 0
	nCats <- length(unique(v[!is.na(v)]))

	if(is.factor(v)){
		result <- "categorical"

	}else if(nCats > categoricalCutoff){
		result <- "continuous"

	}else if(nCats == 2){
		result <- "binary"

	}else{
		result <- "ordinal"
	}
	return(result)
}

determineTypes <- function(d){
	result <- list()
	for(v in names(d)){
		result[[v]] <- determineType(d[[v]])
	}
	return(result)
}

calculateCovMx <- function(d,typeData){
	result <- NA
	temp <- d
	
	for(v in names(d)){
		if(typeData[[v]] == "categorical"){
			temp[[v]] <- as.numeric(temp[[v]])
		}
		temp[[v]] <- scale(temp[[v]])			
	}
	
	if(sum(is.na(d))==0){
		result <- cov(temp)
	}else{
		result <- diag(ncol(d))
		for(i in 1:ncol(d)){
			for(j in 1:ncol(d)){
				if(i != j){
					temp_i <- temp[[names(temp)[[i]]]]
					temp_j <- temp[[names(temp)[[j]]]]
					temp_i_na <- is.na(temp_i)
					temp_j_na <- is.na(temp_j)
					temp_ij_na <- temp_i_na + temp_j_na

					temp_i <- temp_i[!temp_ij_na]
					temp_j <- temp_j[!temp_ij_na]
					
					result[i,j] <- result[j,i] <- cov(temp_i,temp_j)
					
				}
			}
		}
	}
	
	return(result)
}

calculateMarginals <- function(d,typeData){
	result <- list()
	for(v in names(d)){
		if(typeData[[v]] == "continuous"){
			noNAs <- d[[v]][!is.na(d[[v]])]
			result[[v]] <- quantile(noNAs,seq(0,1,1/nQuantiles))
		}else{
			result[[v]] <- table(as.numeric(d[[v]]))/nrow(data)
		}
	}
	return(result)
}

calculateMissingnessMarginals <- function(d){
	result <- list()
	for(v in names(d)){
		result[[v]] <- sum(is.na(d[[v]]))/nrow(d)
	}
	return(result)	
}

calculateMissingnessCov <- function(d){
	temp <- scale(is.na(d))
	return(cov(temp))
}

results <- list()
results$types <- determineTypes(data)
results$cov <- calculateCovMx(data,results$types)
results$marginals <- calculateMarginals(data,results$types)
results$missingnessMarginals <- calculateMissingnessMarginals(data)
results$missingnessCov <- calculateMissingnessCov(data)

results$n <- nrow(data)

prefix <- strsplit(dataFileName,"[.]")[[1]][[1]]
write(toJSON(results),paste0("output_",prefix,".json"))