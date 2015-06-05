#*****************************************************************
# Appendix B. CLA code (in R) by Ilya Kipnis (QuantStratTradeR10) SSRN-id2606884.pdf 
#*****************************************************************
require(quantmod)
require(PerformanceAnalytics)
require(TTR)

CCLA <- function(covMat, retForecast, maxIter = 1000,
	verbose = FALSE, scale = 252,
	weightLimit = .7, volThresh = .1) 
{
	if(length(retForecast) > length(unique(retForecast))) {
		sequentialNoise <- seq(1:length(retForecast)) * 1e-12
		retForecast <- retForecast + sequentialNoise
	}
	
	#initialize original out/in/up status
	if(length(weightLimit) == 1) {
		weightLimit <- rep(weightLimit, ncol(covMat))
	}
	
	# sort return forecasts
	rankForecast <- length(retForecast) - rank(retForecast) + 1
	remainingWeight <- 1 #have 100% of weight to allocate
	upStatus <- inStatus <- rep(0, ncol(covMat))
	i <- 1
	
	# find max return portfolio
	while(remainingWeight > 0) {
		securityLimit <- weightLimit[rankForecast == i]
		if(securityLimit < remainingWeight) {
			upStatus[rankForecast == i] <- 1 #if we can't invest all remaining weight into the security
			remainingWeight <- remainingWeight - securityLimit
		} else {
			inStatus[rankForecast == i] <- 1
			remainingWeight <- 0
		}
		i <- i + 1
	}
	
	#initial matrices (W, H, K, identity, negative identity)
	covMat <- as.matrix(covMat)
	retForecast <- as.numeric(retForecast)
	init_W <- cbind(2*covMat, rep(-1, ncol(covMat)))
	init_W <- rbind(init_W, c(rep(1, ncol(covMat)), 0))
	H_vec <- c(rep(0, ncol(covMat)), 1)
	K_vec <- c(retForecast, 0)
	negIdentity <- -1*diag(ncol(init_W))
	identity <- diag(ncol(init_W))
	matrixDim <- nrow(init_W)
	weightLimMat <- matrix(rep(weightLimit, matrixDim), ncol=ncol(covMat), byrow=TRUE)
	#out status is simply what isn't in or up
	outStatus <- 1 - inStatus - upStatus

	#initialize expected volatility/count/turning points data structure
	expVol <- Inf
	lambda <- 100
	count <- 0
	turningPoints <- list()
	
	while(lambda > 0 & count < maxIter) {
		#old lambda and old expected volatility for use with numerical algorithms
		oldLambda <- lambda
		oldVol <- expVol
		count <- count + 1
		
		#compute W, A, B
		inMat <- matrix(rep(c(inStatus, 1), matrixDim), nrow = matrixDim, byrow = TRUE)
		upMat <- matrix(rep(c(upStatus, 0), matrixDim), nrow = matrixDim, byrow = TRUE)
		outMat <- matrix(rep(c(outStatus, 0), matrixDim), nrow = matrixDim, byrow = TRUE)
		W <- inMat * init_W + upMat * identity + outMat * negIdentity
		
		inv_W <- solve(W)
		modified_H <- H_vec - rowSums(weightLimMat* upMat[,-matrixDim] * init_W[,-matrixDim])
		A_vec <- inv_W %*% modified_H
		B_vec <- inv_W %*% K_vec
		
		#remove the last elements from A and B vectors
		truncA <- A_vec[-length(A_vec)]
		truncB <- B_vec[-length(B_vec)]
		
		#compute in Ratio (aka Ratio(1) in Kwan.xls)	
		inRatio <- rep(0, ncol(covMat))
		inRatio[truncB > 0] <- -truncA[truncB > 0]/truncB[truncB > 0]

		#compute up Ratio (aka Ratio(2) in Kwan.xls)
		upRatio <- rep(0, ncol(covMat))
		upRatioIndices <- which(inStatus==TRUE & truncB < 0)
		
		if(length(upRatioIndices) > 0) {
			upRatio[upRatioIndices] <- (weightLimit[upRatioIndices] - truncA[upRatioIndices]) / truncB[upRatioIndices]
		}
	
		#find lambda -- max of up and in ratios
		maxInRatio <- max(inRatio)
		maxUpRatio <- max(upRatio)
		lambda <- max(maxInRatio, maxUpRatio)

		#compute new weights
		wts <- inStatus*(truncA + truncB * lambda) + upStatus * weightLimit + outStatus * 0
		
		#compute expected return and new expected volatility
		expRet <- t(retForecast) %*% wts
		expVol <- sqrt(wts %*% covMat %*% wts) * sqrt(scale)
				
		#create turning point data row and append it to turning points
		turningPoint <- cbind(count, expRet, lambda, expVol, t(wts))
		colnames(turningPoint) <- c("CP", "Exp. Ret.", "Lambda", "Exp. Vol.", colnames(covMat))
		turningPoints[[count]] <- turningPoint
		
		#binary search for volatility threshold -- if the first iteration is lower than the threshold,
		#then immediately return, otherwise perform the binary search until convergence of lambda
		if(oldVol == Inf & expVol < volThresh) {
			turningPoints <- do.call(rbind, turningPoints)
			threshWts <- tail(turningPoints, 1)
			return(list(turningPoints, threshWts))
		} else if(oldVol > volThresh & expVol < volThresh) {
			upLambda <- oldLambda
			dnLambda <- lambda
			meanLambda <- (upLambda + dnLambda)/2
			
			while(upLambda - dnLambda > .00001) {
				#compute mean lambda and recompute weights, expected return, and expected vol
				meanLambda <- (upLambda + dnLambda)/2
				wts <- inStatus*(truncA + truncB * meanLambda) + upStatus * weightLimit + outStatus * 0
				expRet <- t(retForecast) %*% wts
				expVol <- sqrt(wts %*% covMat %*% wts) * sqrt(scale)
				
				#if new expected vol is less than threshold, mean becomes lower bound
				#otherwise, it becomes the upper bound, and loop repeats
				if(expVol < volThresh) {
					dnLambda <- meanLambda
				} else {
					upLambda <- meanLambda
				}
			}
			
			#once the binary search completes, return those weights, and the corner points
			#computed until the binary search. The corner points aren't used anywhere, but they're there.
			threshWts <- cbind(count, expRet, meanLambda, expVol, t(wts))
			colnames(turningPoint) <- colnames(threshWts) <- c("CP", "Exp. Ret.", "Lambda", "Exp. Vol.", colnames(covMat))
			turningPoints[[count]] <- turningPoint
			turningPoints <- do.call(rbind, turningPoints)
			return(list(turningPoints, threshWts))
		}
		
		#this is only run for the corner points during which binary search doesn't take place
		#change status of security that has new lambda
		if(maxInRatio > maxUpRatio) {
			inStatus[inRatio == maxInRatio] <- 1 - inStatus[inRatio == maxInRatio]
			upStatus[inRatio == maxInRatio] <- 0
		} else {
			upStatus[upRatio == maxUpRatio] <- 1 - upStatus[upRatio == maxUpRatio]
			inStatus[upRatio == maxUpRatio] <- 0
		}
		outStatus <- 1 - inStatus - upStatus
	}
	
		
	#we only get here if the volatility threshold isn't reached
	#can actually happen if set sufficiently low
	turningPoints <- do.call(rbind, turningPoints)
	threshWts <- tail(turningPoints, 1)
	return(list(turningPoints, threshWts))
}


sumIsNa <- function(column) {
	return(sum(is.na(column)))
}

returnForecast <- function(prices) {
	forecast <- (ROC(prices, n = 1, type="discrete") + ROC(prices, n = 3, type="discrete") +
	ROC(prices, n = 6, type="discrete") + ROC(prices, n = 12, type="discrete"))/22
	forecast <- as.numeric(tail(forecast, 1))
	return(forecast)
}


kellerCLAfun <- function(prices, returnWeights = FALSE,
	weightLimit, volThresh, uncappedAssets) 
{
	if(sum(colnames(prices) %in% uncappedAssets) == 0) {
		stop("No assets are uncapped.")
	}
	
	#initialize data structure to contain our weights
	weights <- list()
	#compute returns
	returns <- Return.calculate(prices)
	returns[1,] <- 0 #impute first month with zeroes
	ep <- endpoints(returns, on = "months")
	
	for(i in 2:(length(ep) - 12)) {
		priceSubset <- prices[ep[i]:ep[i+12]] #subset prices
		retSubset <- returns[ep[i]:ep[i+12]] #subset returns
		assetNAs <- apply(retSubset, 2, sumIsNa)
		zeroNAs <- which(assetNAs == 0)
		priceSubset <- priceSubset[, zeroNAs]
		retSubset <- retSubset[, zeroNAs]
		
		#remove perfectly correlated assets
		retCors <- cor(retSubset)
		diag(retCors) <- NA
		corMax <- round(apply(retCors, 2, max, na.rm = TRUE), 7)
		while(max(corMax) == 1) {
			ones <- which(corMax == 1)
			valid <- which(!names(corMax) %in% uncappedAssets)
			toRemove <- intersect(ones, valid)
			toRemove <- max(valid)
			retSubset <- retSubset[, -toRemove]
			priceSubset <- priceSubset[, -toRemove]
			retCors <- cor(retSubset)
			diag(retCors) <- NA
			corMax <- round(apply(retCors, 2, max, na.rm = TRUE), 7)
		}
		
		covMat <- cov(retSubset) #compute covariance matrix
		
		#Dr. Keller's return forecast
		retForecast <- returnForecast(priceSubset)
		uncappedIndex <- which(colnames(covMat) %in% uncappedAssets)
		weightLims <- rep(weightLimit, ncol(covMat))
		weightLims[uncappedIndex] <- 1
		
		cla <- CCLA(covMat = covMat, retForecast = retForecast, scale = 12,
			weightLimit = weightLims, volThresh = volThresh) #run CCLA algorithm
		CPs <- cla[[1]] #corner points
		wts <- cla[[2]] #binary search volatility targeting -- change this line and the next
		
		#if using max sharpe ratio golden search
		wts <- wts[, 5:ncol(wts)] #from 5th column to the end
		if(length(wts) == 1) {
			names(wts) <- colnames(covMat)
		}
		
		zeroes <- rep(0, ncol(prices) - length(wts))
		names(zeroes) <- colnames(prices)[!colnames(prices) %in% names(wts)]
		wts <- c(wts, zeroes)
		wts <- wts[colnames(prices)]
		
		#append to weights
		wts <- xts(t(wts), order.by=tail(index(retSubset), 1))
		weights[[i]] <- wts
	}
	
	weights <- do.call(rbind, weights)
	#compute strategy returns
	stratRets <- Return.portfolio(returns, weights = weights)
	if(returnWeights) {
		return(list(weights, stratRets))
	}
	return(stratRets)
}
