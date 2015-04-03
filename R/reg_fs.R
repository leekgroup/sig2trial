#' 
#' Regression-based feature selection
#' 
#' Leverages the fact that the F-statistic for y~x and
#' x~y is the same to quickly choose pairs that add conditional
#' value for predicting an outcome. Also runs a separate pair-on-pair
#' regression to eliminate collinear pairs. 
#' 
#' @param pairmat An m x n matrix of pairwise features with rows=features, columns=samples
#' @param outcome A vector of outcomes of length n
#' @param covar An optional n x p matrix of additional covariates to adjust for
#' @param npair The number of paris we wish to select
#' 
#' @export
#' 
#' @return pairs A vector with the index for each chosen pair
#' 

reg_fs <- function(pairmat, outcome, covar=NULL, npair=5){
	# Tolerance for testing residuals
	tol <- 1e-10
	# Covariates are always accounted for
	if(!is.null(covar)){
		if(nrow(covar) != ncol(pairmat)){
			stop("covar and pairmat must have agreeing dimensions.")
		}
		outmat <- cbind(outcome, covar)
	} else {
		outmat <- cbind(outcome)
	}

	if(ncol(pairmat) != length(outcome)){
		stop("pairmat and outcome must have agreeing dimensions.")
	}
	
	tpm <- pairmat # temp copy
	pairs <- vector("integer", npair)
	# Initial step is for best overall pair
	mod0 <- cbind(rep(1, length(outcome)))
	mod <- cbind(mod0, outmat)
	idx <- 1:nrow(tpm)
	
	for(i in 1:npair){
		# Get F-statistic for every remaining pair
		fs <- fstats(tpm, mod, mod0)
		cur <- which.max(fs)
		mod0 <- cbind(mod0, tpm[cur,])
		mod <- cbind(mod, tpm[cur,])
		pairs[i] <- idx[cur]
		# Get residuals for regressing all remaining pairs on all chosen features
		res <- apply(tpm, 1, function(y){sum((lm.fit(mod0, y)$residuals)^2)})
		# Need to dump this and all identical rows
		#all_cur <- which(apply(tpm, 1, function(x){all(x == tpm[cur,])}))
		all_cur <- which(res < tol)
		tpm <- tpm[-all_cur,]
		idx <- idx[-all_cur]		
	}

	pairs	
}
