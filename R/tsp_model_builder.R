#'
#' Build and cross-validate a TSP-based model
#'
#' This function takes training/test data and pairs generated via empirical control feature
#' selection and builds a decision tree model. It also cross-validates to get an out-of-sample
#' accuracy estimate
#'
#' @param train p x n training data matrix
#' @param train_outcome Outcome data of length n
#' @param train_covar n x q additional covariates for training data (optional)
#' @param pairs r x n matrix of TSP generated via empirical controls
#' @param test p x m test data matrix, where p columns and column names match up with train
#' @param test_covar m x s additional covariates for training data (necessary if train_covar specified; column names must match)
#' @param npair Number of pairs desired in the model
#' @param predtype Type of predictions to make - "class" if initial outcome is factor, "vector" if initial outcome is non-factor
#'
#' @export 
#'
#' @details This is a wrapper for a series of model-building steps. The main output of
#' this function is the TSP decision tree model. We incorporate a second
#' feature selection step (after empirical controls, done separately) that chooses
#' from the candidate pairs. Pairs are chosen based on how much additional predictive
#' value they provide on top of pairs already selected (and non-pair covariates, if specified).
#' We also cross-validate this entire procedure five times to get an estimate of
#' out-of-sample accuracy of our model.
#'
#' @return A list contaiing the following attributes:
#'	\item{tree}{The final decision tree built on training data}
#'	\item{p_train}{Model predictions on training data}
#'	\item{p_test}{Model predictions on test data}
#'	\item{final_names}{Pair names in final model}
#'	\item{pair_names}{Pair name aliases for pretty tree printing}
#'	\item{acc}{Out-of-sample accuracy calculated via cross-validation}
#'



tsp_model_builder <- function(train, train_outcome, train_covar, pairs, test, test_covar, npair, predtype){

	ncv <- 5 # no. cross validation folds
	idxs <- split(sample(1:ncol(train)), rep(1:ncv, each=ncol(train)/ncv))
	acc <- vector("numeric", ncv)

	# we're going to get an out-of-sample accuracy measure via CV
	for(i in 1:ncv){
      	  idx <- idxs[[i]]
	        ktrain <- pairs[,-idx]
      	  ktest <- pairs[,idx]
	        ktrain_outcome <- train_outcome[-idx]
      	  ktest_outcome <- train_outcome[idx]
	        if(!is.null(train_covar)){
      	          ktrain_covar <- train_covar[-idx,]
            	    ktest_covar <- train_covar[idx,]
	        } else {
      	          ktrain_covar <- ktest_covar <- train_covar # All null
	        }

	        # Now that we have a subset, need to again check which pairs do not flip
	        rmp <- which(rowMeans(ktrain) == 1 | rowMeans(ktrain) == 0)
	        if(length(rmp) > 0){
      	          ktrain <- ktrain[-rmp,]
            	    ktest <- ktest[-rmp,]
	        }

	        # Do regression feature selection on ktrain
	        cp <- reg_fs(ktrain, ktrain_outcome, ktrain_covar, npair)

	        tmp_train_data <- as.data.frame(cbind(ktrain_covar, t(ktrain[cp,])))
	        tree <- rpart(ktrain_outcome~., data = tmp_train_data)
	        tree <- prune(tree, cp=tree$cptable[which.min(tree$cptable[,"xerror"]),"CP"])
		    tmp_test_data <- as.data.frame(cbind(ktest_covar,t(ktest[cp,])))
	        preds <- predict(tree, newdata=tmp_test_data, type=predtype)

      	    if(predtype == "class"){
				acc[i] <- sum(preds == ktest_outcome)/length(ktest_outcome)
			else {
				acc[i] <- mean((ktest_outcome - preds)^2)
			}
	}

	# Now we build the overall model on the whole data
	cp_final <- reg_fs(pairs, train_outcome, train_covar, npair)
	pairtmp <- as.data.frame(cbind(train_covar, t(pairs[cp_final,])))
	final_names <- c(colnames(train_covar), rownames(pairs[cp_final,]))
	pair_names <- c(colnames(train_covar), paste0("p", 1:npair))

	tree <- rpart(train_outcome~., data=pairtmp)
	tree <- prune(tree, cp=tree$cptable[which.min(tree$cptable[,"xerror"]),"CP"])

	colnames(pairtmp) <- pair_names

	display_tree <- rpart(train_outcome~., data=pairtmp)
	display_tree <- prune(display_tree, cp=tree$cptable[which.min(tree$cptable[,"xerror"]),"CP"])

	p_train <- predict(tree, type=predtype)

	test_dm <- as.data.frame(cbind(test_covar, sapply(final_names, single_pairs, test)))
	#colnames(test_dm) <- pair_names

	p_test <- predict(tree, newdata=test_dm, type=predtype)

	list("tree"=tree, "display_tree"=display_tree, "p_train"=p_train, "p_test"=p_test, "final_names"=final_names, "pair_names"=pair_names, "acc"
=acc)

}