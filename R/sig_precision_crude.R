#'
#' Crude estimate of precision gain due to gene signature predictions
#'
#' This function takes training/test data and pairs generated via empirical control feature
#' selection and builds a decision tree model. It also cross-validates to get an out-of-sample
#' accuracy estimate
#'
#' @param test_outcome A vector of length n outcomes for the test/validation set. Currently only supports binary outcomes.
#' @param test_pred Prediction vector of length n created using the decision tree model built by tsp_model_builder.R
#' @param test_covar A n x p matrix of additional covariates to adjust for. (optional)
#'
#' @export
#'
#' @details This function approximates how much precision gain we might expect
#' in a clinical trial setting if we adjused for predictions with our gene signature.
#' We can optionally provide additional covariates and estimate how much additional
#' gain we might observe if we include predictions from our model.
#'
#' @return A crude approximation of the reduction in variance of a treatment effect estimator
#' for the outcome of interest.

sig_precision_crude <- function(test_outcome, test_pred, test_covar){
	# Case 1: No other covariates to adjust for
	if(is.null(test_covar)){
		mod1 <- glm(test_outcome ~ 1, family=binomial)
		mod2 <- glm(test_outcome ~ test_pred, family=binomial)
	} else {
		mod1 <- glm(test_outcome ~ test_covar, family=binomial)
		dm <- cbind(test_covar, test_pred)
		mod2 <- glm(test_outcome ~ dm)
	}

	1 - sum((as.numeric(test_outcome) - mod2$fitted.values)^2, na.rm = T)/sum((as.numeric(test_outcome) - mod1$fitted.values)^2, na.rm = T)
}