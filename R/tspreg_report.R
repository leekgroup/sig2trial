#'
#' Generate a knitr report for model-building using tspreg
#'
#' @param data A matrix of continuous data with features in rows
#' and samples in columns
#' @param outcome A binary outcome vector with length = ncol(data)
#' @param covar (optional) A design matrix of additional covariates to adjust for with samples in rows and covariates in columns
#' @param val (optional) A validation dataset, where feature rownames are contained in the feature rownames of the training data
#' @param val_outcome (optional) Outcome vector for validation data. Required if val is provided.
#' @param val_covar (optional) A design matrix of additional covariates. Required if covar and val are provided.
#' @param npair The number of pairs desired in the final model
#' @param filepath A character string specifying the name of the file. Default NULL, which generates a file called "[curtime]_output.html" in the working directory.
#' @param title A character string setting the rport title. Default "Example".
#' @param seed A random seed set at the beginning of the report. Default 47209
#'
#' @export
#'
#' @details This is a wrapper to the TSP model-building procedure. Minimal
#' input is a matrix of continous data from which to build features (i.e. a
#' gene expression matrix) and a binary outcome vector. This function will
#' supply the provided information to an R markdown template that runs
#' a standardized model-building procedure. The output will appear in the
#' working directory as an HTML report.
#'
#' @return The final decision tree model fit in the procedure.

tspreg_report <- function(data, outcome, covar=NULL, val=NULL, val_outcome=NULL, val_covar=NULL, npair=5, filename=NULL, title="Example", seed=47209){
	# Formal input checking to come
	if(is.null(filepath)){
		filepath <- paste0(getwd(), "/", format(Sys.time(), "%y_%m_%d_%H%M%S"), "_output.html")
	} else {
		filepath <- paste0(getwd(), "/", filename, ".html")
	}
	
	render(system.file("template.Rmd", package="sig2trial"), output_format="knitrBootstrap::bootstrap_document",output_file=filepath)

	model_out$tree
}
