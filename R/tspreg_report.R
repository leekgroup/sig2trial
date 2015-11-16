#'
#' Generate a knitr report for model-building using tspreg
#'
#' @param path Path to an alternative report (via dup_report + edits). Default NULL runs the standard report.
#' @param train A matrix of continuous data with features in rows
#' and samples in columns
#' @param outcome A binary outcome vector with length = ncol(data)
#' @param covar (optional) A data frame of additional covariates to adjust for with samples in rows and covariates in columns
#' @param val (optional) A validation dataset, where feature rownames are contained in the feature rownames of the training data
#' @param val_outcome (optional) Outcome vector for validation data. Required if val is provided.
#' @param val_covar (optional) A data frame matrix of additional covariates. Required if covar and val are provided.
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
#' If the user as created their own version of the standard report with custom
#' edits, they may supply the full path to this file in the "path" variable. The
#' function will use the alternative markdown file to build the report instead.
#'
#' @return The final decision tree model fit in the procedure.

tspreg_report <- function(path=NULL, train, outcome, covar=NULL, val=NULL, val_outcome=NULL, val_covar=NULL, filename=NULL, title="Example", seed=47209){

	# A variety of input checks
	if(!is.matrix(train) | !is.vector(outcome)){
		stop("Please ensure that your training data is in matrix form and your outcome is a vector")
	}

	if(ncol(train) != length(outcome)){
		stop("Number of subjects in training matrix and outcome vector do not match")
	}

	if(!is.null(covar)){
		if(!is.data.frame(covar)){
			stop("Covariate set must be a data frame")
		} else if(nrow(covar) != ncol(train)){
			stop("Number of subjects in covariate set and training matrix do not match")
		}
	}

	if(!is.null(val)){
		if(!is.matrix(val) | !is.vector(val_outcome)){
					stop("Please ensure that your validation data is in matrix form and your validation outcome is a vector")
		}

		if(ncol(val) != length(val_outcome)){
			stop("Number of subjects in validation matrix and validation outcome vector do not match")
		}

		if(!is.null(val_covar)){
			if(!is.data.frame(val_covar)){
				stop("Validation covariate set must be a data frame")
			} else if(nrow(val_covar) != ncol(val)){
				stop("Number of subjects in validation covariate set and validation matrix do not match")
			}
		}
	}
	
	if(is.null(path)){
		path <- system.file(paste0(analysis, "_template.Rmd"), package="sig2trial")
	}

	data <- list("train" = train, "outcome" = outcome, "covar" = covar, "val" = val, "val_outcome" = val_ouctome, "val_covar" = val_covar)	
	# Pass everything to a generic report-running function
	return_elements <- build_report(path, data=data, filename=filename, title=title, seed=seed)	

	return_elements
}