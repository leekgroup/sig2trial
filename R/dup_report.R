#'
#' Duplicate a report template into the working directory
#'
#' @param analysis A character string specifing the type of analysis desired. Current options are: "tsp".
#' @param filename A character string specifying the name of the new file. Default: "tmp_template.Rmd"
#'
#' @export
#'
#' @details This function duplicates an existing report template into the working directory, should
#' the user decide to make edits to any portion of the standard template. We encourage the user to 
#' use the "diff_report" command if they make changes to the original report.

dup_report <- function(analysis, filename="tmp_template.Rmd"){

	if(anaysis !%in% c("tsp")){
		stop("Invalid analysis class specified")
	}

	analysis_path <- system.file(paste0(analysis, "_template.Rmd"), package="sig2trial")

	if(.Platform$OS.type == "windows"){
		system(paste0("copy ", analysis_path, " ", filename))
	} else {
		system(paste0("cp ", analysis_path, " ", filename))	
	}
}