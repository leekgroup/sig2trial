#'
#' Duplicate a report template into the working directory
#'
#' @param analysis A character string specifing the type of analysis desired. Current options are: "tsp".
#'
#' @export
#'
#' @details This function duplicates an existing report template should the 
#' user decide to make edits to any portion of the standard template. The function
#' will pop-up a file chooser dialog box so that the user may select a directory
#' in which to create the duplicated file. We encourage the user to use the 
#' "diff_report" command if they make changes to the original report.
#' 
#' @return The file path to the duplicated file, should the user later desire to
#' supply it to "diff_report".

duplicate_report <- function(analysis){

	if(!(analysis %in% c("tsp"))){
		stop("Invalid analysis class specified")
	}

	analysis_path <- normalizePath(system.file(paste0(analysis, "_template.Rmd"), package="sig2trial"))
	newfile_path <- file.choose()

	if(.Platform$OS.type == "windows"){
		shell(paste0("copy ", analysis_path, " ", newfile_path))
	} else {
		system(paste0("cp ", analysis_path, " ", newfile_path))	
	}
	
	newfile_path
}