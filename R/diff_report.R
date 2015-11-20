#'
#' Compare an edited report template to the standard report template
#'
#' @param analysis A character string specifing the desired analysis template. Current options are: "tsp".
#' @param file_path A character string specifying the path to the edited template file.
#'
#' @export
#'
#' @details This function uses diffr and htmlwidgets to render a file difference
#' between an existing package template and a user-edited template. An HTML
#' file is rendered and saved which displays the highlighted differences in
#' the two files.
#' 
#' @return String with the file path for the html rendering of the diff
#'

diff_report <- function(analysis, file_path){

	if(!(analysis %in% c("tsp"))){
		stop("Invalid analysis class specified")
	}

	analysis_path <- normalizePath(system.file(paste0(analysis, "_template.Rmd"), package="sig2trial"))

	tmp_diff <- diffr(analysis_path, file_path, before=paste("Original", analysis, "template"), after=paste("Edited", analysis, "template"))
	html_path <- file.choose()
	saveWidget(tmp_diff, html_path)
	
	html_path
	
}