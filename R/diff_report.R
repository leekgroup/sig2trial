#'
#' Compare an edited report template to the standard report template
#'
#' @param analysis A character string specifing the desired analysis template. Current options are: "tsp".
#' @param filename A character string specifying the name of the new template file. Default: "tmp_template.Rmd"
#' @param outputname A character string specifying the name of a file to contain the file differences. Default: "tmp_diff.txt"
#'
#' @export
#'
#' @details This function does a simple file comparison of the original standard report template
#' and a user-edited template. The differences are logged in the output file and saved in the
#' working directory.

diff_report <- function(analysis, filename="tmp_template.Rmd", outputname="tmp_diff.txt"){

	if(anaysis !%in% c("tsp")){
		stop("Invalid analysis class specified")
	}

	analysis_path <- system.file(paste0(analysis, "_template.Rmd"), package="sig2trial")

	if(.Platform$OS.type == "windows"){
		system(paste0("FC ", analysis_path, " ", filename, " > ", outputname))
	} else {
		system(paste0("diff ", analysis_path, " ", filename, " > ", outputname))	
	}
}