#'
#' Generate a knitr report for a specified analysis
#'
#' @param analysis A character string specifing the type of analysis desired. Current options are: "tsp".
#' @param path Character vector specifying the path of a duplicated analysis template, should the user choose to use this instead
#' @param data A list object containing all relevant data elements. Specific attributes are defined in the details.
#' @param filename A character string specifying the name of the file. Default NULL, which generates a file called "[curtime]_output.html" in the working directory.
#' @param title A character string setting the report title. Default "Example".
#' @param seed A random seed set at the beginning of the report. Default 47209
#'
#' @export
#'
#' @details This is a generic function that passes a data object to an analysis template
#' of the user's choosing. The template checks the data object to ensure compatibility.
#' 
#'
#' @return An object, as specified in the description of the chosen analysis

build_report <- function(path, data, filename=NULL, title="Example", seed=47209){
	
	if(is.null(filename)){
		filename <- paste0(getwd(), "/", format(Sys.time(), "%y_%m_%d_%H%M%S"), "_output.html")
	} else {
		filename<- paste0(getwd(), "/", filename, ".html")
	}
	
	render(path, output_format="knitrBootstrap::bootstrap_document",output_file=filename)

	return_elements
}