#'
#' Upload a saved diff as an anonymous gist
#'
#' @param html_path A character string specifying the path to the saved diff html from "diff_report"
#'
#' @export
#'
#' @details This function uses the github gist API to upload the saved diff
#' HTML file from "diff_report". The URL to the uploaded file is provided.
#'



submit_diff <- function(html_path){

	timestamp <- format(Sys.time(), "%a_%b_%d_%Y_%H_%M_%S")
	fullname <- paste0(analysis, "_diff_", timestamp, ".html")
	url <- "https://api.github.com/gists"
	file_content <- paste(readLines(html_path), collapse="\n")
	files_list <- list()
	files_list[[fullname]] <- list("content"=file_content)

	jlist <- list("description" = paste(analysis, "diff generated", timestamp), "public" = "true", "files"=files_list)
	resp <- POST(url, body=toJSON(jlist))
	web_url <- content(resp, "parsed")$html_url
	print(paste0("Your file has been rendered at: ", web_url)) 

}