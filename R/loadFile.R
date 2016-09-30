#' @include utils.R
#' @include basic.R
#' @include metrics.R
NULL
#' Reads raw data from a UbiTrail result file.
#'
#' A general function to read raw tracking data and meta information from a .csv result file outputted by UbiTrail.
#' 
#' @param FILE a CSV result file outputted by UbiTrail.
#' @param verbose logical; if TRUE, the function will print messages at every step.
#' @note The returned list contains a numerical matrix for each area.
#' The attributes of list contain metadata and additional information is present in each of the attributes of each matrix.
#' @return A list of numerical matrices with each matrix corresponding to an area.
#' @examples
#' ## Locate raw data example including with package
#' FILE <- system.file("extdata", "tenebrio_ubitrail.csv.gz", package = "rubitrail")
#'
#' tenebrio_raw <- rubitLoadFile(FILE)
#'
#' ###See general metadata:
#' attributes(tenebrio_raw)
#'
#' ###See how many reads in each area:
#' summary(tenebrio_raw)
#'
#' ###See informations about the area named '08':
#' attributes(tenebrio_raw[['08']])
#'
#' @seealso \code{\link{rubitBasic}} and \code{\link{rubitMetrics}} for more further processing and analysis of tracking data.
#' @export
rubitLoadFile <- function(FILE, verbose=FALSE){
	if(verbose) print("Reading data...")
	meta <- rubitMetaData(FILE)
	data <- rubitData(FILE)
	if(verbose) print("Formatting data...")
	l <- rubitParseDataToList(data, meta)
	if(verbose) print("Converting data to absolute data...")
	
	#To preserve the attributes of l:
	atrs <- attributes(l);
	l <- lapply(l, h_rubitRelativeToAbsolute)
	attributes(l) <- atrs
	
	l
}