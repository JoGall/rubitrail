#' @include utils.R
#' @include linearInterpolate.R
NULL
#' Apply a running median filter on an an area matrix.
#' 
#' This function is used to eliminate outliers from an area matrix.
#' @param m a numerical matrix corresponding to an area.
#' @param k an integer specifying the size of the smoothing window. \emph{It must be odd}.
#' @return A numerical matrix of the same dimension as m.
#' @examples
#' data(weevils)
#'
#' ### Apply different 'k' values to a list of area matrices:
#' w15 <- lapply(weevils, rubitMedianFilter, k = 15)
#' w101 <- lapply(weevils, rubitMedianFilter, k = 101)
#'
#' ### See impacts of smoothing parameter 'k' on a trajectory:
#' ## raw trajectory
#' plot(weevils[['08']][1:100,'X'] ~ weevils[['08']][1:100,'Y'], asp=1, type='l')
#' ## acceptable level of smoothing
#' lines(w15[['08']][1:100,'X'] ~ w15[['08']][1:100,'Y'],col='green')
#' ## oversmoothed
#' lines(w101[['08']][1:100,'X'] ~ w101[['08']][1:100,'Y'],col='red')
#'
#' @seealso \code{\link{rubitLinearInterpolate}} to get regular sampling after filtering.
#' @export
rubitMedianFilter <- function(m, k = 15) {
	
	atr <- attributes(m)
	
	if(!any(class(m) == "matrix"))
		stop(sprintf("The function %s expected argument 'm' to be a matrix. If you have a a list of matrices, use lapply to call this function on each element of the list. See examples for details.",gettext(match.call()[[1]]) ))
		
		
	if(attributes(m)$tags.isFiltered)
		warning("This data matrix has already been filtered. You are not working on raw data.")
	
		
	if(attributes(m)$tags.isHomogenous)
		warning("This data has been interpolated before smoothing. This will hardly remove outliers.")
	
	if(nrow(m) < 2*k){
		attributes(m)$tags.hasEnoughPoints <- FALSE
		}
		
	m[,'X'] <- runmed(m[,'X'], k)
	m[,'Y'] <- runmed(m[,'Y'], k)
	m[,'Territory'] <- runmed(m[,'Territory'],k) ##TODO(qg) this filter should be on on ordinal factor (not numeric)
	
	attributes(m)$tags.isFiltered <- TRUE
	
	m
}