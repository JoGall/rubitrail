#' @include basic.R
#' @include metrics.R
#' @include linearInterpolate.R
NULL
#' Calculates the framerate of a time series, in frames per second.
#'
#' Calculates the modal number of frames per second from a matrix, dataframe or list of time series.
#' 
#' @param dat a matrix or dataframe, or list of matrices or dataframes, that contain a variable called \code{'time'}. Data should not not yet be resampled, e.g. by \code{\link{rubitLinearInterpolate}}
#' @note This function is intended for outputs from the 'rubitrail' package.
#' @return a numeric: the calculated framerate, in frames per second. For a single time series, the median framerate is returned; for a list of time series, the mode of individual framerates is returned.
#' @examples
#' data(weevils)
#'
#' ### Framerate of area '08'
#' calcFPS(weevils[['08']])
#'
#' ### Framerate of each individual area
#' sapply(weevils, calcFPS)
#'
#' ### Modal framerate of all areas combined
#' calcFPS(weevils)
#'
#' @seealso \code{\link{rubitBasic}} and \code{\link{rubitMetrics}} for information on re-encoding a new framerate into tracking data.
#' @export
calcFPS <- function(dat) {
	ifelse(class(dat) == "list", itv <- Mode(sapply(dat, function(x) median(diff(x[,"time"])))), 	itv <- median(diff(dat[,"time"])) )
	fps <- 1000 / itv
	fps
}