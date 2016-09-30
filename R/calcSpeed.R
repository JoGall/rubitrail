#' @include utils.R
#' @include metrics.R
#' @include basic.R
NULL
#' Calculates speed and acceleration from processed tracking data.
#'
#' Calculates raw and and smoothed (using a rolling median) speed and acceleration from smoothed tracking data.
#'
#' @param m a matrix containing processed tracking data outputted by \code{\link{rubitBasic}}.
#' @param window the size of the rolling median window used to smooth speed and acceleration, in frames.
#' @return The inputted matrix with additional information added for speed (in mm/s) and acceleration (in mm/s^2).
#' @examples
#' data(tenebrio_basic)
#'
#' ### Apply the function over all areas in list
#' sapply(tenebrio_basic, rubitCalcSpeed, window = 1)
#'
#' @seealso \code{\link{rubitMetrics}} to understand the different steps of processing.
#' @export
rubitCalcSpeed <- function(m, window = 21) {
	
	if(!any(class(m) == "matrix"))
		stop(sprintf("The function %s expected argument 'm' to be a matrix. If you have a a list of matrices, use lapply to call this function on each element of the list. See examples for details.",gettext(match.call()[[1]]) ))
		
	overwrite <- FALSE
	if(any(attributes(m)$dimnames[[2]] == "rawSpeed")){
		overwrite <- TRUE
		warning("Speeds have already been calculated for this data matrix. Overwriting the data.")
	}
		
	if(attributes(m)$tags.hasEnoughPoints) {
		
		FPS <- calcFPS(m)
		speed_raw <- m[,'Distance'] * FPS
		speed_smooth <- my.rollapply(speed_raw, window, median)
		accel_raw <- abs(c(NA, diff(speed_raw) ) )  #acceleration defined as *absolute* change in speed
		accel_smooth <- my.rollapply(accel_raw, window, median)
		accel_speed_smooth <- abs(c(NA, diff(speed_smooth)) )

		if(!overwrite)
			mm <- cbind(m, speed_raw, speed_smooth, accel_raw, accel_smooth, accel_speed_smooth)
		else {
			mm <- m
			mm[,'speed_raw'] <- speed_raw
			mm[,'speed_smooth'] <- speed_smooth
			mm[,'accel_raw'] <- accel_raw
			mm[,'accel_smooth'] <- accel_smooth
			mm[,'accel_speed_smooth'] <- accel_speed_smooth
		}
		
		atr <-attributes(m)
		atr$dim <- attributes(mm)$dim
		atr$dimnames <- attributes(mm)$dimnames
		attributes(mm) <- atr
		attributes(mm)$tags.isBasic <- FALSE
	} else {
		speed_raw <- numeric()
		speed_smooth <- numeric()
		accel_raw <- numeric()
		accel_smooth <- numeric()
		accel_speed_smooth <- numeric()
		if(!overwrite)
			mm <- cbind(m, speed_raw, speed_smooth, accel_raw, accel_smooth, accel_speed_smooth)
		else {
			mm <- m
			mm[,'speed_raw'] <- speed_raw
			mm[,'speed_smooth'] <- speed_smooth
			mm[,'accel_raw'] <- accel_raw
			mm[,'accel_smooth'] <- accel_smooth
			mm[,'accel_speed_smooth'] <- accel_speed_smooth
		}
		atr <-attributes(m)
		atr$dim <- attributes(mm)$dim
		atr$dimnames <- attributes(mm)$dimnames
		attributes(mm) <- atr
		attributes(mm)$tags.isBasic <- FALSE
	}

	mm
}