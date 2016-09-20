#' @include medianFilter.R
NULL
#' Resamples and interpolates data to achieve regular time intervals, and trims and/or extends tracking data to fit within desired start and end timepoints.
#' 
#' X,Y-positions are returned at a regular time interval using linear interpolation. Data can be trimmed to fit within desired start and end timepoints, or new positions inferred for additional timepoints (before or after object tracking).
#' @param m a numerical matrix corresponding to an area.
#' @param hz the desired resampling frequency, in Hz.
#' @param start_at,end_at the starting / ending times to interpolate and/or cut data to, in minutes.
#' @param adj_fps encodes a new framerate, in Hz.
#' @param minRow an integer defining the minimal number of reads. If less than minRow reads are present in m, the function returns an empty matrix.
#' @return A trimmed and resampled numerical matrix based upon the input data. The attributes of the input matrix are copied to the new matrix.
#' @note X,Y-positions are defined as 'NA' between the \code{start_at} time and the point of first object tracking. For terminally interpolated timepoints (i.e. between last object detection and the \code{end_at} time), the last known X,Y-position is repeated.
#' Re-encoding a new framerate with \code{adj_fps} can correct potential errors made during video recording and/or tracking analysis. Check that the value returned by \code{\link{calcFPS}} matches the calculated framerate of the original video (e.g. using the 'ffprobe' function in FFmpeg [\url{https://ffmpeg.org/}].
#' @examples
#' data(weevils)
#'
#' ### Interpolation before filtering
#' w1 <- lapply(weevils, rubitLinearInterpolate, hz = 50)
#' plot(w1[['08']][,'X'] ~ w1[['08']][,'Y'], asp=1, type='l')
#'
#' ### Interpolation after filtering
#' w19 <- lapply(weevils, rubitRemoveOutliers, p = 0.001)
#' w19 <- lapply(w19, rubitMedianFilter, k = 19)
#' w19 <- lapply(w19, rubitLinearInterpolate, hz = 50)
#' plot(w19[['08']][,'X'] ~ w15[['08']][,'Y'], asp=1, type='l')
#'
#' @seealso \code{\link{rubitMedianFilter}} to smooth data before interpolation.
#' @export
rubitLinearInterpolate <- function(m, hz = 30, start_at = NA, end_at = NA, adj_fps = NA, minRow = 11){
	
	if(!any(class(m) == "matrix"))
		stop("This function works with a matrix. If you have a a list of matrices, use lapply to call this function on each element of the list. See examples for details.")
		
	if(attributes(m)$tags.isHomogenous)
		warning("This data matrix has already been resampled.")
		
	atr <- attributes(m)
		
	#change framerate
	if(!is.na(adj_fps)){
		time_scalar <- 1000 / median(diff(m[,"time"])) / adj_fps
		m[,"time"] <- m[,"time"] * time_scalar
	}
	
	#convert user-specified times from minutes to ms
	ifelse(is.na(start_at), start_at <- min(m[,"time"]), start_at <- start_at * 1000 * 60 )
	ifelse(is.na(end_at), end_at <- max(m[,"time"]), end_at <- end_at * 1000 * 60 )
		
	#trim matrix to timepoints
	m <- subset(m, m[,"time"] >= start_at & m[,"time"] <= end_at)
	
	
	if(nrow(m) >= minRow & atr$tags.hasEnoughPoints) {
		
		#resample time
		t0 <- m[1,'time']
		tf <- m[nrow(m),'time']
		t_out <- seq(from = t0, to = tf, by = 1000 / hz)	

		xx <- approx(x=m[,'time'], y=m[,'X'], xout=t_out)$y
		yy <- approx(x=m[,'time'], y=m[,'Y'], xout=t_out)$y
		L <- approx(x=m[,'time'], y=m[,'L'], xout=t_out)$y
		T <- approx(x=m[,'time'], y=m[,'Territory'], xout=t_out, method='const')$y
		
		mm <- cbind(T, xx, yy, t_out, L)
		
		#interpolate missing timepoints
		new_times_start <- numeric()
		if(start_at < min(mm[,"t_out"]) - (1000/hz) )
			new_times_start <- seq(start_at, min(mm[,"t_out"]) - (1000/hz), 1000 / hz)
		new_times_end <- numeric()
		if(end_at > max(mm[,"t_out"]) + (1000/hz) )
			new_times_end <- seq(max(mm[,"t_out"]), end_at, 1000 / hz)
		
		#adjust data length to match new timepoints (NA X,Y-position for opening frames; last documented X,Y-position inferred for closing frames)
		T_ <- c(rep(NA, length(new_times_start)), mm[,"T"], rep(NA, length(new_times_end)))
		xx_ = c(rep(NA, length(new_times_start)), mm[,"xx"], rep(xx[length(xx)], length(new_times_end)))
		yy_ = c(rep(NA, length(new_times_start)), mm[,"yy"], rep(yy[length(yy)], length(new_times_end)))
		t_out_ = c(new_times_start, mm[,"t_out"], new_times_end)
		L_ = c(rep(NA, length(new_times_start)), mm[,"L"], rep(NA, length(new_times_end)))
			
		m_out <- cbind(T_, xx_, yy_, t_out_, L_)
		
		#reassign attributes
		atr$dim <- attributes(m_out)$dim
		attributes(m_out) <- atr
		attributes(m_out)$tags.hasEnoughPoints <- TRUE
	
	} else {
		m_out <- m
		atr$dim <- c(nrow(m), 5)
		attributes(m_out) <- atr
		attributes(m_out)$tags.hasEnoughPoints <- FALSE
	}
	
	attributes(m_out)$tags.isHomogenous <- TRUE
	
	m_out
}