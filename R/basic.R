#' @include loadFile.R
#' @include utils.R
#' @include calcFPS.R
#' @include linearInterpolate.R
#' @include removeOutliers.R
#' @include medianFilter.R
NULL
#' Reads a raw UbiTrail result file and returns filtered X,Y-trajectories.
#'
#' This function loads a UbiTrail result file as a list of matrices. Raw X,Y-trajectories are then smoothed, interpolated, and resampled before the distance between successive positions is calculated.
#' 
#' @param FILE a .csv result file outputted by UbiTrail.
#' @param scale a numeric to calibrate the true spatial scale, in pixels per mm. At the default value, measurements are returned in pixels.
#' @param hz the frequency of resampling, in Hz. This argument is passed to the interpolation function.
#' @param start_at,end_at the desired start and end times to interpolate and/or cut data to, in minutes.
#' @param adj_fps encodes a new framerate, in Hz.
#' @param xy_smoothing the level of smoothing for raw trajectories. This argument is passed to the filter function.
#' @param p the threshold used to remove outliers, as a proportion of the overall likelihood distribution; e.g. for \code{p = 0.01}, the largest 1\% of outliers will be removed. See \code{\link{rubitRemoveOutliers}} for more information.
#' @param nmin the minimal number of reads. If not enough reads are presents in an area, an empty matrix is returned.
#' @param a,b,c parameters to correct lens distortion. See \code{\link{lensCorrection}} for more information.
#' @param filterFUN the filter function to be used. It must have the same arguments as \code{\link{rubitMedianFilter}}.
#' @param interpFUN the interpolation function to be used. It must have the same arguments as \code{\link{rubitLinearInterpolate}}.
#' @param verbose logical; if TRUE, the function will print messages at every step.
#' @return A list of numerical matrices, with each matrix corresponding to an area. The attributes of list contain metadata about the original video file and attributes of each matrix contain information on the dimensions of the area.
#' @note Re-encoding a new framerate with \code{adj_fps} can correct potential errors made during video recording and/or tracking analysis. Check that the value returned by \code{\link{calcFPS}} matches the calculated framerate of the original video (e.g. using the 'ffprobe' function in FFmpeg [\url{https://ffmpeg.org/}].
#' @examples
#' data(tenebrio)
#'
#' ## See general metadata:
#' attributes(tenebrio)
#'
#' ## See information on area '01':
#' attributes(tenebrio[['01']])
#'
#' ### Create a filelist of all results files in a directory, e.g.
#' # filelist <- list.files()
#'
#' ### Read a single results file
#' rubitBasic(filelist[1], scale= 2.08, hz = 20, adj_fps = 19.05, b = -0.022, verbose = TRUE)
#'
#' ## Apply function over a list of results files:
#' lapply(filelist, rubitBasic, scale= 2.08, hz = 20, adj_fps = 19.05, b = -0.022, verbose = TRUE)
#'
#' @seealso \code{\link{rubitToDF}} for converting the returned list (or list of lists) to a dataframe for ease of further analysis. See \code{\link{rubitLinearInterpolate}}, \code{\link{rubitMedianFilter}}, and \code{\link{rubitRemoveOutliers}} to understand the different steps of processing used in this function. Also see code{\link{calcFPS}} for calculating the framerate of data and see \code{\link{lensCorrection}} for more information on lens distortion.
#' @export
rubitBasic <- function(FILE, scale = 1, hz = 30, start_at = NA, end_at = NA, adj_fps = NA, xy_smoothing = 15, p = 0.001, nmin = xy_smoothing*10, a = 0, b = 0, c = 0, filterFUN = rubitMedianFilter, interpFUN = rubitLinearInterpolate, verbose = FALSE){

	#read data
	l <- rubitLoadFile(FILE)
	atrs <- attributes(l)  #remember original attributes
	
	#lens correction
	imWidth <- as.numeric(attributes(l)$Width)
	imHeight <- as.numeric(attributes(l)$Height)
	l <- lapply(l, lensCorrection, imWidth, imHeight, a = a, b = b, c = c)

	if(p > 0){
		if(verbose) print(sprintf("Removing outliers..."))
		l <- lapply(l, rubitRemoveOutliers, p = p )
	}

	if(verbose) print(sprintf("Filtering data..."))
	l <- lapply(l, filterFUN, k = xy_smoothing )
	
	if(verbose) print("Interpolating data..")
	l <- lapply(l, interpFUN, adj_fps = adj_fps, hz = hz, start_at = start_at, end_at = end_at, minRow = nmin )
	
	if(verbose) print("Calculating distances...")
	l <- lapply(l, rubitCalcDistance, scale = scale )
	
	attributes(l) <- atrs  #reinstate attributes
	
    return(l)
}