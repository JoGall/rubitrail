#' @include calcPosition.R
#' @include basic.R
#' @include utils.R
NULL
#' Visualise an individual trajectory and positional information in a circular area.
#'
#' Plots a trajectory over a circular area, dividing the area into a number of grid cells of equal size to visualise exploration, and defining an outer perimeter to visualise thigmotaxis.
#'
#' @param m a matrix containing processed tracking data outputted by \code{\link{rubitBasic}}
#' @param n_radials the number of concentric circles to divide a circular arena into
#' @param n_slices the number of slices to divide a circular arena into
#' @param thigmo_dist the distance from the boundary perimeter defined as being central (i.e. not thigmotaxis), in mm. If thigmo_dist = NA, thigmotaxis is defined as movement in the outer 50\% of the area (i.e. > \eqn{R / sqrt(2)} from the area centre, where \eqn{R} is the radius of the area).
#' @param scale a numeric to calibrate the true spatial scale, in pixels per mm. At the default value, measurements are returned in pixels.
#' @param n_bootstraps the number of random data samples used to calculate the minimum enclosing circle defining each circular area.
#' @return a plot of the divided area with the full trajectory overlaid.
#' @examples
#' data(tenebrio)
#'
#' ### Single plot, with area divided into 96 cells
#' ### and thigmotaxis defined in outer 20mm of area:
#' rubitPlotPosition(tenebrio[['05']], n_radials = 8, n_slices = 12)
#'
#' ### Print plots of matrices in list to PDF:
#' #pdf("plots.pdf")
#' lapply(tenebrio, rubitPlotPosition, n_radials = 8, n_slices = 12)
#' #dev.off()
#'
#' @seealso \code{\link{rubitCalcPosition}} for details on calculating positional information.
#' @export
rubitPlotPosition <- function(m, n_radials = 1, n_slices = 1, thigmo_dist = NA, scale = 1, n_bootstraps = 20) {
	
	if(!any(class(m) == "matrix"))
		stop(sprintf("The function %s expected argument 'm' to be a matrix. If you have a a list of matrices, use lapply to call this function on each element of the list. See examples for details.",gettext(match.call()[[1]]) ))
		
	if(attributes(m)$tags.hasEnoughPoints) {
		
		#calculate radials
		radials <- getRadials(m[,'X'], m[,'Y'], n_radials, n_bootstraps)
		#get slice coordinates
		slices <- getSlices(n_slices, radials)
		
		#convert cartesian coordinates to polar
		polarCoords <- cart2polar(m[,'X'], m[,'Y'], radials$midX[1], radials$midY[1])
		#conform x,y-coordinates to fit inside area dimensions
		polarCoords$rad <- ifelse(polarCoords$rad > max(radials$rad), max(radials$rad), polarCoords$rad)
		#convert back to cartesian coordinates
		newXY <- polar2cart(polarCoords$rad, polarCoords$theta, radials$midX[1], radials$midY[1])
		
		#plot
		plot(newXY$X, newXY$Y, type='n', asp=1, xlab="X", ylab="Y", main = paste0(attributes(m)$filename, "\n", attributes(m)$Area))
		#draw radials
		for(i in 1:nrow(radials)){
			draw.circle(radials$midX[i], radials$midY[i], radials$rad[i], border="gray75", lwd=2)
		}
		#draw slices
		for(i in 1:nrow(slices)){
			segments(radials$midX[1], radials$midY[1], slices$x[i], slices$y[i], col="gray75", lwd=2)
		}
		#draw thigmotaxis line
		outer_r <- radials[n_radials,]$rad
		ifelse(is.na(thigmo_dist), inner_r <- outer_r / sqrt(2), inner_r <- outer_r - (thigmo_dist*scale))
		draw.circle(radials$midX[1], radials$midY[1], inner_r, border="red", lwd=2)
		#add complete trajectory
		points(newXY[,"X"], newXY[,"Y"], type='l')
	}
}
