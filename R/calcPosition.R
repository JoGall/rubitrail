#' @include main.R
#' @include basic.R
#' @include utils.R
NULL
#' Calculate positional information (e.g. thigmotaxis, exploration) from a circular area.
#'
#' Divides a circular area into a user-defined number of grid cells of equal size in order to quantify exploration (number of unique grid cells visited per unit time). Divides a circular area into inner and outer zones in order to quantify thigmotaxis (movement near the area perimeter).
#'
#' @param m a matrix containing processed tracking data outputted by \code{\link{rubitBasic}}.
#' @param n_radials the number of concentric circles to divide a circular area into.
#' @param n_slices the number of slices to divide a circular area into.
#' @param thigmo_dist the distance from the boundary perimeter defined as being central (i.e. not thigmotaxis), in mm. If thigmo_dist = NA, thigmotaxis is defined as movement in the outer 50\% of the area (i.e. > \eqn{R / sqrt(2)} from the area centre, where \eqn{R} is the radius of the whole area).
#' @param scale a numeric to calibrate the true spatial scale, in pixels per mm. At the default value, measurements are returned in pixels.
#' @param n_bootstraps the number of random data samples used to calculate the minimum enclosing circle defining each circular area.
#' @return The inputted matrix with additional information on position added for each timepoint.
#' @examples
#' data(tenebrio)
#'
#' ### Divide circular area into 96 cells, and define thigmotaxis
#' ### as movement within 2cm of the area perimeter.
#' sapply(tenebrio, rubitCalcPosition, n_radials = 8, n_slices = 12, thigmo_dist = 20)
#'
#' @seealso \code{\link{rubitPlotPosition}} to visualise positional information, and \code{\link{rubitMain}} to understand the different steps of processing. This function uses the function \code{getMinCircle()} from the package 'shotGroups' to calculate a minimum enclosing circle for X,Y-coordinates.
#' @export
rubitCalcPosition <- function(m, n_radials = 1, n_slices = 1, thigmo_dist = NA, scale = 1, n_bootstraps = 20) {
	
	if(!any(class(m) == "matrix"))
		stop(sprintf("The function %s expected argument 'm' to be a matrix. If you have a a list of matrices, use lapply to call this function on each element of the list. See examples for details.",gettext(match.call()[[1]]) ))
		
	overwrite <- FALSE
	if(any(attributes(m)$dimnames[[2]] == "cell")){
		overwrite <- TRUE
		warning("Positions have already been calculated for this data matrix and data has already been conformed.")
	}
	else {
		
		if(attributes(m)$tags.hasEnoughPoints) {
			
			##exploration
			#find minimum enclosing circle
			radials <- getRadials(m[,'X'], m[,'Y'], n_radials, n_bootstraps)
			#convert cartesian to polar
			polarCoords <- cart2polar(m[,'X'], m[,'Y'], radials$midX[1], radials$midY[1])
			#conform x,y-coordinates to fit inside area dimensions
			polarCoords$rad <- ifelse(polarCoords$rad > max(radials$rad), max(radials$rad), polarCoords$rad)
			#convert back to cartesian coordinates
			newXY <- polar2cart(polarCoords$rad, polarCoords$theta, radials$midX[1], radials$midY[1])
			#assign new conformed coordinates
			m[,'X'] <- newXY$X
			m[,'Y'] <- newXY$Y
			
			#calculate cell ID for each x,y point
			cellID <- getCellID(polarCoords, radials, n_slices)
			cell <- cellID$cell
		
			##thigmotaxis
			#calculate inner and outer circle radii
			outer_r <- radials[n_radials,]$rad
			ifelse(is.na(thigmo_dist), inner_r <- outer_r / sqrt(2), inner_r <- outer_r - (thigmo_dist * scale))
	#		#scale area perimeters: may be required for area information 
	#		#derived from image mask
	#		area_scalar <- 0.93
	#		outer_r <- outer_r * area_scalar
	#		inner_r <- outer_r / sqrt(2)

			#continuous thigmotaxis metric: distance from perimeter
			dist_from_mid <- sqrt((radials[n_radials,]$midX - m[,'X'])^2 + (radials[n_radials,]$midY - m[,'Y'])^2)
			perimeter_dist <- outer_r - dist_from_mid
			#discrete thigmotaxis metric: perimeter (1) or centre (0)
			perimeter <- ifelse(dist_from_mid > inner_r, 1, 0)

			if(!overwrite)
				mm <- cbind(m, cell, perimeter_dist, perimeter)
			else {
				mm <- m
				mm[,'cell'] <- cell
				mm[,'perimeter_dist'] <- perimeter_dist / scale
				mm[,'perimeter'] <- perimeter
			}
			
			atr <-attributes(m)
			atr$dim <- attributes(mm)$dim
			atr$dimnames <- attributes(mm)$dimnames
			attributes(mm) <- atr
			attributes(mm)$tags.isConformed <- TRUE
			
		} else {
			cell <- numeric()
			perimeter_dist <- numeric()
			perimeter <- numeric()
			if(!overwrite)
				mm <- cbind(m, cell, perimeter_dist, perimeter)
			else {
				mm <- m
				mm[,'cell'] <- cell
				mm[,'perimeter_dist'] <- perimeter_dist
				mm[,'perimeter'] <- perimeter
			}
			atr <-attributes(m)
			atr$dim <- attributes(mm)$dim
			atr$dimnames <- attributes(mm)$dimnames
			attributes(mm) <- atr
			attributes(mm)$tags.isConformed <- TRUE
		}

		mm
	}
}