#' @include metrics.R
#' @include basic.R
#' @include utils.R
NULL
#' Calculate positional information (e.g. thigmotaxis, exploration) from a circular area.
#'
#' Divides a circular area into inner and outer zones in order to quantify thigmotaxis (distance from area perimeter), and divides a circular area into any number of equally-sized grid cells in order to quantify exploration (number of unique area grid cells visited).
#'
#' @param m a matrix containing processed tracking data outputted by \code{\link{rubitBasic}}.
#' @param scale a numeric to calibrate the true spatial scale, in pixels per mm. If \code{scale} == 1, measurements are returned in pixels. This value should match that used in \code{\link{rubitBasic}}.
#' @param area_rad the minimum radius of the area. If an area shows insufficient movement to define a minimum enclosing circle of at least this radius, then a new minimum enclosing circle is calculated using \code{area_rad} and area metainformation stored in \code{attributes(m)}. This unit is defined in pixels unless \code{scale} != 1.
#' @param thigmo_dist the distance from the boundary perimeter defined as being central (i.e. not thigmotaxis). If thigmo_dist = NA, thigmotaxis is defined as movement in the outer 50\% of the area (i.e. > \eqn{R / sqrt(2)} from the area centre, where \eqn{R} is the radius of the area). This unit is defined in pixels unless \code{scale} != 1.
#' @param n_radials the number of concentric circles to divide a circular area into.
#' @param n_slices the number of slices to divide a circular area into.
#' @param n_bootstraps the number of random data samples used to calculate the minimum enclosing circle defining each circular area.
#' @return The input matrix with additional positional information added for each timepoint.
#' @examples
#' data(tenebrio)
#'
#' ### Divide circular area into 96 cells, and define thigmotaxis
#' ### as movement within 2cm of the area perimeter.
#' sapply(tenebrio, rubitCalcPosition, n_radials = 8, n_slices = 12, thigmo_dist = 20)
#'
#' @seealso \code{\link{rubitPlotPosition}} to visualise positional information, and \code{\link{rubitMetrics}} to understand the different steps of processing. This function uses the function \code{getMinCircle()} from the package 'shotGroups' to calculate a minimum enclosing circle for X,Y-coordinates.
#' @export
rubitCalcPosition <- function(m, scale = 1, area_rad = NA, thigmo_dist = NA, n_radials = 1, n_slices = 1, n_bootstraps = 20) {
	
	if(!any(class(m) == "matrix"))
		stop(sprintf("The function %s expected argument 'm' to be a matrix. If you have a a list of matrices, use lapply to call this function on each element of the list. See examples for details.",gettext(match.call()[[1]]) ))
		
	overwrite <- FALSE
	if(any(attributes(m)$dimnames[[2]] == "cell")){
		overwrite <- TRUE
		warning("Positions have already been calculated for this data matrix and data has already been conformed.")
	}
	else {
		
		if(attributes(m)$tags.hasEnoughPoints) {
			
			##define area perimeter and get radials
			#if a minimum radius is defined, use area meta data from attributes to define radials in areas with insufficient movement
			if(!is.na(area_rad)) {
				rad0 <- getMinCircle(na.omit(m[,c("X", "Y")]))$rad
				if(rad0*1.03 < area_rad*scale) { # account for 3% variation in radius size
					midX <- attributes(m)$X + (attributes(m)$W / 2)
					midY <- attributes(m)$Y + (attributes(m)$H / 2)
					radials <- makeRadials(midX, midY, area_rad*scale, n_radials)
				}
				else
					radials <- getRadials(m[,'X'], m[,'Y'], n_radials, n_bootstraps)
			#otherwise calculate radials from X,Y-coords
			} else	
				radials <- getRadials(m[,'X'], m[,'Y'], n_radials, n_bootstraps)
			
			##cartesian >> polar coords
			#convert cartesian to polar
			polarCoords <- cart2polar(m[,'X'], m[,'Y'], radials$midX[1], radials$midY[1])
			#conform x,y-coordinates to fit inside area dimensions
			polarCoords$rad <- ifelse(polarCoords$rad > max(radials$rad), max(radials$rad), polarCoords$rad)
			#convert back to cartesian coordinates
			newXY <- polar2cart(polarCoords$rad, polarCoords$theta, radials$midX[1], radials$midY[1])
			#assign new conformed coordinates
			m[,'X'] <- newXY$X
			m[,'Y'] <- newXY$Y
			
			##calculate exploration
			#calculate cell ID for each x,y point
			cellID <- getCellID(polarCoords, radials, n_slices)
			cell <- cellID$cell
		
			##define thigmotaxis
			#calculate inner and outer circle radii
			outer_r <- radials[n_radials,]$rad
			ifelse(is.na(thigmo_dist), inner_r <- outer_r / sqrt(2), inner_r <- outer_r - (thigmo_dist*scale))

			#continuous thigmotaxis metric: distance from perimeter
			dist_from_mid <- sqrt((radials[n_radials,]$midX - m[,'X'])^2 + (radials[n_radials,]$midY - m[,'Y'])^2)
			perimeter_dist <- (outer_r - dist_from_mid) / scale

			#discrete thigmotaxis metric: perimeter (1) or centre (0)
			perimeter <- ifelse(dist_from_mid > inner_r, 1, 0)
			if(!overwrite)
				mm <- cbind(m, cell, perimeter_dist, perimeter)
			else {
				mm <- m
				mm[,'cell'] <- cell
				mm[,'perimeter_dist'] <- perimeter_dist
				mm[,'perimeter'] <- perimeter
			}
			
			
			##reassign attributes
			atr <-attributes(m)
			atr$dim <- attributes(mm)$dim
			atr$dimnames <- attributes(mm)$dimnames
			attributes(mm) <- atr
			attributes(mm)$tags.isConformed <- TRUE
			
		} else {
			##else if not enough points
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