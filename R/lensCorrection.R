#' @include main.R
#' @include utils.R
NULL
#' Corrects lens distortion in raw X,Y-coordinates.
#'
#' Corrects lens distortion using the formula \eqn{R = r (ar^3 + br^2 + cr + d )},
#' where \eqn{r} is the distance to the image center and \eqn{R} is the equivalent radius for the corrected image.
#'
#' @param m a matrix with X,Y-coordinate data labelled 'X' and 'Y'.
#' @param imWidth,imHeight the width and height of the original image.
#' @param a,b,c parameters used to correct lens distortion.
#' @note Ready-made undistortion parameters for particular cameras can be found online (e.g. \url{http://sourceforge.net/projects/hugin/files/PTLens\%20Database}).
#' Parameters can be estimated manually using a checkerboard calibration image and the undistortion feature in image manipulation programs such as ImageMagick (\url{http://imagemagick.org}), or automatically using Hugin's lens calibration tool (\url{http://wiki.panotools.org/Calibrate_lens_gui}).
#' See \url{http://www.imagemagick.org/Usage/lens/} for more information on lens correction.
#' @return The inputted matrix with corrected X,Y-coordinates.
#' @examples
#' data(tenebrio)
#'
#' tenebrio <- lensCorrection(tenebrio, 640, 480, b = -0.022)
#'
#' @export
lensCorrection <- function(m, imWidth, imHeight, a = 0, b = 0, c = 0) {
		
		normX <- m[,"X"] - (imWidth / 2)
		normY <- m[,"Y"] - (imHeight / 2)

		radius_u <- sqrt(imWidth^2 + imHeight^2)
		r <- sqrt(normX^2 + normY^2) / radius_u
		d <- 1 - (a+b+c)
		Rsrc <- r * (a*r^3 + b*r^2 + c*r + d)
		Rsrc <- Rsrc * radius_u

		theta <- atan2(normY, normX)
		newX <- (imWidth / 2) + Rsrc * cos(theta)
		newY <- (imHeight / 2) + Rsrc * sin(theta)
		
		mm <- m
		mm[,"X"] <- newX
		mm[,"Y"] <- newY
		
		mm
}