#' @include linearInterpolate.R
#' @include medianFilter.R
NULL
#' Removes outliers from tracking data based upon log-likelihood.
#' 
#' Removes outliers from an area matrix based upon the log-likelihood of tracked X,Y-coordinates.
#' @param m a numerical matrix corresponding to an area.
#' @param p the proportion of least likely X,Y-coordinates to remove based on log-likelihood. For example, for \code{p = 0.01}, the least likely 1\% of points will be removed.
#' @return A numerical matrix of the same dimensions as m.
#' @examples
#' data(weevils_raw)
#'
#' Remove least likely 0.01\% of points
#' w_filt <- lapply(weevils_raw, rubitRemoveOutliers, p = 0.001)
#'
#' @seealso \code{\link{rubitLinearInterpolate}} to interpolate X,Y-coordinates after removing outliers.
#' @export
rubitRemoveOutliers <- function(m, p = 0.001){
	if(nrow(m) < 5)
		return(m)
		
	if( p<0 || p>=0.5)
		stop("p must be between 0 and 0.5")
		
	v <- m[,'L']
	mv <- mean(v)
	sdv <- sd(v)
	p_l <- qnorm(p,mv,sdv,lower.tail= T)
# 	t_h <- qnorm(p,mv,sdv,lower.tail= F)
	invalid <- v < p_l
	
	mm <- m[v > p_l,]
	
	atr <- attributes(m)
	atr$dim <- attributes(mm)$dim
	attributes(mm) <- atr
	mm
}