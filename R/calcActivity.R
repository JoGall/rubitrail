#' @include metrics.R
#' @include basic.R
NULL
#' Calculates the frequency and duration of mobile / stationary phases
#'
#' Defines stationary and mobile phases by determining the number of consecutive timepoints in which movement speeds are below (stationary; FALSE) or above (mobile; TRUE) a speed threshold, \code{min_speed}. The window size, \code{window}, defines the number of consecutive timepoints required to change the activity phase, in seconds.
#'
#' @param m a matrix containing processed tracking data outputted by \code{\link{rubitBasic}}
#' @param window the window size used to define changes in activity, in seconds.
#' @param min_speed the minimum speed threshold used to define changes in activity below which no movement is inferred, in mm/second.
#' @param simple logical; if FALSE, more complex information on average distance, velocity and position is returned for individual runs.
#' @return A new matrix containing information on runs for the inputted area.
#' @examples
#' data(tenebrio_basic)
#'
#' ### Apply the function over all areas in list
#' sapply(tenebrio_basic, rubitCalcActivity, window = 3, simple = TRUE)
#'
#' @seealso \code{\link{rubitMetrics}} to understand the different steps of processing. This function uses the run length encoding function, \code{rle()}, from base R.
#' @export
rubitCalcActivity <- function(m, window = 1, min_speed = 0.1, simple = FALSE) {
	
	if(!any(class(m) == "matrix"))
		stop(sprintf("The function %s expected argument 'm' to be a matrix. If you have a a list of matrices, use lapply to call this function on each element of the list. See examples for details.",gettext(match.call()[[1]]) ))
	
	atr <- attributes(m)
	
	if(attributes(m)$tags.hasEnoughPoints) {
		
		window <- round(window * calcFPS(m))
		
		#calculate raw RLE
		m[,"speed_smooth"][is.na(m[,"speed_smooth"])] <- 0
		RLE <- rle(m[,"speed_smooth"] >= min_speed)
		#smooth raw RLE
		if(length(RLE$lengths) > 1 ) {
			for(j in 2:length(RLE$lengths)){
				if(RLE$lengths[j] < window ){
					RLE$values[j] <- RLE$values[j-1]
				}
			}
		}
		RLE_RLE <- rle(RLE$values == TRUE)
		clusters <- rep(1:length(RLE_RLE$lengths), RLE_RLE$lengths)
		comb <- data.frame(lengths = RLE$lengths, values = RLE$values, clusters)
		agg <- aggregate(x = comb$lengths, by = list(comb$values, comb$clusters), FUN = "sum")
		runs <- data.frame(run = agg[,3], values = agg[,1])

		#extract features from RLE
		mm <- matrix(numeric(), ncol=11)
		cum <- 0
		for(j in 1:length(runs$run)){
			#subset each phase
			ss <- m[(cum+1):(cum+runs$run[j]),]
			#make variables
			run <- runs$run[j]
			values <- runs$values[j]
			time_start <- ss[,"time"][1]
			time_end <- ss[,"time"][run]
			duration <- time_end - time_start
			if(simple)
				mm <- rbind(mm, cbind(run, values, start = time_start, end = time_end, duration))
			else{
				speed_mean <- mean(ss[,"speed_smooth"])
				speed_median <- median(ss[,"speed_smooth"])
				speed_var <- var(ss[,"speed_smooth"])
				Distance_sum <- max(cumsum(ss[,"Distance"]))
				perimeter_dist_mean <- mean(ss[,"perimeter_dist"], na.rm=T)
				perimeter_mode <- Mode(ss[,"perimeter"])
				
				mm <- rbind(mm, cbind(run, values, start = time_start, end = time_end, duration, speed_mean, speed_median, speed_var, Distance_sum, perimeter_dist_mean, perimeter_mode))
			}
			cum <- sum(runs$run[1:j])
		}

		atr$dim <- attributes(mm)$dim
		atr$dimnames <- attributes(mm)$dimnames
		attributes(mm) <- atr
		
	} else {
		run <- numeric()
		values <- numeric()
		time_start <- numeric()
		time_end <- numeric()
		duration <- numeric()
					
		if(simple)
			mm <- cbind(run, values, start = time_start, end = time_end, duration)
		else {
			speed_mean <- numeric()
			speed_median <- numeric()
			speed_var <- numeric()
			Distance_sum <- numeric()
			perimeter_dist_mean <- numeric()
			perimeter_mode <- numeric()
		
			mm <- cbind(run, values, start = time_start, end = time_end, duration, speed_mean, speed_median, speed_var, Distance_sum, perimeter_dist_mean, perimeter_mode)
		}
		
		atr$dim <- attributes(mm)$dim
		atr$dimnames <- attributes(mm)$dimnames
		attributes(mm) <- atr
	}

	mm
}