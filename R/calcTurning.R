#' @include main.R
#' @include basic.R
NULL
#' Resamples tracking data and calculates absolute and relative turning angles.
#'
#' Calculates turning angle between successive vectors of movement by resampling data to minimise jitter, returning a matrix containing both absolute and relative angles in degrees.
#'
#' @param m a matrix containing processed tracking data outputted by \code{\link{rubitBasic}}.
#' @param resample the number of seconds over which to resample tracking data and calculate a new trajectory and turning angle from.
#' @return A new matrix containing information on turning angles for the inputted area.
#' @examples
#' data(tenebrio)

#' ### Apply the function over all areas in list
#' sapply(tenebrio, rubitCalcTurning, resample = 1)
#'
#' @seealso \code{\link{rubitMain}} to understand the different steps of processing.
#' @export
rubitCalcTurning <- function(m, resample = 1) {
	
	if(!any(class(m) == "matrix"))
		stop(sprintf("The function %s expected argument 'm' to be a matrix. If you have a a list of matrices, use lapply to call this function on each element of the list. See examples for details.",gettext(match.call()[[1]]) ))
	
	if(attributes(m)$tags.isResampled)
		stop(sprintf("This data matrix has already been resampled. This function requires full raw data."))
	
	atr <- attributes(m)
			
	if(attributes(m)$tags.hasEnoughPoints) {
		
		#change resample rate from seconds to frames
		resample <- resample * calcFPS(m)
		
		#resample data		
		ss <- m[seq(1, nrow(m), by = resample),]

		#calculate turning angle (in degrees)
		v <- diff(complex(real = ss[,'X'], imaginary = ss[,'Y']))
		size <- Mod(v)
		absAngle <- c(NA, diff(Arg(v)) %% (2*pi), NA ) * 360/(2*pi)
		relAngle <- ifelse(absAngle > 180, -(360 - absAngle), absAngle)
	
		#calculate distance moved in resampled period	
		X1 <- c(ss[,'X'], NA)
		X0 <- c(NA, ss[,'X'])
		Y1 <- c(ss[,'Y'], NA)
		Y0 <- c(NA, ss[,'Y'])
		dX <- X1 - X0
		dY <- Y1 - Y0
		D <- sqrt(dX^2 + dY^2)
		D[length(D)-1] <- D[length(D)]
		D <- D[-1]
		D <- c(NA, D[-length(D)])

		mm <- cbind(time = ss[,'time'], Distance = D, absAngle = round(absAngle,3), relAngle = round(relAngle,3))
		
		atr$dim <- attributes(mm)$dim
		atr$dimnames <- attributes(mm)$dimnames
		attributes(mm) <- atr
		attributes(mm)$tags.isResampled <- TRUE
		
	} else {
		absAngle <- numeric()
		relAngle <- numeric()
		time <- numeric()
		Distance <- numeric()
		
		mm <- cbind(time, Distance, absAngle, relAngle)
		
		atr$dim <- attributes(mm)$dim
		atr$dimnames <- attributes(mm)$dimnames
		attributes(mm) <- atr
		attributes(mm)$tags.isResampled <- TRUE
	}

	m
}