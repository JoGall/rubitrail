Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

my.rollapply <- function(vec, window, FUN) {
    m <- sapply(seq_along(vec), 
		function(i) if (i < window) NA else FUN(vec[i:(i-window+1)])
	)
	#centre align
	n <- length(m)
	p <- floor(window/2)
	mm <- c(m[(p+1):n], rep(NA, p))

	mm
}

rubitLoadFile <- function(FILE, verbose=FALSE){
	if(verbose) print("Reading data...")
	meta <- rubitMetaData(FILE)
	data <- rubitData(FILE)
	if(verbose) print("Formatting data...")
	l <- rubitParseDataToList(data, meta)
	if(verbose) print("Converting data to absolute data...")
	
	#To preserve the attributes of l:
	atrs <- attributes(l);
	l <- lapply(l, h_rubitRelativeToAbsolute)
	attributes(l) <- atrs
	
	l
}

rubitMetaData <- function(FILE, verbose=FALSE){
    e <- scan(FILE, what='character', nlines=1, quiet=!verbose)
    meta <- eval(parse(text=e))
    
    H <- as.numeric(meta$Global['Height']) 
    meta$Global['filename'] <- FILE
    meta$Areas['Y',] <- (H - meta$Areas['Y',]) - meta$Areas['H',]
    return(meta)
}
	
rubitData <- function(FILE, verbose=FALSE){
    n <- scan(FILE, what='character', skip=1,sep=',', nlines=1, quiet=!verbose)
    a <- scan(FILE, skip=2, sep=',', quiet=!verbose)
    nc <- length(n)
    nr <- length(a) / nc
    m_ <- matrix(a, nr, nc, byrow=TRUE)
    colnames(m_) <- n
    m_[,'Y'] <- 1 - m_[,'Y']
    m_
}

rubitParseDataToList <- function(data, meta){
	Global <- meta$Global
	Areas <- meta$Areas
	filename <- meta$Global['filename']
	l <- lapply(colnames(Areas), h_rubitSetAttributes, m = data, areas = Areas, filename = filename)
	names(l) <- colnames(meta$Areas)
    attributes(l) <- c(attributes(l), as.list(Global))
    
    l
}

h_rubitSetAttributes <- function(i, m, areas, filename) {
	mm <- m[m[,'Area']==as.numeric(i),-c(which(colnames(m) == 'Area'))]
	tag <- list(hasEnoughPoints = TRUE, isHomogenous = FALSE, isFiltered = FALSE, isConformed = FALSE, isResampled = FALSE)
	attributes(mm) <- c(attributes(mm), list(Area=i), as.list(areas[,i]), filename, tags = tag)
	
	return(mm)		
}

h_rubitRelativeToAbsolute <- function(m) {
	h <- as.numeric(attributes(m)$H)
	w <- as.numeric(attributes(m)$W)
	x0 <- as.numeric(attributes(m)$X)
	y0 <- as.numeric(attributes(m)$Y)
	m[,'X'] <- m[,'X']*w + x0
	m[,'Y'] <- m[,'Y']*h + y0
	
	m
}

rubitCalcDistance <- function(m, scale = 1){
	
	if(!any(class(m) == "matrix"))
		stop(sprintf("The function %s expected argument 'm' to be a matrix. If you have a a list of matrices, use lapply to call this function on each element of the list. See examples for details.",gettext(match.call()[[1]]) ))
		
	overwrite <- FALSE
	if(any(attributes(m)$dimnames[[2]] == "Distance")){
		overwrite <- TRUE
		warning("These distances have already been calculated for this data matrix. Overwriting the data.")
	}
	
		
	if(attributes(m)$tags.hasEnoughPoints){
		
		X <- c(m[,'X'],NA)
		lagX <- c(NA,m[,'X'])
		Y <- c(m[,'Y'],NA)
		lagY <- c(NA,m[,'Y'])
		
		dX <- X - lagX
		dY <- Y - lagY
		
		D <- sqrt(dX^2 + dY^2)
		D[length(D)-1] <- D[length(D)]
		D <- D[-1]
		if(!overwrite)
			mm <- cbind(m,Distance = D)
		else{
			mm <- m
			mm[,'Distance'] <- D / scale
			}
		
		atr <-attributes(m)
		atr$dim <- attributes(mm)$dim
		atr$dimnames <- attributes(mm)$dimnames
		attributes(mm) <- atr
		}
	else{
		D <- numeric()
		if(!overwrite)
			mm <- cbind(m,Distance = D)
		else{
			mm <- m
			mm[,'Distance'] <- D
			}
#~ 		mm <- cbind(m,Distance = D)
		atr <-attributes(m)
		atr$dim <- attributes(mm)$dim
		atr$dimnames <- attributes(mm)$dimnames
		attributes(mm) <- atr
	}
	mm
}

getMinCircle_v <- function(m) {
	circ <- getMinCircle(m)
	return(c(circ$ctr, circ$rad))
}

getRadials <- function(x, y, n_radials, n_bootstraps = 20) {
	#median minimum enclosing circle (MEC)
	xy <- matrix(c(x, y), ncol=2)
	xy <- na.omit(xy)
	ssArr <- replicate(n_bootstraps, xy[sample(nrow(xy), 500, replace=T),], simplify=F)
	
	#calculate MEC using smoothed X,Y data
	circlesXY <- sapply(ssArr, function(x) getMinCircle_v(x))
	midX <- median(circlesXY[1,])
	midY <- median(circlesXY[2,])
	#radii of circles
	outer_rad <- median(circlesXY[3,])
	rad <- c()
	for(i in 1:n_radials){
		rad_i <- outer_rad * sqrt(i/n_radials)
		rad <- rbind(rad, rad_i)
	}

	rads <- data.frame(id = 1:n_radials, midX = rep(midX, n_radials), midY = rep(midY, n_radials), rad, row.names=NULL)

	return(rads)
}

makeRadials <- function(midX, midY, my_rad, n_radials) {
	outer_rad <- my_rad
	rad <- c()
	for(i in 1:n_radials){
		rad_i <- outer_rad * sqrt(i/n_radials)
		rad <- rbind(rad, rad_i)
	}

	rads <- data.frame(id = 1:n_radials, midX = rep(midX, n_radials), midY = rep(midY, n_radials), rad, row.names=NULL)

	return(rads)
}

getSlices <- function(n_slices, radii) {
	angs <- seq(0, 360, 360 / n_slices)
	
	df <- sapply(angs, function(x) getCoords(x, max(radii$rad), radii$midX[1], radii$midY[1]))
	
	df <- data.frame(matrix(unlist(t(df)), ncol=2))
	names(df) <- c("x", "y")
	
	return(df)
}

getCoords <- function(a, d, x0, y0) {
	a <- ifelse(a <= 90, 90 - a, 450 - a)
	data.frame(x = x0 + d * cos(a / 180 * pi), y = y0 + d * sin(a / 180 * pi))
}

cart2polar <- function(x, y, midX, midY) {
	#normalise coordinates relative to centre of circle
	normX <- x - midX
	normY <- y - midY
	
	#convert to polar coordinates
	rad <- sqrt(normX^2 + normY^2)
	theta <- atan2(normY, normX)
	
	#convert radians to degrees
	theta = theta / pi * 180
	
	return(data.frame(rad, theta))
}

polar2cart <- function(rad, theta, midX, midY){
	#convert degrees to radians
	theta = theta * pi / 180
	
	#normalise and convert to cartesian coordinates
	X <- midX + rad * cos(theta)
	Y <- midY + rad * sin(theta)

	return(data.frame(X, Y))
}

getCellID <- function(polarCoords, radials, n_slices) {
	#bin by slice
	slice_breaks <- seq(-180, 180, 360/n_slices)
	sliceID <- cut(polarCoords$theta, slice_breaks)
	levels(sliceID) <- c(1:n_slices)
	
	#bin by radial
	radial_breaks <- c(0, radials$rad)
	radialID <- cut(polarCoords$rad, radial_breaks)
	levels(radialID) <- c(1:nrow(radials))
	
	#generate unique cell ID
	cellID <- n_slices * (as.numeric(radialID) - 1) + as.numeric(sliceID)
	
	#output
	data.frame(slice = sliceID, radial = radialID, cell = cellID)
}
