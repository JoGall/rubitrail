##main functions
positionalInfo <- function(m, n_radials, n_slices, n_bootstraps = 20) {
	#create empty storage
	global_df <- data.frame()
	
	message("Total number of cells in arena: ", n_radials * n_slices)
	
	#make progress bar
	tmp <- 1
	pb <- txtProgressBar(min = 0, max = length(unique(m$id)), style = 3)
	
	##filelist loop
	for(i in unique(m$id)) {
		#subset
		ss <- subset(m, id == i)
		
		##exploration
		#calculate radials
		radials <- getRadials(ss$smoothX, ss$smoothY, n_radials, n_bootstraps)
		#convert cartesian to polar
		polarCoords <- getPolarCoords(ss$smoothX, ss$smoothY, radials$midX[1], radials$midY[1])
		#calculate cell ID for each x,y point
		cellID <- getCellID(polarCoords, radials, n_slices)
	
		##thigmotaxis
		#calculate inner and outer circle radii
		outer_r <- radials[n_radials,]$rad
		inner_r <- outer_r / sqrt(2)
#		#scale area perimeters, if necessary
#		outer_r <- outer_r * 0.93
#		inner_r <- outer_r / sqrt(2)
		#determine inner or outer position
		dist_from_mid <- sqrt((radials[n_radials,]$midX - ss$smoothX)^2 + (radials[n_radials,]$midY - ss$smoothY)^2)
		position <- ifelse(dist_from_mid > inner_r, "OUTSIDE", "INSIDE")
		
		##output
		ss_df <- data.frame(ss, cell = cellID$cell, position)
		global_df <- rbind(global_df, ss_df)
	
		## update progress bar
		Sys.sleep(0.1)
		setTxtProgressBar(pb, tmp)
		tmp <- tmp+1
	}
	close(pb)
	
	return(global_df)
}

plotExplor <- function(data, n_radials, n_slices) {
	#calculate radials
	radials <- getRadials(data$smoothX, data$smoothY, n_radials)
	#convert X,Y Cartesian coordinates to polar
	polarCoords <- getPolarCoords(data$smoothX, data$smoothY, radials$midX[1], radials$midY[1])
	#adjust arena size if all points not contained within
	if(max(polarCoords$rad, na.rm=T) > max(radials$rad, na.rm=T)) {
		alpha <- max(polarCoords$rad, na.rm=T) / max(radials$rad, na.rm=T)
		radials$rad <- radials$rad * alpha
	}
	#get slice coordinates
	slices <- getSlices(n_slices, radials)

	#plot
	plot(data$smoothX, data$smoothY, type='n', asp=1, xlab="X", ylab="Y")
	#draw radials
	for(i in 1:nrow(radials)){
		draw.circle(radials$midX[i], radials$midY[i], radials$rad[i], border="gray75", lwd=2)
	}
	#draw slices
	for(i in 1:nrow(slices)){
		segments(radials$midX[1], radials$midY[1], slices$x[i], slices$y[i], col="gray75", lwd=2)
	}
	#add complete path
	points(data[,"smoothX"], data[,"smoothY"], type='l')
	
#	#plot random points
#	ss_point <- data.frame(X, Y)[sample(1:length(X), 1),]
#	points(ss_point, col="red", pch=19, cex=1.5)
}
	
##sub functions
getEnclosingCircle <- function(m) {
	circ <- getMinCircle(m)
	return(c(circ$ctr, circ$rad))
}

getRadials <- function(x, y, n_radials, n_bootstraps = 20) {
	#median minimum enclosing circle (MEC)
	xy <- matrix(c(x, y), ncol=2)
	xy <- na.omit(xy)
	ssArr <- replicate(n_bootstraps, xy[sample(nrow(xy), 500, replace=T),], simplify=F)
	
	##calculate MEC using smoothed X,Y data
	circlesXY <- sapply(ssArr, function(x) getEnclosingCircle(x))
	midX <- median(circlesXY[1,])
	midY <- median(circlesXY[2,])
	#radii of circles
	outer_rad <- median(circlesXY[3,])
	rad <- c()
	for(i in 1:n_radials){
		rad_i <- outer_rad * sqrt(i/n_radials)
		rad <- rbind(rad, rad_i)
	}
# 	##calulate MEC using area metadata from mask
# 	circlesMeta <- sapply(ssArr, function(x) getEnclosingCircle(x))
# 	midX <- median(circlesMeta[1,])
# 	midY <- median(circlesMeta[2,])
# 	#radii of circles
# 	outer_rad <- median(circlesMeta[3,])
# 	rad <- c()
# 	for(i in 1:n_radials){
# 		rad_i <- outer_rad * sqrt(i/n_radials)
# 		rad <- rbind(rad, rad_i)
# 	}

	##output
	r_list <- data.frame(id = 1:n_radials, midX = rep(midX, n_radials), midY = rep(midY, n_radials), rad, row.names=NULL)
	return(r_list)
}

getPolarCoords <- function(x, y, midX, midY) {
	#normalise coordinates relative to centre of circle
	normX <- x - midX
	normY <- y - midY
	#convert Cartesian to polar coordinates
	rad <- sqrt(normX^2 + normY^2)
	theta <- atan2(normY, normX) / pi * 180
	return(data.frame(rad, theta))
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

getCoords <- function(a, d, x0, y0) {
  a <- ifelse(a <= 90, 90 - a, 450 - a)
  data.frame(x = x0 + d * cos(a / 180 * pi), 
             y = y0 + d * sin(a / 180 * pi))
}

getSlices <- function(n_slices, radii) {
	angs <- seq(0, 360, 360 / n_slices)
	df <- sapply(angs, function(x) getCoords(x, max(radii$rad), radii$midX[1], radii$midY[1]))
	df <- data.frame(matrix(unlist(t(df)), ncol=2))
	names(df) <- c("x", "y")
	return(df)
}


# ##thigmotaxis
# #minimum enclosing circle funtion
# minCir <- function(m) {
# 	C <- getMinCircle(m)
# 	return(c(C$ctr, C$rad))
# }
# estimate range of movement
# calcRangeX <- diff(range(na.omit(X)))
# trueRangeX <- attributes(rawread[[j]])$W
# calcRangeY <- diff(range(na.omit(Y)))
# trueRangeY <- attributes(rawread[[j]])$H			
# #if sufficient object movement, try to find minimum enclosing circle from x,y data
# if(calcRangeX/trueRangeX > 0.8 & calcRangeY/trueRangeY > 0.8){
# 	xy <- matrix(c(X, Y), ncol=2)
# 	ssArr <- replicate(100, xy[sample(nrow(xy), 100, replace=T),], simplify=F)
# 	circles <- sapply(ssArr, function(x) minCir(x))
# 	midX <- median(circles[1,])
# 	midY <- median(circles[2,])
# 	outer_r <- median(circles[3,])
# 	inner_r <- outer_r / sqrt(2)
# #else estimate bounding circle using original area metadata
# } else {
# 	midX <- attributes(rawread[[j]])$X + (0.5*attributes(rawread[[j]])$W)
# 	midY <- attributes(rawread[[j]])$Y + (0.5*attributes(rawread[[j]])$H)
# 	outer_r <- mean(c(attributes(rawread[[j]])$W, attributes(rawread[[j]])$H)) / 2 * 0.95
# 	inner_r <- outer_r / sqrt(2)
# }
# #scale area perimeters, if necessary
# outer_r <- outer_r * 0.93
# inner_r <- outer_r / sqrt(2)
# #determine inner or outer position
# dist_from_mid <- sqrt((midX - X)^2 + (midY - Y)^2)
# position <- ifelse(dist_from_mid > inner_r, "OUTSIDE", "INSIDE")


# #==================
# # draft functions
# #==================
#
# explorFun <- function(m, n_radials, n_slices, n_bootstraps = 20) {
# 	#create empty storage
# 	global_df <- data.frame()
# 	
# 	#make progress bar
# 	tmp <- 1
# 	pb <- txtProgressBar(min = 0, max = length(unique(m$id)), style = 3)
# 	
# 	#loop
# 	for(i in unique(m$id)) {
# 		#subset
# 		ss <- subset(m, id == i)
# 		#calculate radials
# 		radials <- getRadials(ss$smoothX, ss$smoothY, n_radials, n_bootstraps)
# 		#convert cartesian to polar
# 		polarCoords <- getPolarCoords(ss$smoothX, ss$smoothY, radials$midX[1], radials$midY[1])
# 		#calculate cell ID for each x,y point
# 		cellID <- getCellID(polarCoords, radials, n_slices)
# 		
# 		#output
# 		ss_df <- data.frame(ss, cell = cellID$cell)
# 		global_df <- rbind(global_df, ss_df)
# 	
# 		# update progress bar
# 		Sys.sleep(0.1)
#    		setTxtProgressBar(pb, tmp)
# 		tmp <- tmp+1
# 	}
# 	close(pb)
# 	
# 	message("Total number of cells in arena: ", n_radials * n_slices)
# 	
# 	return(global_df)
# }

###########################################
## TESTING
###########################################

# ##create test data
# d <- ubitBasic("~/Desktop/toAnalyse/analysis/18/1/Result.csv")
# dd <- d[[01]]
# X <- dd[,"X"]
# Y <- dd[,"Y"]
# 
# #subsample
# ss <- dd[sample(nrow(dd),10),]
# 
# radii <- getRadials(X, Y, 4)
# segs <- getSegments(12, radii)
# 
# ## example plot	
# #plot
# plot(X, Y, type='l', asp=1)
# #draw concentric circles
# for(i in 1:nrow(radii)){
# 	draw.circle(radii$x[i], radii$y[i], rings$rad[i], border="gray75", lwd=2)
# }
# #draw segments
# for(i in 1:nrow(slices)){
# 	segments(radii$x[1], radii$y[1], segs$x[i], segs$y[i], col="gray75", lwd=2)
# }
# #plot random points
# #set.seed(1)
# ss_point <- data.frame(X, Y)[sample(1:length(X), 1),]
# points(ss_point, col="red", pch=19, cex=1.5)
