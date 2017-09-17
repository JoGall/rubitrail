##calculate statistics: general function
rubitStats <- function(dat, n_bins = 1, var_name, means = TRUE, infer_zero_treatments = NA, sub_bins = n_bins, n_cells = 96, min_speed = 0.1) {

	##determine if overall stats or time series
	if(var_name == "pause_dur" | var_name == "walk_dur" | var_name == "pauses" | var_name == "time_walking")
		ifelse(n_bins > 1, dat$time_bin <- cut(cumsum(dat$duration), breaks = n_bins, labels=FALSE), dat$time_bin <- rep(1, nrow(dat)))
	else
		ifelse(n_bins > 1, dat$time_bin <- cut(dat$time, breaks = n_bins, labels=FALSE), dat$time_bin <- rep(1, nrow(dat)))
		
	##activity metrics
	#pause duration
	if(var_name == "pause_dur") {
		ss <- subset(dat, values==0)
		z <- ddply(ss, .(id, treat, time_bin), .fun = function(x) median(x[,"duration"]/1000, na.rm=T)
		)
	#walk duration
	} else if(var_name == "walk_dur") {
		ss <- subset(dat, values==1)
		z <- ddply(ss, .(id, treat, time_bin), .fun = function(x) median(x[,"duration"]/1000, na.rm=T)
		)
	#number of pauses / walks
	} else if(var_name == "pauses") {
		ss <- subset(dat, values==0)
		z <- ddply(ss, .(id, treat, time_bin), .fun = function(x) nrow(x)
		)
	#proportion of time spent moving
	} else if(var_name == "time_walking") {
		z <- ddply(dat, .(id, treat, time_bin), .fun = function(x) sum(x[x["values"]==1,][,"duration"]) / sum(x[,"duration"])
		)
	}
	
	##speed and position metrics
	#total distance travelled
	else if(var_name == "distance") {
		z <- ddply(dat, .(id, treat, time_bin), .fun = function(x) sum(x[,"Distance"], na.rm=T)
		)
	#total exploration
	} else if(var_name == "explor") {
		z <- ddply(dat, .(id, treat, time_bin), .fun = function(x) length(unique(x[,"cell"], na.rm=T)) / n_cells
		)
	#total exploration (measured every minute)
	} else if(var_name == "explor_per_min") {
		ss <- dat
		mins <- ceiling(max(ss$time) / 1000 / 60)
		#first time bin (per minute)
		ss$time_bin_1 <- cut(ss$time, breaks = mins, labels=FALSE)
		z <- ddply(ss, .(id, treat, time_bin_1), .fun = function(x) length(unique(x[,"cell"], na.rm=T)) / n_cells
		)
		#second time bins (per x minutes)
		ifelse(n_bins > 1, z$time_bin_2 <- cut(z$time_bin_1, breaks = n_bins, include.lowest=T), z$time_bin_2 <- rep(1, nrow(z)) )
		z <- ddply(z, .(id, treat, time_bin_2), .fun = function(x) median(x[,"V1"], na.rm=T)
		)
		#convert time_bin_2 factor levels to integer factor (i.e. 1:n_bins)
		levels(z$time_bin_2) <- 1:length(levels(z$time_bin_2))
		z$time_bin_2 <- as.integer(z$time_bin_2)
		names(z)[3] <- "time_bin"
	#exploration per distance travelled
	} else if(var_name == "explor_per_dist") {
		ss <- subset(dat, Distance > 0.1)
		z <- ddply(ss, .(id, treat, time_bin), .fun = function(x) length(unique(x[,"cell"], na.rm=T)) / n_cells / sum(x[,"Distance"], na.rm=T)
		)
	#exploration per distance travelled (measured every minute)
	} else if(var_name == "explor_per_min_per_dist") {
		ss <- dat
		mins <- ceiling(max(ss$time) / 1000 / 60)
		#first time bin (per minute)
		ss$time_bin_1 <- cut(ss$time, breaks = mins, labels=FALSE)
		ss <- subset(ss, Distance > 0.1)
		z <- ddply(ss, .(id, treat, time_bin_1), .fun = function(x) length(unique(x[,"cell"], na.rm=T)) / n_cells / sum(x[,"Distance"], na.rm=T)
		)
		#second time bins (per x minutes)
		ifelse(n_bins > 1, z$time_bin_2 <- cut(z$time_bin_1, breaks = n_bins, include.lowest=T), z$time_bin_2 <- rep(1, nrow(z)) )
		z <- ddply(z, .(id, treat, time_bin_2), .fun = function(x) median(x[,"V1"], na.rm=T)
		)
		#convert time_bin_2 factor levels to integer factor (i.e. 1:n_bins)
		levels(z$time_bin_2) <- 1:length(levels(z$time_bin_2))
		z$time_bin_2 <- as.integer(z$time_bin_2)
		names(z)[3] <- "time_bin"
	#thigmotaxis discrete: %age time outer vs. inner arena
	} else if(var_name == "perimeter") {
		z <- ddply(dat, .(id, treat, time_bin), .fun = function(x) median(length(na.omit(x[,var_name][x[,var_name]==1])) / length(na.omit(x[,var_name])), na.rm=T)
		)
	#thigmotaxis continuous: perimeter distance
	} else if(var_name == "perimeter_dist") {
		z <- ddply(dat, .(id, treat, time_bin), .fun = function(x) median(x[,"perimeter_dist"], na.rm=T)
		)
	#turning angle
	} else if(var_name == "turning") {
		z <- ddply(dat, .(id, treat, time_bin), .fun = function(x) median(abs(x[,"relAngle"]), na.rm=T)
		)
	#meander
	} else if(var_name == "meander") {
		ss <- subset(dat, Distance > 0.1)
		z <- ddply(ss, .(id, treat, time_bin), .fun = function(x) median(abs(x[,"relAngle"]) / x[,"Distance"], na.rm=T)
		)
	#meander per minute
	} else if(var_name == "meander_per_min") {
		ss <- dat
		mins <- ceiling(max(ss$time) / 1000 / 60)
		#first time bin (per minute)
		ss$time_bin_1 <- cut(ss$time, breaks = mins, labels=FALSE)
		ss <- subset(ss, Distance > 0.1)
		z <- ddply(ss, .(id, treat, time_bin_1), .fun = function(x) median(abs(x[,"relAngle"]) / x[,"Distance"], na.rm=T)
		)
		#second time bins (per x minutes)
		ifelse(n_bins > 1, z$time_bin_2 <- cut(z$time_bin_1, breaks = n_bins, include.lowest=T), z$time_bin_2 <- rep(1, nrow(z)) )
		z <- ddply(z, .(id, treat, time_bin_2), .fun = function(x) median(x[,"V1"], na.rm=T)
		)
		#convert time_bin_2 factor levels to integer factor (i.e. 1:n_bins)
		levels(z$time_bin_2) <- 1:length(levels(z$time_bin_2))
		z$time_bin_2 <- as.integer(z$time_bin_2)
		names(z)[3] <- "time_bin"
	#turnarounds 
	} else if(var_name == "turnarounds") {
		ss <- na.omit(subset(dat, sqrt(relAngle^2) > 165))
		z <- ddply(ss, .(id, treat, time_bin), .fun = function(x) nrow(x)
		)
	#moving speed
	} else if(var_name == "mobile_speed") {
		ss <- subset(dat, speed_raw > min_speed)
		z <- ddply(ss, .(id, treat, time_bin), .fun = function(x) median(x[,"speed_raw"], na.rm=T)
		)
	#moving speed smooth
	} else if(var_name == "mobile_speed_smooth") {
		ss <- subset(dat, speed_smooth > min_speed)
		z <- ddply(ss, .(id, treat, time_bin), .fun = function(x) median(x[,"speed_smooth"], na.rm=T)
		)
	
	##other simple continuous variables
	} else
		z <- ddply(dat, .(id, treat, time_bin), .fun = function(x) median(x[,var_name], na.rm=T)
		)
	
	if(any(!is.na(infer_zero_treatments))) {
		for(i in 1:length(infer_zero_treatments)) {
			new_id <- rep(max(dat$id) + i, n_bins)
			new_treat <- rep(infer_zero_treatments[i], n_bins)
			new_time_bin <- 1:n_bins
			new_value <- rep(0, n_bins)
			ss_df <- data.frame(id = new_id, treat = new_treat, time_bin = new_time_bin, V1 = new_value)

			z <- rbind(z, ss_df)
		}
	}
	
	#mean of medians
	if(means==TRUE) {
		z <- ddply(z, .(treat, time_bin), .fun = function(x) c(mean(x[,"V1"], na.rm=T), std.error(x[,"V1"], na.rm=T), length(x[,"V1"])))
		names(z)[2:5] <- c("time_bin", "mean", "SE", "n")
	}
	
	return(z)
}
