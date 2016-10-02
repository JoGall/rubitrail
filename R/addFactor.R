addFactor <- function(dat, new_factor, new_factor_name) {
	if(class(dat) == "list") {
		for(i in names(dat)) {
			#extend short vector of variables to fit full data
			rep_times <- rle(dat[[i]]$id)$lengths
			new_factor_full <- rep(new_factor, times=rep_times)
			#append to data
			dat[[i]]$temp <- new_factor_full
			colnames(dat[[i]])[colnames(dat[[i]]) == "temp"] <- new_factor_name
		}
	}
	else {
		#extend short vector of variables to fit full data
		rep_times <- rle(dat$id)$lengths
		new_factor_full <- rep(new_factor, times=rep_times)
		#append to data
		dat$temp <- new_factor_full
		colnames(dat)[colnames(dat) == "temp"] <- new_factor_name
	}
	dat
}

addID <- function(dat) {
	if(class(dat) == "list") {
		for(i in names(dat)) {
			dat[[i]] <- transform(dat[[i]], id = as.numeric(interaction(dat[[i]]$filename, dat[[i]]$area)))
		}
	} 
	else {
		dat <- transform(dat, id = as.numeric(interaction(dat$filename, dat$area)))
	}
	
	dat
}

printID <- function(dat) {
	
	global_id <- data.frame()
	
	if(class(dat) == "list") {
		for(i in unique(dat[[1]]$id)) {
			ss <- subset(dat[[1]], id == i)
			ss_id <- data.frame(id = unique(ss$id), area = unique(ss$area), filename = unique(ss$filename))
			global_id <- rbind(global_id, ss_id)
		}
	}
	else {
		for(i in unique(dat$id)) {
			ss <- subset(dat, id == i)
			ss_id <- data.frame(id = unique(ss$id), area = unique(ss$area), filename = unique(ss$filename))
			global_id <- rbind(global_id, ss_id)
		}
	}
	
	global_id
}