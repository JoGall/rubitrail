#' @include basic.R
#' @include metrics.R
NULL
#' Flattens rubitrail lists into a single dataframe.
#'
#' A helper function which flattens a list, or a list of lists, returned by \code{\link{rubitBasic}} and \code{\link{rubitMetrics}} and converts into a dataframe for ease of further analysis.
#' 
#' @param LIST a list (single result file) or list of lists (multiple results files) returned by \code{\link{rubitBasic}} or \code{\link{rubitMetrics}}.
#' @param basic logical defining the type of data contained in \code{LIST}. Use FALSE for metrics data returned by \code{rubitMetrics} and TRUE for basic data returned by \code{\link{rubitBasic}}.
#' @return A single dataframe for 'basic' input data. A list of three dataframes for 'metrics' input data, with elements named 'speed', 'turning' and 'activity'.
#' @examples
#' data(tenebrio)
#'
#' rubitToDF(tenebrio, "basic")
#'
#' rubitToDF(tenebrio, "metrics")
#'
#' @seealso \code{\link{rubitBasic}} and \code{\link{rubitMetrics}} for information on processing raw trajectories and extracting metrics.
#' @export
rubitToDF <- function(LIST, basic = FALSE) {
	
	#data returned by rubitBasic  (i.e. one dataset for each area matrix)
	if(basic) {
		# Single list (one result file)
		if(class(LIST[[1]]) == "matrix") {
			filename <- attributes(LIST)$filename
			areas <- names(LIST)
			
			l_df <- data.frame()
			for(i in unique(areas)){
				ss_df <- as.data.frame(LIST[[i]])
				ss_df$filename <- rep(filename, nrow(ss_df))
				ss_df$area <- rep(i, nrow(ss_df))
				l_df <- rbind(l_df, ss_df)
			}
			return(l_df)		
		}
		# List of lists (multiple results files)
		else {
			ll_df <- data.frame()
			#loop for list of lists
			for(j in 1:length(LIST)) {
				filename <- attributes(LIST[[j]])$filename
				areas <- names(LIST[[j]])
				l_df <- data.frame()
				for(i in unique(areas)){
					ss_df <- as.data.frame(LIST[[j]][[i]])
					ss_df$filename <- rep(filename, nrow(ss_df))
					ss_df$area <- rep(i, nrow(ss_df))
					l_df <- rbind(l_df, ss_df)
				}
				ll_df <- rbind(ll_df, l_df)
			}
			return(ll_df)
		}
	}
	
	#data returned by rubitMetrics  (i.e. three datasets for each area matrix: 'speed', 'turning' and 'activity')
	else {
	
		# Single list (one result file)
		if(class(LIST[["speed"]][[1]]) == "matrix") {
			filename <- attributes(LIST[["speed"]])$filename
			areas <- names(LIST[["speed"]])
			
			#speed data
			l_speed <- data.frame()
			for(i in unique(areas)){
				ss_speed <- as.data.frame(LIST[["speed"]][[i]])
				ss_speed$filename <- rep(filename, nrow(ss_speed))
				ss_speed$area <- rep(i, nrow(ss_speed))
				l_speed <- rbind(l_speed, ss_speed)
			}
			#turning data
			l_turning <- data.frame()
			for(i in unique(areas)){
				ss_turning <- as.data.frame(LIST[["turning"]][[i]])
				ss_turning$filename <- rep(filename, nrow(ss_turning))
				ss_turning$area <- rep(i, nrow(ss_turning))
				l_turning <- rbind(l_turning, ss_turning)
			}
			#activity data
			l_activity <- data.frame()
			for(i in unique(areas)){
				ss_activity <- as.data.frame(LIST[["activity"]][[i]])
				ss_activity$filename <- rep(filename, nrow(ss_activity))
				ss_activity$area <- rep(i, nrow(ss_activity))
				l_activity <- rbind(l_activity, ss_activity)
			}
			return(list(speed = l_speed, turning = l_turning, activity = l_activity))
		}
		
		# List of lists (multiple results files)
		if(class(LIST[[1]][["speed"]][[1]]) == "matrix") {
			ll_speed <- data.frame()
			ll_turning <- data.frame()
			ll_activity <- data.frame()
			
			#loop for list of lists
			for(j in 1:length(LIST)) {
				filename <- attributes(LIST[[j]][["speed"]])$filename
				areas <- names(LIST[[j]][["speed"]])
				l_speed <- data.frame()
				l_turning <- data.frame()
				l_activity <- data.frame()
				
				for(i in unique(areas)){
					ss_speed <- as.data.frame(LIST[[j]][["speed"]][[i]])
					ss_speed$filename <- rep(filename, nrow(ss_speed))
					ss_speed$area <- rep(i, nrow(ss_speed))
					l_speed <- rbind(l_speed, ss_speed)
				
					ss_turning <- as.data.frame(LIST[[j]][["turning"]][[i]])
					ss_turning$filename <- rep(filename, nrow(ss_turning))
					ss_turning$area <- rep(i, nrow(ss_turning))
					l_turning <- rbind(l_turning, ss_turning)
				
					ss_activity <- as.data.frame(LIST[[j]][["activity"]][[i]])
					ss_activity$filename <- rep(filename, nrow(ss_activity))
					ss_activity$area <- rep(i, nrow(ss_activity))
					l_activity <- rbind(l_activity, ss_activity)
				}
				ll_speed <- rbind(ll_speed, l_speed)
				ll_turning <- rbind(ll_turning, l_turning)
				ll_activity <- rbind(ll_activity, l_activity)
			}
			return(list(speed = ll_speed, turning = ll_turning, activity = ll_activity))
		}
	}
}