getNullAreas <- function(dat) {

	l <- rapply(dat, function(x) return(c(attributes(x)$filename, attributes(x)$Area, attributes(x)$tags.hasEnoughPoints)), how="list")

	l <- unlist(l, recursive=F)

	l <- as.data.frame(do.call("rbind", l), row.names=F)

	names(l) <- c("filename", "area", "movement_detected")
	
	l
}