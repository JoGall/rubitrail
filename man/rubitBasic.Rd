% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/basic.R
\name{rubitBasic}
\alias{rubitBasic}
\title{Reads a raw UbiTrail result file and returns filtered X,Y-trajectories.}
\usage{
rubitBasic(FILE, scale = 1, hz = 30, start_at = NA, end_at = NA,
  adj_fps = NA, k = 15, p = 0.001, nmin = k * 10, a = 0, b = 0,
  c = 0, filterFUN = rubitMedianFilter,
  interpFUN = rubitLinearInterpolate, verbose = FALSE)
}
\arguments{
\item{FILE}{a .csv result file outputted by UbiTrail.}

\item{scale}{a numeric to calibrate the true spatial scale, in pixels per mm. At the default value, measurements are returned in pixels.}

\item{hz}{the frequency of resampling, in Hz. This argument is passed to the interpolation function.}

\item{start_at, end_at}{the desired start and end times to interpolate and/or cut data to, in minutes.}

\item{adj_fps}{encodes a new framerate, in Hz.}

\item{k}{the level of smoothing for raw trajectories. This argument is passed to the filter function.}

\item{p}{the threshold used to remove outliers, as a proportion of the overall likelihood distribution; e.g. for \code{p = 0.01}, the largest 1\% of outliers will be removed. See \code{\link{rubitRemoveOutliers}} for more information.}

\item{nmin}{the minimal number of reads. If not enough reads are presents in an area, an empty matrix is returned.}

\item{a, b, c}{parameters to correct lens distortion. See \code{\link{lensCorrection}} for more information.}

\item{filterFUN}{the filter function to be used. It must have the same arguments as \code{\link{rubitMedianFilter}}.}

\item{interpFUN}{the interpolation function to be used. It must have the same arguments as \code{\link{rubitLinearInterpolate}}.}

\item{verbose}{logical; if TRUE, the function will print messages at every step.}
}
\value{
A list of numerical matrices, with each matrix corresponding to an area. The attributes of list contain metadata about the original video file and attributes of each matrix contain information on the dimensions of the area.
}
\description{
This function smooths, interpolates and resamples X,Y-trajectories from raw tracking before calculating the distance between successive positions. These data can then be used with \code{\link{rubitMetrics}} to extract a range of behavioural metrics.
}
\note{
Re-encoding a new framerate with \code{adj_fps} can correct potential errors made during video recording and/or tracking analysis. Check that the value returned by \code{\link{calcFPS}} matches the calculated framerate of the original video (e.g. using the 'ffprobe' function in FFmpeg [\url{https://ffmpeg.org/}].
}
\examples{
### Read a single UbiTrail results file:
###----------------------------------------
## Locate raw data example including with package
FILE <- system.file("extdata", "tenebrio_ubitrail.csv.gz", package = "rubitrail")

tenebrio_basic <- rubitBasic(FILE, scale= 2.08, hz = 30, start_at = 0, end_at = 60, adj_fps = 19.05, k = 21, a = -0.005, b = -0.006, verbose = TRUE)

## See general metadata:
attributes(tenebrio_basic)

## See information on area '01':
attributes(tenebrio_basic[['01']])


### Read a list of UbiTrail results files:
###----------------------------------------
## Create a filelist of all UbiTrail results files in a directory:
# filelist <- list.files()

## Apply function over filelist:
lapply(filelist, rubitBasic, scale= 2.08, hz = 30, start_at = 0, end_at = 60, verbose = TRUE)

}
\seealso{
\code{\link{rubitMetrics}} for extracting behavioural metrics from processed trajectories outputted by this function.
\code{\link{rubitToDF}} converts the list (or list of lists) returned by this function into a dataframe for ease of further analysis.
See \code{\link{rubitLinearInterpolate}}, \code{\link{rubitMedianFilter}}, and \code{\link{rubitRemoveOutliers}} to understand the different steps of processing used in this function.
See code{\link{calcFPS}} for calculating the framerate of data and see \code{\link{lensCorrection}} for more information on lens distortion.
}
