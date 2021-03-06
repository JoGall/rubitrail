% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/linearInterpolate.R
\name{rubitLinearInterpolate}
\alias{rubitLinearInterpolate}
\title{Resamples and interpolates data to achieve regular time intervals, and trims and/or extends tracking data to fit within desired start and end timepoints.}
\usage{
rubitLinearInterpolate(m, hz = 30, start_at = NA, end_at = NA,
  adj_fps = NA, minRow = 11)
}
\arguments{
\item{m}{a numerical matrix corresponding to an area.}

\item{hz}{the desired resampling frequency, in Hz.}

\item{start_at, end_at}{the starting / ending times to interpolate and/or cut data to, in minutes.}

\item{adj_fps}{encodes a new framerate, in Hz.}

\item{minRow}{an integer defining the minimal number of reads. If less than minRow reads are present in m, the function returns an empty matrix.}
}
\value{
A trimmed and resampled numerical matrix based upon the input data. The attributes of the input matrix are copied to the new matrix.
}
\description{
X,Y-positions are returned at a regular time interval using linear interpolation. Data can be trimmed to fit within desired start and end timepoints, or new positions inferred for additional timepoints (before or after object tracking).
}
\note{
X,Y-positions are defined as 'NA' between the \code{start_at} time and the point of first object tracking. For terminally interpolated timepoints (i.e. between last object detection and the \code{end_at} time), the last known X,Y-position is repeated.
Re-encoding a new framerate with \code{adj_fps} can correct potential errors made during video recording and/or tracking analysis. Check that the value returned by \code{\link{calcFPS}} matches the calculated framerate of the original video (e.g. using the 'ffprobe' function in FFmpeg [\url{https://ffmpeg.org/}].
}
\examples{
data(weevils_raw)

### Interpolation before filtering
w1 <- lapply(weevils_raw, rubitLinearInterpolate, hz = 50)
plot(w1[['08']][,'X'] ~ w1[['08']][,'Y'], asp=1, type='l')

### Interpolation after filtering
w19 <- lapply(weevils_raw, rubitRemoveOutliers, p = 0.001)
w19 <- lapply(w19, rubitMedianFilter, k = 19)
w19 <- lapply(w19, rubitLinearInterpolate, hz = 50)
plot(w19[['08']][,'X'] ~ w15[['08']][,'Y'], asp=1, type='l')

}
\seealso{
\code{\link{rubitMedianFilter}} to smooth data before interpolation.
}
