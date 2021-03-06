RUbitrail
====

An R package to analyse the output of [Ubitrail](http://ubitrail.sourceforge.net/), an open-source software for tracking animal locomotion. Together, these aim to provide a complete toolkit for quantifying two-dimensional locomotory behaviours in animals. Documentation and tutorials on using the software are available [here](http://ubitrail.sourceforge.net/).

RUbtrail's pre-processing steps allow for:
* outlier removal based upon log-likelihood of tracked positions
* trajectory smoothing
* linear interpolation of missing / occluded frames

And further analysis can return data on a range of behavioural metrics, including:
* speed and acceleration
* turning angle
* activity (frequency and duration of bursts / pauses)
* exploration (proportion of area visited)
* thigmotaxis (centrophobism)

Install the latest version of this package from github:

```{r}
require(devtools)
devtools::install_github("jogall/rubitrail")
```
