#rubitrail

rubitrail is a package to analyse the output of [UbiTrail](http://ubitrail.sourceforge.net/), an open-source software for tracking animal locomotion.

Used in combination, UbiTrail and rubitrail aim to provide a complete toolkit for quantifying the movement of individual animals. Documentation and tutorials on using the software are available [here](http://ubitrail.sourceforge.net/).

rubtrail's pre-processing steps allow for:
* trajectory smoothing
* outlier removal based upon log-likelihood of tracked positions
* linear interpolation of missing / occluded frames

And further analysis can return data on a range of behavioural metrics, including:
* speed and acceleration
* turning angle
* activity (frequency and duration of bursts / pauses)
* exploration (proportion of area traversed)
* thigmotaxis (centrophobism)

Install the latest version of this package from github:

    ```R
    require(devtools)
    devtools::install_github("jogall/rubitrail")
    ```
