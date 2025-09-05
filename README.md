# EntoInsights

The **EntoInsights** R package provides a range of functions to support users of the Automated Monitoring of Insects (AMI) and LepiSense systems. Its functionality broadly assists users who wish to analyse classification datasets, as well as partners reviewing and retrieving their uploaded image and acoustic datasets.

The current functionality of the package allows users to:

* Clean UK-based classification records using the **record cleaner** function.
* Extract datetime information from filenames.
* Download and process snapshot images, audio, and ultrasound recordings (requires a credentials file).
* List deployments in the object store and retrieve their associated metadata (requires API username and password).
* List files uploaded to deployments (requires a credentials file).
* Extract environmental variables using the NASA POWER API and the R packages **moonlit** and **elevatr**.

## Installing the package

The package does not have any non-R dependencies and can be installed simply by running:

```r
# Install EntoInsights
devtools::install_github("AMI-system/EntoInsights")
library(EntoInsights)
```

For a more comprehensive description of the package functionality, users are encouraged to consult the vignette (HTML).
