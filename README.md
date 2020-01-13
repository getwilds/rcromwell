# fh-wdlR
A repo containing a basic R package for using Cromwell with WDL workflows at Fred Hutch via R. (Contact Amy Paguirigan for help).


## Install from GitHub
You can install the most recent version of `fh.wdlR` by:

```r
require(remotes)
remotes::install_github('FredHutch/fh.wdlR')
```

Install a specific release version (in this case v1.0) by:
```r
require(remotes)
remotes::install_github('FredHutch/fh.wdlR@v1.0')
```

## Set Credentials:

You'll need the environment variable `CROMWELLURL` to be set to the URL of your current Cromwell server.
```{r}
Sys.setenv(CROMWELLURL="yourcromwellserverURL")
```
