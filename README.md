# fh.wdlR
A repo containing a basic R package for using Cromwell with WDL workflows at Fred Hutch. (Contact Amy Paguirigan for help).


## Install from GitHub
You will need the following packages installed first:
```{r}
install.packages(pkgs = c("httr", "jsonlite", "magrittr",
                "dplyr", "ssh", "purrr", "paws", "tidyr"))
```

Then you can install the most recent version of `fh.wdlR` by:

```r
require(remotes)
remotes::install_github('FredHutch/fh.wdlR')
```

Install a specific release version (in this case v1.0) by:
```r
require(remotes)
remotes::install_github('FredHutch/fh.wdlR@v1.0')
```

## Set up your Cromwell Server
Use instructions over in the [diy-cromwell-server repo](https://github.com/FredHutch/diy-cromwell-server) to get the configuration files needed and some testing workflows.  

### Create your Cromwell server via R
Once you have set up your config files to a place in our filesystem where `rhino` can get to them (like your `home` directory or somehwere in `Fast`), then you can use the `cromwellCreate` function to set up a Cromwell server on `gizmo`.  

```{r}
library(fh.wdlR)
cromwellCreate(FredHutchId = "username", port = "2020",
        pathToServerLogs = "/home/username/cromwell/cromwell-serverlogs/%A.txt",
        pathToServerScript = "/home/username/cromwell/cromServer.sh",
        pathToParams = "/home/username/cromwell/cromwellParams.sh")
```

When you use the `cromwellCreate` function it will return the necessary information for using the API via a browser AND will automatically set your `CROMWELLURL` environment variable to the correct location for the remaining job submission and management functions in the R package.

### Set Credentials of an existing Cromwell server

If you have already set up a Cromwell server, you'll need the environment variable `CROMWELLURL` to be set to the URL of your current Cromwell server for the job submission and metadata management functions to work.  The URL is made up of the gizmo node it's running on and the port you specified in the config file (e.g. "http://gizmof11:2020").
```{r}
Sys.setenv(CROMWELLURL="yourcromwellserverURL")
```
