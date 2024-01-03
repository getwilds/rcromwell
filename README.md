# rcromwell

R package for using Cromwell with WDL workflows.


## Install from GitHub

Install the most recent version of `rcromwell`:

```r
require(remotes)
remotes::install_github('getwilds/rcromwell')
```

Install a specific release version (in this case v1.0) by:
```r
require(remotes)
remotes::install_github('getwilds/rcromwell@v1.0')
```

## Set up your Cromwell Server

Use instructions over in the [diy-cromwell-server repo](https://github.com/FredHutch/diy-cromwell-server) to get the configuration files needed and some testing workflows.  


## Example workflow process

```{r}
## Set your Cromwell URL
setCromwellURL(nodeAndPort = "gizmoXXX:20202")
```

### Validate your workflow formatting

```{r}
list.files(pattern = "*.wdl")
valid <- cromwellValidate(WDL = "myworkflow.wdl"); valid[["errors"]]
```
## Go fix your issues, now send your workflow to Crowmell

```{r}
thisJob <- cromwellSubmitBatch(WDL = "myworkflow.wdl",
                    Params = "myworkflow-parameters.json",
                    Batch = "myworkflow-batch.json",
                    Options = "workflow-options.json")

# thisJob$id is now the unique Cromwell ID for your entire workflow - you can use that to request all sorts of metadata!!!
thisOne<- thisJob$id; thisOne
```

## Now get all your metadata and track the workflow!!

```{r}
# Returns a data frame of all jobs run in the past number of days (uses your database)
jobs <- cromwellJobs(days = 2)

# Returns a data frame (one line if you only submit one workflow id) containing workflow level metadata
w <- cromwellWorkflow(thisOne)

# This is handy to print the current status of the workflow(s) is(are)
w$status

# Returns a data frame containing all call level metadata
c <- cromwellCall(thisOne)

# Handy set of dplyr commands to tell you about how the various calls are doing
c %>% group_by(callName, executionStatus) %>% summarize(status = n()) %>% arrange(executionStatus)

# Returns a data frame containing call level call caching  metadata
ca <- cromwellCache(thisOne)

# Handy set of dplyr commands to tell you about what sort of call caching is happening
ca %>% group_by(callCaching.hit, callName) %>% summarize(hits = n())

# Opens up a popup in your browser with a timing diagram in it.
cromwellTiming(thisOne)

# Returns a data frame containing call level failure metadata
f <- cromwellFailures(thisOne)

# Will tell Cromwell to abort the current workflow - note this cannot be undone and it will take a while to stop all the jobs.  
abort <- cromwellAbort(thisOne)

# When a workflow is done, request information about the workflow outputs.
out <- cromwellOutputs(thisOne)
```

## Misc stuff

```{r}
# Ugly list of raw metadata should you need it for workflow troubleshooting
WTF <- cromwellGlob(thisOne); WTF[["failures"]]
```
