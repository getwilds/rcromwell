<!-- README.md is generated from README.Rmd. Please edit that file -->



# rcromwell

<!-- badges: start -->
[![Project Status: Experimental – Useable, some support, not open to feedback, unstable API.](https://getwilds.github.io/badges/badges/experimental.svg)](https://getwilds.github.io/badges/#experimental)
[![R-CMD-check](https://github.com/getwilds/rcromwell/actions/workflows/R-CMD-check.yaml/badge.svg?branch=dev)](https://github.com/getwilds/rcromwell/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

Convenience Tools for Managing WDL Workflows via Cromwell

## Installation

You can install the development version of `rcromwell` from [GitHub](https://github.com/) with:

```r
# install.packages("remotes")
remotes::install_github("getwilds/rcromwell")
```

Install a specific release version (in this case v1.0) by:

```r
remotes::install_github('getwilds/rcromwell@v1.0')
```

## Set up your Cromwell Server

Use instructions over in the [diy-cromwell-server repo](https://github.com/FredHutch/diy-cromwell-server) to get the configuration files needed and some testing workflows.


## Example workflow process

Set your Cromwell URL

```r
cromwell_config("http://gizmoXXX:20202")
```

### Validate your workflow formatting

```r
list.files(pattern = "*.wdl")
valid <- cromwell_validate(WDL = "myworkflow.wdl")
valid[["errors"]]
```

### Go fix your issues, now send your workflow to Crowmell

```r
thisJob <- cromwell_submit_batch(WDL = "myworkflow.wdl",
                    Params = "myworkflow-parameters.json",
                    Batch = "myworkflow-batch.json",
                    Options = "workflow-options.json")
(thisOne <- thisJob$id)
```

`thisJob$id` is the unique Cromwell ID for your entire workflow - you can use that to request all sorts of metadata!!!

### Now get all your metadata and track the workflow!!

Return a data frame of all jobs run in the past number of days (uses your database)

```r
cromwell_jobs(days = 2)
```

Return a data frame (one line if you only submit one workflow id) containing workflow level metadata

```r
w <- cromwell_workflow(thisOne)
```

Print the current status of the workflow(s) is(are)

```r
w$status
```

Return a data frame containing all call level metadata

```r
df <- cromwell_call(thisOne)
```

Handy set of dplyr commands to tell you about how the various calls are doing

```r
df %>%
  group_by(callName, executionStatus) %>%
  summarize(status = n()) %>%
  arrange(executionStatus)
```

Returns a data frame containing call level call caching  metadata

```r
ca <- cromwell_cache(thisOne)
```

Handy set of dplyr commands to tell you about what sort of call caching is happening

```r
ca %>%
  group_by(callCaching.hit, callName) %>%
  summarize(hits = n())
```

Opens up a popup in your browser with a timing diagram in it.

```r
cromwell_timing(thisOne)
```

Returns a data frame containing call level failure metadata

```r
cromwell_failures(thisOne)
```

Will tell Cromwell to abort the current workflow - note this cannot be undone and it will take a while to stop all the jobs.

```r
cromwell_abort(thisOne)
```

When a workflow is done, request information about the workflow outputs.

```r
cromwell_outputs(thisOne)
```

### Misc stuff

Ugly list of raw metadata should you need it for workflow troubleshooting

```r
wtf <- cromwell_glob(thisOne)
wtf[["failures"]]
```
