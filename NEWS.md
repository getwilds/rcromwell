# rcromwell 3.3.0

* Swapping `httr` for `httr2` and fixing styling (@sckott in [#48](https://github.com/getwilds/rcromwell/pull/48))

# rcromwell 3.2.5

* gains new function `cromwell_labels` that hits the `/labels` route (#43) (#45)
* remove comment about failures not working (#35)
* add prior art section to readme with one entry (#30)
* reorder output columns of `cromwell_workflow` and always return tibble for `cromwell_call` (#42) (#46)
* better links in readme to docs (#41)

# rcromwell 3.2.1

* fix `cromwell_submit_batch` - internally changed `workflow_options` to `workflowOptions` (#32)

# rcromwell 3.2.0

* rework all functions that interact with a cromwell server to allow for token and the server URL to be passed in as parameters to the function - this allows for an easier shiny flow when it's not a good idea to save these values as env vars (but does make sense for a single user, which is still supported) (#29)

# rcromwell 3.0.0

* First version of this package under the new name `rcromwell` (#17)
* Functions and functions arguments changed from camelCase to snake_case
* Package documentation added (#24)
* refactor http internals (#15)
* Add Scott as maintainer and as an author in DESC file (#23)
* Clean up README (#22)
* Reference current release version in dockerfile when release happens (#19)
* Change license to MIT (#14)
* Added tests (#11)
