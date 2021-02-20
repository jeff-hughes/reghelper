## Submission notes

This update includes a few bug fixes:

* Fixes errors in `beta()` and `simple_slopes()` resulting from factor variables
  that have spaces or other special characters
* Adds support for character vectors added to models, where R silently converts
  them to factors
* Fixes error in `graph_model()` documentation

## Test environments

* local Arch Linux install, R 4.0.3
* macOS Catalina 10.15 (Github CI runner), R 3.6.3, R 4.0.2
* Ubuntu 18.04 (Github CI runner), R 3.6.3, R 4.0.2
* win-builder (devel, release, and oldrelease)


## R CMD check results

There were no ERRORs, WARNINGs or NOTEs.

## Downstream dependencies

The updates do not change the API in any way, and should not impact downstream
dependencies.
