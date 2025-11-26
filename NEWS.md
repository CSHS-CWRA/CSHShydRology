# CSHShydRology 1.4.4
* changes to hydrograph plotting functions, including adding `ch_gg_hydrographs` which creates **ggplot2** objects
* removal of function `ch_flow_raster_trend` as it used the package **Kendal**, which will be archived by CRAN

# CSHShydRology 1.4.3
* added functions ch_high_Grubbs_test and ch_ffa_screen_plot to identify outliers when fitting distributions to mean daily streamflows
* updated urls to specify https instead of http

# CSHShydRology 1.4.2
* minor fixes to return statements in `ch_tr_sign` and `ch_tr_signif`
* fixed some URLs which redirect to other locations
* removed functions `ch_get_map_base` and `ch_map_plot_data` as we could not trap the error resulting from accessing the server. We hope to be able to restore these functions in the future.

# CSHShydRology 1.2.9
* revised examples of functions using `tidyhydat` to use small test database to avoid issues with CRAN testing

# CSHShydRology 1.2.8
* changed several \donttest flags to \dontrun to prevent errors when running `check --as-cran --run-donttest`

# CSHShydRology 1.2.7
* all function examples using Whitebox now test for the presence of the Whitebox executable before running
  - this avoids the Whitebox functions returning an error when tested if there is no executable installed
  - allows us to remove the \donttest flag from several functions

# CSHShydRology 1.2.6
* fixed many issues causing problems with `check --as-cran`
* functions now fail gracefully attempting to download data if url is incorrect or file is missing
* improved package Description

# CSHShydRology 1.2.2
* Removed ch_get_AHCCD_monthly() as ECCC no longer allows direct website access

# CSHShydRology 1.2.0
* First version to be on CRAN

# CSHShydRology 1.1.0
* Renamed all functions to have the prefix "ch"

# CSHShydRology 1.0.0
* First official version
* Contains functions by Paul Whitfield and Rob Chlumsky
* Also contains vignettes

# CSHShydRology 0.0.1
* Added a `NEWS.md` file to track changes to the package.
* Added some contributing documentation
* added wtr_yr function



