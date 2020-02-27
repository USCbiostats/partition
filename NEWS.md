# partition (development version)

* Added microbiome data from [https://doi.org/10.1186/s13073-016-0290-3](https://doi.org/10.1186/s13073-016-0290-3) as 5 exported data sets: `baxter_clinical`, `baxter_otu`, `baxter_family`, `baxter_genus`, and `baxter_data_dictionary` (#16)
* fixed partial match warning in `measure_min_icc()` (b382caae)
* added an issue template

# partition 0.1.1

* added a contributor guideline and COC
* directly call `cols` in `tidyr::unnest()` to respect changes made in tidyr 1.0.0 (0cf1e1f)
* fixed bug to internalize docs for `increase_hits()`/`get_hits()` (9cab013)
* Added a `NEWS.md` file to track changes to the package.

# partition 0.1.0
* Initial release
