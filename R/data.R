#' Microbiome data
#'
#' Clinical and microbiome data derived from "Microbiota-based model improves
#' the sensitivity of fecal immunochemical test for detecting colonic lesions"
#' by Baxter et al. (2016). These data represent a subset of 172 health
#' participants. `baxter_clinical` contains 8 clinical variables for each of the
#' participants: `sample_name`, `id`, `age`, `bmi`, `gender`, `height`,
#' `total_reads`, and `disease_state` (all `H` for healthy). `baxter_otu` has
#' 1,234 columns, where each columns represent an Operational Taxonomic Unit
#' (OTU). OTUs are species-like relationships among bacteria determined by
#' analyzing their RNA. The cells are logged counts for how often the OTU was
#' detected in a participant's stool sample. Each column name is a shorthand
#' name, e.g. `otu1`; you can find the true name of the OTU mapped in
#' `baxter_data_dictionary`. `baxter_family` and `baxter_genus` are also logged
#' counts but instead group OTUs at the family and genus level, respectively, a
#' common approach to reducing microbiome data. Likewise, the column names are
#' shorthands, which you can find mapped in `baxter_data_dictionary`.
#'
#' @format 5 data frames
#' @source Baxter et al. (2016) \doi{10.1186/s13073-016-0290-3}
#'
#' @name baxter_data
"baxter_clinical"

#' @rdname baxter_data
"baxter_otu"

#' @rdname baxter_data
"baxter_family"

#' @rdname baxter_data
"baxter_genus"

#' @rdname baxter_data
"baxter_data_dictionary"
