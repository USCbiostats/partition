## code to prepare `baxter_clinical` dataset goes here

options(stringsAsFactors = FALSE)
## Read in species level abundance data from Baxter
unzip("data-raw/crc_baxter.otu_table.100.denovo.rdp_assigned.zip", "data-raw/crc_baxter.otu_table.100.denovo.rdp_assigned")
flbx <- "data-raw/crc_baxter.otu_table.100.denovo.rdp_assigned"
otus.bx <- read.delim(file = flbx, header = TRUE, sep = "\t")
tot.reads <- apply(otus.bx[, -1], 2, sum, rm.na = TRUE)

# remove OTUs that do not have at least 10% non-zero values
thresh <- 0.1
fltr <- function(vec, scrn) {
  aa <- sum(vec > 0, na.rm = TRUE)
  ltmp <- TRUE
  if (aa < scrn) ltmp <- FALSE
  return(ltmp)
}
scrn <- floor(ncol(otus.bx[, -1]) * thresh)
ltmp <- apply(otus.bx[, -1], 1, fltr, scrn = scrn)
otus.bx1 <- otus.bx[ltmp, ]

# Read in patient demographic and clinical data
flbx1 <- "data-raw/crc_baxter.metadata.txt"
dat.bx <- read.delim(file = flbx1, header = TRUE, sep = "\t")
cv.nms <- c("Sample_Name_s", "Age_s", "BMI_s", "Gender_s", "Height_s", "total_reads", "DiseaseState")
dat.bx <- dat.bx[, cv.nms]

# format IDs to match OTU data
dat.bx[, "ID"] <- paste("X", dat.bx$Sample_Name_s, sep = "")

# reorder dat.bx to match OTU data
ind <- match(colnames(otus.bx1)[-1], dat.bx[, "ID"])
dat.bx1 <- dat.bx[ind, ]

# Filter out non-healthy patients
aa <- dat.bx1$DiseaseState == "H"
dat.bx2 <- dat.bx1[aa, ]
otus.bx2 <- otus.bx1[, c(TRUE, aa)]

# scale OTU counts by total reads
tscl <- mean(dat.bx2$total_reads) / dat.bx2$total_reads
otus.bx3 <- otus.bx2
sfun <- function(vec, tscl) {
  tmp <- ceiling(vec * tscl)
  return(tmp)
}
tmp <- apply(otus.bx2[, -1], 1, sfun, tscl = tscl)
otus.bx3[, -1] <- t(tmp)

# transpose OTU data so OTUs are columns
otus.bx4 <- t(otus.bx3[, -1])
colnames(otus.bx4) <- otus.bx3$X

# summarize OTU data by taxonomic rank, family
rnks <- colnames(otus.bx4)
rind <- regexpr("g__", rnks, fixed = TRUE) - 2
rnks1 <- substr(rnks, 1, rind)

# identify and remove summary ranks ending in f__
ind <- grep("*f__$", rnks1)
rnks2 <- rnks1[-ind]
otus.bx5 <- otus.bx4[, -ind]

# sum over known families
frnks <- unique(rnks2)
otus.bxf <- as.data.frame(matrix(NA, nrow = nrow(otus.bx5), ncol = length(frnks)))
names(otus.bxf) <- frnks
for (j in 1:length(frnks)) {
  tmp <- as.matrix(otus.bx5[, is.element(rnks2, frnks[j])])
  otus.bxf[, frnks[j]] <- apply(tmp, 1, sum, na.rm = TRUE)
} # End j loop

# summarize OTU data by taxonomic rank, genus
rnks <- colnames(otus.bx4)
rind <- regexpr("s__", rnks, fixed = TRUE) - 2
rnks1 <- substr(rnks, 1, rind)

# identify and remove summary ranks ending in g__
ind <- grep("*g__$", rnks1)
rnks2 <- rnks1[-ind]
otus.bx5 <- otus.bx4[, -ind]

# sum over known genus
grnks <- unique(rnks2)
otus.bxg <- as.data.frame(matrix(NA, nrow = nrow(otus.bx5), ncol = length(grnks)))
names(otus.bxg) <- grnks
for (j in 1:length(grnks)) {
  tmp <- as.matrix(otus.bx5[, is.element(rnks2, grnks[j])])
  otus.bxg[, grnks[j]] <- apply(tmp, 1, sum, na.rm = TRUE)
} # End j loop

dat.bx2[, "BMI"] <- as.numeric(dat.bx2[, "BMI_s"])
dat.bx2[, "age"] <- as.numeric(dat.bx2[, "Age_s"])

# log transform OTU counts
otus.bx4l <- log(1 + otus.bx4)
otus.bxfl <- log(1 + otus.bxf)
otus.bxgl <- log(1 + otus.bxg)

baxter_clinical <- tibble::as_tibble(dat.bx2[, c(1, 8, 9:10, 4:7)])

names(baxter_clinical) <- c(
  "sample_name",
  "id",
  "age",
  "bmi",
  "gender",
  "height",
  "total_reads",
  "disease_state"
)

otu_names <- colnames(otus.bx4l)
otu_coulmns <- paste0("otu_", seq_along(otu_names))
baxter_otu <- tibble::as_tibble(otus.bx4l)
names(baxter_otu) <- otu_coulmns

family_names <- colnames(otus.bxfl)
family_coulmns <- paste0("family_", seq_along(family_names))
baxter_family <- tibble::as_tibble(otus.bxfl)
names(baxter_family) <- family_coulmns

genus_names <- colnames(otus.bxgl)
genus_coulmns <- paste0("genus_", seq_along(genus_names))
baxter_genus <- tibble::as_tibble(otus.bxgl)
names(baxter_genus) <- genus_coulmns

datasets <- c(
  rep("baxter_otu", length(baxter_otu)),
  rep("baxter_genus", length(baxter_genus)),
  rep("baxter_family", length(baxter_family))
)

baxter_data_dictionary <- tibble::tibble(
  dataset = datasets,
  column = c(otu_coulmns, genus_coulmns, family_coulmns),
  name = c(otu_names, genus_names, family_names)
)


usethis::use_data(baxter_clinical, overwrite = TRUE)
usethis::use_data(baxter_otu, overwrite = TRUE)
usethis::use_data(baxter_family, overwrite = TRUE)
usethis::use_data(baxter_genus, overwrite = TRUE)
usethis::use_data(baxter_data_dictionary, overwrite = TRUE)
