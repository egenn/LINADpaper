# LINAD Experiments PMLB Data
# 2019 Efstathios D. Gennatas egenn.github.io

# Libraries ====
remotes::install_github("egenn/rtemis")
library(rtemis)
rtInitProjectDir()

# Data ====
# Local path to cloned PMLB GitHub repository: Regression datasets
basedir <- "/Data/penn-ml-benchmarks/datasets/regression/"

dir(basedir)
all.dirs <- list.dirs(basedir, recursive = FALSE)
.names <- basename(all.dirs)
all.files <- Sys.glob(file.path(all.dirs, "*.tsv.gz"))
datl <- vector("list", length(all.dirs))

# '- Read datasets ====
for (i in seq_along(datl)) {
  cat("   Reading dataset ", i, " of ", length(datl), "...\n", sep = "")
  datl[[i]] <- as.data.frame(data.table::fread(all.files[i]))
}
names(datl) <- .names

# '- Select datasets with 200 <=N cases <= 1000 ====
all(sapply(datl, function(i) rev(colnames(i))[1] == "target"))
dat_dim <- t(sapply(datl, dim))
index <- which(dat_dim[, 1] >= 200 & dat_dim[, 1] <= 1000)
datls <- vector("list", length(index))
for (i in seq_along(index)) datls[[i]] <- datl[[index[i]]]
names(datls) <- .names[index]

# '- Save datasets to .rds ====
saveRDS(datls, "./Data/PMLB_Regression_200-1000cases.rds")
