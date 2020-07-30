# LINAD PMLB Regression Experiments
# 2020 Efstathios D. Gennatas MBBS AICSM PhD egenn.lambdamd.org

# Libraries ====
remotes::install_github("egenn/rtemis")
library(rtemis)

# Data ====
# Assume a "Data" directory under working directory containing PMLB data
reg <- readRDS("./Data/PMLB_Regression_200-1000cases.rds")
setnames <- names(reg)

# GLMNET ====
glmnet.error <- vector("list", length(reg))
outdir <- "./Results/Regression/GLMNET/"
dir.create(outdir, recursive = TRUE)

for (i in seq_along(reg)) {
  name <- setnames[i]
  mod <- elevate(reg[[i]], mod = "glmnet",
                 mod.params = list(alpha = seq(0, 1, .1)),
                 resampler = "strat.sub",
                 n.resamples = 20,
                 seed = 2019,
                 outdir = paste0(outdir, name))
  glmnet.error[[i]]$train <- mod$error.train.repeats.mean
  glmnet.error[[i]]$test <- mod$error.test.repeats.mean
}
saveRDS(glmnet.error, "./Results/Regression/glmnet.error.rds")

# CART ====
cart.error <- vector("list", length(reg))
outdir <- "./Results/Regression/CART/"
dir.create(outdir, recursive = TRUE)

for (i in seq_along(reg)) {
  name <- setnames[i]
  mod <- elevate(reg[[i]], mod = "cart",
                 mod.params = list(maxdepth = 20,
                                   cp = 0,
                                   prune.cp = c(0, .001, .01, .1),
                                   minsplit = 2,
                                   minbucket = 1),
                 resampler = "strat.sub",
                 n.resamples = 20,
                 seed = 2019,
                 outdir = paste0(outdir, name))
  cart.error[[i]]$train <- mod$error.train.repeats.mean
  cart.error[[i]]$test <- mod$error.test.repeats.mean
}
saveRDS(cart.error, "./Results/Regression/cart.error.rds")

# RF ====
rf.error <- vector("list", length(reg))
outdir <- "./Results/Regression/RF/"
dir.create(outdir, recursive = TRUE)

for (i in seq_along(reg)) {
  name <- setnames[i]
  n.features <- NCOL(reg[[i]]) - 1
  mtry <- unique(c(round(.25 * n.features), round(.5 * n.features), round(.75 * n.features), n.features))
  mod <- elevate(reg[[i]], mod = "ranger",
                 mod.params = list(n.trees = 1000, mtry = mtry),
                 resampler = "strat.sub",
                 n.resamples = 20,
                 seed = 2019,
                 outdir = paste0(outdir, name))
  rf.error[[i]]$train <- mod$error.train.repeats.mean
  rf.error[[i]]$test <- mod$error.test.repeats.mean
}
saveRDS(rf.error, "./Results/Regression/rf.error.rds")

# GBM ====
gbm.error <- vector("list", length(reg))
outdir <- "./Results/Regression/GBM/"
dir.create(outdir, recursive = TRUE)

for (i in seq_along(reg)) {
  name <- setnames[i]
  mod <- elevate(reg[[i]], mod = "gbm3",
                 mod.params = list(shrinkage = c(.01, .1),
                                   interaction.depth = c(1, 3, 5, 7)),
                 resampler = "strat.sub",
                 n.resamples = 20,
                 seed = 2019,
                 outdir = paste0(outdir, name))
  gbm.error[[i]]$train <- mod$error.train.repeats.mean
  gbm.error[[i]]$test <- mod$error.test.repeats.mean
}
saveRDS(gbm.error, "./Results/Regression/gbm.error.rds")

# LINAD ====
shytree_6.error <- vector("list", length(reg))
outdir <- "./Results/Regression/SHYTREE_6/"
dir.create(outdir, recursive = TRUE)

for (i in seq_along(reg)) {
  name <- setnames[i]
  mod <- elevate(reg[[i]], mod = "shytree",
                 lin.type = "forwardStepwise",
                 mod.params = list(n.cores = 40),
                 nvmax = c(2, 4, 6),
                 gamma = c(0, .1),
                 learning.rate = c(.1, .5),
                 max.leaves = 50,
                 resampler = "strat.sub",
                 n.resamples = 20,
                 seed = 2019,
                 outdir = paste0(outdir, name))
  shytree_6.error[[i]]$train <- mod$error.train.repeats.mean
  shytree_6.error[[i]]$test <- mod$error.test.repeats.mean
}
saveRDS(shytree_6.error, "./Results/Regression/shytree_6.error.rds")

# LINADbag ====
bagshytree.error <- vector("list", length(reg))
outdir <- "./Results/Regression/BAGSHYTREE/"
dir.create(outdir, recursive = TRUE)

for (i in seq_along(reg)) {
  name <- setnames[i]
  mod <- elevate(reg[[i]], mod = "bag",
                 lin.type = "forwardStepwise",
                 mod.params = list(mod = "shytree",
                                   k = 50,
                                   force.max.leaves = 20,
                                   learning.rate = 1),
                 resampler = "strat.sub",
                 n.resamples = 20,
                 seed = 2019,
                 outdir = paste0(outdir, name))
  bagshytree.error[[i]]$train <- mod$error.train.repeats.mean
  bagshytree.error[[i]]$test <- mod$error.test.repeats.mean
}
saveRDS(bagshytree.error, "./Results/Regression/bagshytree.error.rds")

# LINADboost ====
# This used hytreenow
hytboost_1.error <- vector("list", length(reg))
outdir <- "./Results/Regression/HYTBOOST_1/"
dir.create(outdir, recursive = TRUE)

for (i in seq_along(reg)) {
  name <- setnames[i]
  mod <- elevate(reg[[i]], mod = "hytboost",
                 lin.type = "glmnet",
                 alpha = 1,
                 lambda = .01,
                 max.depth = c(1, 2, 3),
                 learning.rate = c(.05, .1),
                 max.iter = 500,
                 earlystop.params = rtset.earlystop(window = 10,
                                                    window_decrease_pct_min = .01),
                 resampler = "strat.sub",
                 n.resamples = 20,
                 seed = 2019,
                 outdir = paste0(outdir, name))
  hytboost_1.error[[i]]$train <- mod$error.train.repeats.mean
  hytboost_1.error[[i]]$test <- mod$error.test.repeats.mean
}
saveRDS(hytboost_1.error, "./Results/Regression/hytboost_1.error.rds")