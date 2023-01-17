

# Gaussian Clustering 

suppressPackageStartupMessages({
library(mclust)
library(data.table)
})

# Reading arguments
args = commandArgs(trailingOnly = TRUE)
data_path = as.character(args[1])

# Loading in data
tt = readRDS(data_path)


# Running clustering
mm = Mclust(tt, G=2:50)
print(summary(mm))
#print(mm$BIC)


# Saving output
saveRDS(mm, "outputs/m50.rds")


# mm = Mclust(tt, G=1:10, modelNames = c("EII", "VII"))


