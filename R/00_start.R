# Standard starting point. Run this first whenever data need to be checked:
#   1. Restart R (Session > Restart R in RStudio).
#   2. In the R Console type:  setwd("D:/SMCR-LAPER-code")
#   3. Then type:               source("R/00_start.R")
# After it finishes, the data (version 5) are in the object d and every check can start from there.

if (basename(getwd()) != "SMCR-LAPER-code") {
  stop("Wrong folder. Type setwd(\"D:/SMCR-LAPER-code\") in the Console, then run this script again.")
}
source("R/00_config.R")
source("R/helpers.R")

cat("Working folder :", getwd(), "\n")
cat("Files in data/ :\n"); print(list.files("data"))
if (!file.exists(DATA_FILE)) {
  stop("DATA_FILE not found: ", DATA_FILE,
       "\nOpen R/00_config.R and set DATA_FILE to one of the names listed above, for example \"data/<file name>.xlsx\".")
}
d <- read_smcr()
cat("Data file      :", DATA_FILE, "\n")
cat("Rows, columns  :", nrow(d), ",", ncol(d), "(version 5 should give 1081 and 285)\n")
cat("MD5 of the file:", unname(tools::md5sum(DATA_FILE)), "\n")
cat("Ready. The data are in the object d.\n")
