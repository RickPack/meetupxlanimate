#' Example data for Research Triangle Analysts and
#' Triangle SQL Server User Group (TriPASS)

# Technique from https://grasshoppermouse.github.io/2017/10/18/put-your-data-in-an-r-package/
rta_data     <- readRDS("data-raw/RTA_meetupxl.rds")
tripass_data <- readRDS("data-raw/tripass_meetupxl.rds")
usethis::use_data(rta_data, tripass_data, overwrite = T)

######################
##    Created by    ##
######################
# rta_data           <- meetupxlanimate("Research-Triangle-Analysts")
# tripass_data       <- meetupxlanimate("tripass")
