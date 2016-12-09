
library(feather)


## clean out any previous work
paths <- c("data","data/rawdata", "data/segtext","graph")

for(path in paths){
  unlink(path, recursive = TRUE)    # delete folder and contents; "recursive" to further delete all the subfolder in the folder.
  dir.create(path)                  # create empty folder
}

## run my scripts
source("01_collect_data.R")
source("02_translation.R")
source("03_Chinese_segmentation.R")
source("04_analysis_1.R")
source("05_analysis_2.R")
source("06_analysis_3.R")

rmarkdown::render_site()
