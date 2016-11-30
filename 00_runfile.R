
library(feather)


## clean out any previous work
paths <- c("fp-jtbeyond/data", "fp-jtbeyond/graph")

for(path in paths){
  unlink(path, recursive = TRUE)    # delete folder and contents; "recursive" to further delete all the subfolder in the folder.
  dir.create(path)                  # create empty folder
}

## run my scripts
source("01_collect_data.R")
source("02_Chinese_segmentation.R")
source("03_analysis_1.R")
source("04_analysis_2.R")

#rmarkdown::render("07_report.Rmd", output_dir = "output") # to save the rmd file into a new folder "output" 
