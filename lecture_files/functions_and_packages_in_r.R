#code for package creation and curation

# Load the libraries used below
library("devtools")
library("roxygen2")
library("knitr")


## Initialize the package (only have to do this once!)
#set wd (where do you want package folder to live?)
work_dir <-choose.dir()#should be in repository location!
setwd(work_dir)

## Create the package directory
create("examplepackage") #no odd characters allowed

#look at what you made (go to folder)

#lets make a function and document using knitR
#meannona.R is avaialable from me
#https://github.com/jsgosnell/CUNY-BioStats/blob/master/code_examples/creating_packages/meannona.R
#put this in your R folder

#document using knitr

#to update package

#navigate to package folder
package_name <- "examplepackage"
setwd(paste(getwd(), package_name, sep="/"))
#update documentation
document()
#this adds documents to you man folder or updates them
#used to make help files
?meannona


#see if it would pass CRAN checks
check()

#build it
#needs to be back in main package
build()

#to add other parts
use_vignette("my-vignette")

#testthat

#submit to CRAN, details @ http://r-pkgs.had.co.nz/release.html
#chekc for issues
devtools::build_win()
#send to CRAN
devtools::release()
#
