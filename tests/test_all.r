#===============================================================================
#	Run all tests.
#
#	All test files:
#		*	are tested with setting working directory to the directory having
#			the files.
#		*	should be self-contained: should load all required packages
#			excluding cv.models and testthat.
#		*	should remove all objects the file created.
#===============================================================================
library(cv.models)
library(testthat)


#------------------------------------------------------------------------------
#	Change working directory to package directory.
#------------------------------------------------------------------------------
get.this.file.dir <- function() {
	cmdArgs <- commandArgs(trailingOnly = FALSE)
	needle <- "--file="
	match <- grep(needle, cmdArgs)
	if (length(match) > 0) {
		# Rscript
		return(dirname(sub(needle, "", cmdArgs[match])))
	} else {
		# 'source'd via R console
		return(dirname(normalizePath(sys.frames()[[1]]$ofile)))
	}
}

old.wd <- setwd(get.this.file.dir())


#------------------------------------------------------------------------------
#	Prepare test files. New test file(s) need to be added to here.
#------------------------------------------------------------------------------
test.files <- c(
	"test_types_of_call.r",
	"test_seed.r"
)


#------------------------------------------------------------------------------
#	Run all tests.
#------------------------------------------------------------------------------
run.all <- function(old.wd, files) {
	on.exit(setwd(old.wd))
	for (i in files) {
		test_file(i)
	}
}

run.all(old.wd, test.files)
rm(run.all)
