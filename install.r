#------------------------------------------------------------------------------
#	Change working directory to package directory
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
#	Install the package before compiling.
#------------------------------------------------------------------------------
system("Rscript -e 'library(devtools);install()'")

setwd(old.wd)
