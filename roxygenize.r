require(roxygen2)

#-------------------------------------------------------------------------------
#	�X�N���v�g������f�B���N�g������Ԃ��֐��B
#	http://stackoverflow.com/questions/1815606/rscript-determine-path-of-the-executing-script
#-------------------------------------------------------------------------------
get.this.file.dir <- function(){
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

#detach("package:cvModels", unload = TRUE)
roxygenize(get.this.file.dir(), clean = TRUE)
