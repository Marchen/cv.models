#===============================================================================
#	Utility functions used for testing.
#===============================================================================

#-------------------------------------------------------------------------------
#	Compare result of cross validation.
#-------------------------------------------------------------------------------
has.same.results <- function(x, y) {
	cv.results.x <- x$cv.results[names(x$cv.results) != "call"]
	cv.results.y <- x$cv.results[names(x$cv.results) != "call"]
	return(identical(cv.results.x, cv.results.y))
}
