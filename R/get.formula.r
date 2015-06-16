#-------------------------------------------------------------------------------
#'	Check an object is formula.
#'
#'	@param x an object.
#'	@return returns TRUE if \emph{x} is formula otherwise returns FALSE.
#-------------------------------------------------------------------------------
#	•Ï”‚ªformula‚©‚ğ’²‚×‚éB
#
#	Args:
#		x: •Ï”B
#-------------------------------------------------------------------------------
is.formula <- function(x){
	return(is(x, "formula"))
}


