#-------------------------------------------------------------------------------
#'	Check an object is formula.
#'
#'	@param x an object.
#'	@return returns TRUE if \emph{x} is formula otherwise returns FALSE.
#-------------------------------------------------------------------------------
#	�ϐ���formula���𒲂ׂ�B
#
#	Args:
#		x: �ϐ��B
#-------------------------------------------------------------------------------
is.formula <- function(x){
	return(is(x, "formula"))
}


