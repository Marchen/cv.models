#-------------------------------------------------------------------------------
#'	(Internal) Format and check consistency of family.
#'
#'	@param family family function, a character of family name, or family object.
#'	@param type
#'		a character to specify the type of the value this function returns.
#'		If "family", this function returns 'family' object. If "character" is
#'		specified, this function returns a character string denoting the name
#'		of the family.
#'
#'		This internal function is called by \code{link{detect.model.type}} and
#'		\code{\link{predict.glmmML}} functions.
#-------------------------------------------------------------------------------
#	familyのフォーマットを揃える。
#
#	Args:
#		family: familyオブジェクト、関数、文字列
#		type:
#			familyならfamilyオブジェクトを、characterならfamily名を表す文字列
#			を返す。
#-------------------------------------------------------------------------------
format.family <- function(family, type = c("family", "character")){
	type <- match.arg(type)
	if (is.character(family)){
		family <- get(family)
	}
	if (is.function(family)){
		family <- family()
	}
	if (is.null(family$family)){
		print(family)
		stop("`family' not recognized")
	}
	result <- switch(type, family = family, character = family$family)
	return(result)
}


