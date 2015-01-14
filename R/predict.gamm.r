#'	predict method for gamm object.
#'
#'	This is a predict method for \emph{gamm} object.
#'	This function is an internal function and not intended to be used directly 
#'	by users. This function internally pass the arguments to the
#'	\emph{predict.gam} using \emph{gam} object in \emph{gamm} object.
#'
#-------------------------------------------------------------------------------
#	gammオブジェクト用のpredict関数。内部的にpredict.gamを呼び出す。
#
#	Args:
#		predict.gamを見てください。
#-------------------------------------------------------------------------------
predict.gamm <- function(
	object, newdata, type = "link", se.fit = FALSE, terms = NULL, 
	block.size = NULL, newdata.guaranteed = FALSE, na.action = na.pass, 
	unconditional = FALSE, ...
){
	result <- predict(
		object$gam, newdata, type, se.fit, terms, block.size,
		newdata.guaranteed, na.action, unconditional, ...
	)
	return(result)
}