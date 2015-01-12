#'	predict method for gamm object.
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