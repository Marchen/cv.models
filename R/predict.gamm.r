#'	predict method for gamm object.
#'
#-------------------------------------------------------------------------------
#	gamm�I�u�W�F�N�g�p��predict�֐��B�����I��predict.gam���Ăяo���B
#
#	Args:
#		predict.gam�����Ă��������B
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