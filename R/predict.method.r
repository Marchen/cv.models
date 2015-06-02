#-------------------------------------------------------------------------------
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
#	warning("Prediction will use the unconditional (population-level) values and random effects are ignored.")
	result <- predict(
		object$gam, newdata, type, se.fit, terms, block.size,
		newdata.guaranteed, na.action, unconditional, ...
	)
	return(result)
}


#-------------------------------------------------------------------------------
#'	predict method for glmmML object.
#'
#'	This is a predict method for \emph{glmmML} object.
#'	This function is an internal function and not intended to be used directly 
#'	by users. Prediction is made using unconditional values and random effect
#'	is ignored.
#'
#'	@param object a glmmML object.
#'	@param newdata a data.frame containing data used for prediction.
#'	@param type
#'		type of prediction. Default is scale of response variable.
#'		If "link" is specified, prediction is of link scale is calculated.
#'	@param ... further arguments passed to other methods.
#-------------------------------------------------------------------------------
#	glmmML用のpredictメソッド。
#
#	Args:
#		object: glmmMLオブジェクト。
#		newdata: 予測に使うデータが入ったデータフレーム。
#		type:
#			予測を計算するスケール。デフォルトでは応答変数のスケールで予測を計算
#			する。"link"を指定すると、リンク関数のスケールで予測を計算する。
#		...: 他のメソッドに渡される引数。
#-------------------------------------------------------------------------------
predict.glmmML <- function(object, newdata, type = c("response", "link"), ...){
	type = match.arg(type)
#	warning("Prediction will use the unconditional (population-level) values and random effects are ignored.")
	design.matrix <- model.matrix(object, data = newdata)
	result <- design.matrix %*% coef(object)
	if (type == "link"){
		return(result)
	} else {
		family <- format.family(object$call$family, type = "family")
		return(family$linkinv(result))
	}
}

