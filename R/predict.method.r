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
#'
#'	@param object a glmmML object.
#'	@param newdata a data.frame containing data used for prediction.
#'	@param type
#'		type of prediction. Default is scale of response variable.
#'		If "link" is specified, prediction is of link scale is calculated.
#'	@param conditional
#		if FALSE, marginal (using fixed terms only and unconditioned to random 
#		effect) predicted values are calculated. If TRUE, predicted values 
#		conditioned to the random effect is calculated.
#'	@param ... further arguments passed to other methods.
#-------------------------------------------------------------------------------
#	glmmML用のpredictメソッド。
#
#	Args:
#		object:
#			glmmMLオブジェクト。
#		newdata:
#			予測に使うデータが入ったデータフレーム。
#		type:
#			予測を計算するスケール。
#			デフォルトでは応答変数のスケールで予測を計算
#			する。"link"を指定すると、リンク関数のスケールで予測を計算する。
#		conditional:
#			TRUEだとconditional（ランダム効果による切片の違いも考慮した）な
#			予測値を計算する。
#			FALSEだとmarginal（ランダム効果を無視した固定効果だけ）な予測値を
#			計算する。
#		...: 他のメソッドに渡される引数。
#-------------------------------------------------------------------------------
predict.glmmML <- function(
	object, newdata, type = c("response", "link"), conditional = FALSE, ...
){
	type = match.arg(type)
	design.matrix <- model.matrix(object, data = newdata)
	result <- design.matrix %*% coef(object)
	# ランダム効果を使う時にはランダム効果分の切片のずれを加算する。
	if (conditional){
		cluster <- eval(object$call$cluster, eval(object$call$data))
		result <- result + object$posterior.mode[as.numeric(cluster)]
	}
	if (type == "link"){
		return(result)
	} else {
		family <- format.family(object$call$family, type = "family")
		return(family$linkinv(result))
	}
}

