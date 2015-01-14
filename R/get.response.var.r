

#-------------------------------------------------------------------------------
#'	Get response variable from parameters.
#'
#'	This internal function retrieves  the response variable in specified 
#'	parameters used for modeling.
#'
#'	@inheritParams modify.args.model
#'	@param data data used for modeling.
#-------------------------------------------------------------------------------
#	応答変数を返す。
#
#	Args:
#		object: モデルオブジェクト
#		data: モデル構築に使われるデータ。
#		args.model: モデル構築に使われるパラメーター。
#-------------------------------------------------------------------------------
get.response.var <- function(object, data, args.model){
	UseMethod("get.response.var")
}

#-------------------------------------------------------------------------------
#'	@describeIn get.response.var
#'	@method get.response.var default Default S3 method.
#-------------------------------------------------------------------------------
get.response.var.default <- function(object, data, args.model){
	return(data[[get.response.name(object, args.model)]])
}

