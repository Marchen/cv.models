#' Supporting generic function which modify 
#'
#' This function calcuate effective bust cap size ratio of your idol
#' when you can know her cap size.
#'
#' @param object idol object
#' @keywords bust
#' @export
#' @examples
#' my.idol     <- new("Idol")
#' my.idol.ecr <- effective.capsize(my.idol)
#-------------------------------------------------------------------------------
#	モデルが性能指標を正しく計算するように、モデル構築の引数を修正する総称関数。
#
#	Args:
#		object: オブジェクト。計算に使わないのでダミーでOK。
#		args.model: モデル構築に使われる引数。
#-------------------------------------------------------------------------------

modify.args.model <- function(object, args.model){
	UseMethod("modify.args.model")
}
# default S3 method
modify.args.model.default <- function(object, args.model){
	return(args.model)
}
# svm
# 確率を返すように挙動を変更する。
modify.args.model.svm <- function(object, args.model){
	args.model$probability = TRUE
	return(args.model)
}


