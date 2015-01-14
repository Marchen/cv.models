#-------------------------------------------------------------------------------
#	モデルの構築やpredictに使う引数を入れたargsリストの中から、モデルの性能に
#	影響する候補パラメーターを取り出す総称関数。
#	モデルの構築だとgbmのshrinkageとかinteraction.depth、predictだとgbmのn.trees
#	みたいなのに対応するための関数。
#
#	Args:
#		model:
#			クラスを判別するためのモデルオブジェクト。計算には使われないので、
#			実体は空でOK。
#		args.predict: predictに渡される引数の入ったリスト。
#		type:
#			"model": モデル構築に渡される引数だと仮定してパラメーターを取り出す。
#			"predict": predictに渡される引数だと仮定してパラメーターを取り出す。
#
#	Value:
#		候補パラメーターの入ったリスト。
#-------------------------------------------------------------------------------
get.tunable.args <- function(model, args, type){
	UseMethod("get.tunable.args")
}
# defaultは何もしない。
get.tunable.args.default <- function(model, args, type){
	return(NULL)
}
# gbm
get.tunable.args.gbm <- function(model, args, type){
	arg.names <- switch(
		type,
		model = c(
			"n.minobsinnode", "interaction.depth", "bag.fraction", "shrinkage"
		),
		predict = "n.trees"
	)
	return(get.args(args, arg.names))
}
# randomForest
get.tunable.args.randomForest <- function(model, args, type){
	if (type == "predict"){
		return(NULL)
	} else {
		return(get.args(args, c("mtry","sampsize", "nodesize", "maxnodes")))
	}
}