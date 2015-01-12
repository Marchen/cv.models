#-------------------------------------------------------------------------------
#	モデルの性能評価指標にそのときに使われた調整可能なパラメーターの値を追加する
#	補助関数。
#
#	Args:
#		function.name: モデル作成に使われる関数名を表す文字列。
#		metrics: モデルの性能評価指標が入った行列。
#		args: モデル作成もしくはpredictに渡される引数が入ったリスト。
#		type:
#			"model":
#				argsがモデル構築に渡される引数だと仮定してパラメーターを追加。
#			"predict":
#				argsがpredictに渡される引数だと仮定してパラメーターを追加。
#-------------------------------------------------------------------------------
merge.tunable.args <- function(function.name, metrics, args, type){
	# チューニングした候補パラメーターを取得する
	dummy <- make.dummy(function.name)
	tunable.args <- get.tunable.args(dummy, args, type)
	if (is.null(tunable.args)){
		# なければそのままmetricsを返す
		return(metrics)
	}
	# 結合するデータを用意。行名が残っていると警告が出るので、行名は消す。
	grid <- do.call(expand.grid, tunable.args)
	grid <- split(grid, 1:nrow(grid))
	grid <- lapply(grid , "rownames<-", NULL)
	map.args <- c(list(metrics), list(grid), f = cbind)
	return(do.call(Map, map.args))
}
