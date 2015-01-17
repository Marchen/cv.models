

#-------------------------------------------------------------------------------
#	cv.modelsオブジェクトからモデルを構築する補助関数。
#
#	Args:
#		x: cv.modelsオブジェクト
#		metrics.index:
#			複数のパラメーター候補があるときには、モデル構築に使うパラメーターの
#			値が含まれているcv.metricsのインデックスを指定する。
#			候補パラメーターがなければ1を指定すればOK。
#	Values:
#		以下の変数が含まれたリスト。
#			model: 構築したモデルのオブジェクト。
#			cv.metrics: そのモデルのクロスバリデーション性能指標。
#			cv.prediction: クロスバリデーションで指標を計算するときの予測値。
#			function.name: 呼び出した関数の名前。
#			package.name: 関数が入ったパッケージ名。
#-------------------------------------------------------------------------------
construct.model <- function(x, metrics.index){
	# モデルに渡す引数を準備
	if (!is.null(x$seed)) set.seed(x$seed)
	args <- x$args.model
	args$data <- x$data
	tunable.args <- get.tunable.args(
		make.dummy(x$function.name, x$package.name),
		x$cv.metrics[metrics.index, ], "model"
	)
	args[names(tunable.args)] <- tunable.args
	# モデルを構築
	result <- list(
		model = do.call(x$function.name, args),
		cv.metrics = x$cv.metrics[metrics.index, ],
		cv.prediction = x$cv.prediction[, metrics.index],
		cv.response = x$cv.response[, metrics.index],
		confusion.matrix = x$confusion.matrices[[metrics.index]],
		function.name = x$function.name,
		package.name = x$package.name
	)
	class(result) <- "cv.best.model"
	return(result)
}