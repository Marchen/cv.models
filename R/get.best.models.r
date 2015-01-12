

#-------------------------------------------------------------------------------
#	cv.modelsオブジェクトからmetricsで指定した指標で最も成績のよいモデルを
#	取り出す。cv.modelsオブジェクト・metricsは複数指定可能。
#
#	Args:
#		...: cv.modelsオブジェクト。
#		metrics:
#			モデル間で比較する性能指標の名前が入った文字列ベクトル。
#			複数指定すると値がタイだったときに順番に評価される。
#
#	Value:
#		以下の構造のcv.best.modelsオブジェクト。
#		タイがあるとモデルが複数になる可能性があるので、construct.model()関数の
#		結果をリストに入れて返す。
#			list(
#				list(model, cv.metrics, function.name),
#				list(model, cv.metrics, function.name), ...
#			)
#			model: 構築したモデルのオブジェクト。
#			cv.metrics: そのモデルのクロスバリデーション性能指標。
#			cv.prediction: クロスバリデーションに使った予測値。
#			function.name: 呼び出した関数の名前。
#-------------------------------------------------------------------------------
get.best.models <- function(..., metrics = "auc"){
	# 複数指定されたcv.modelsオブジェクトの中から、それぞれで最適な指標を取り出す。
	# 指標の他に何番目の指標かを表すmetrics.indexも追加する。
	get.best.metrics <- function(x, metrics){
		if (!is(x, "cv.models")) stop("Please specify 'cv.models' object!")
		met <- cbind(x$cv.metrics, metrics.index = 1:nrow(x$cv.metrics))
		met <- met[
			get.best.metrics.index(met[metrics]), c(metrics, "metrics.index")
		]
		return(met)
	}
	objects <- list(...)
	best.metrics <- lapply(objects, get.best.metrics, metrics)
	# 何番目のモデルオブジェクトかを表すobject.indexを追加して結合。
	best.metrics <- mapply(
		cbind, best.metrics, object.index = 1:length(objects), SIMPLIFY = FALSE
	)
	best.metrics <- do.call(rbind, best.metrics)
	# 最適なモデルの指標を計算
	best.index <- get.best.metrics.index(best.metrics[metrics])
	best.metrics <- best.metrics[best.index, ]
	# モデルを構築
	object.index <- best.metrics[, "object.index"]
	metrics.index <- best.metrics[, "metrics.index"]
	make.models <- function(objects, object.index, metrics.index){
		return(construct.model(objects[[object.index]], metrics.index))
	}
	models <- mapply(
		make.models, object.index, metrics.index,
		MoreArgs = list(objects = objects), SIMPLIFY = FALSE
	)
	class(models) <- "cv.best.models"
	return(models)
}
