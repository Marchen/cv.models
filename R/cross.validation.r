
#-------------------------------------------------------------------------------
#	渡された設定でモデルを作り、クロスバリデーションを行う。
#
#	Args:
#		model.function: モデルを作成する関数。
#		args.model:
#			モデルを作成する関数に渡される引数。dataはCVを行うために内部的に
#			置き換えられるので、指定しても意味がありません。
#		そのほかはcv.models関数の説明を見てください。
#
#	Value:
#		以下の値が含まれたリスト。
#		metrics:
#			計算した指標が入った行列(matrix)を返します。各列が各指標です。
#			gbmのようにpredict関数で性能に影響するパラメーターを設定できる関数
#			ではそのパラメーターごとに計算した指標が行で返ってきます。
#			指標の計算の際にYoudenの方法で最適な閾値が計算できなかったときにも
#			複数列の指標が返ってきます。
#		cv.prediction:
#			指標計算の元になったクロスバリデーションの予測値が入ります。
#-------------------------------------------------------------------------------
cross.validation <- function(
	model.function, args.model, data, args.predict = list(), cv.folds = 10,
	cv.metrics = c("auc"), n.cores = NULL, seed = NULL, positive.class = NULL,
	cv.dummy
){
	# 計算クラスターを初期化
	cl <- init.cluster(n.cores)
	cl$library(cv.dummy$package)
	# クロスバリデーションで予測値を計算
	if (!is.null(seed)) set.seed(seed)
	cv.result <- cl$lapply(
		1:cv.folds, cv.one.fold, model.function = model.function,
		args.model = args.model, args.predict = args.predict, data = data,
		cv.group = make.cv.group(data, cv.folds), seed = seed,
		positive.class = positive.class
	)
	cv.result <- do.call(rbind, cv.result)
	# 予測値からモデルの性能評価指標を計算
	metrics <- cl$lapply(
		cv.result[-1], cv.performance, response = cv.result[[1]],
		cv.metrics = cv.metrics, positive.class = positive.class
	)
	cl$close()
	# 結果を整形
	cv.metrics <- merge.tunable.args(
		cv.dummy, lapply(metrics, "[[", "metrics"), args.predict, "predict"
	)
	cv.metrics <- do.call(rbind, cv.metrics)
	row.names(cv.metrics) <- NULL
	cv.prediction = do.call(cbind, lapply(metrics, "[[", "cv.prediction"))
	cv.response = do.call(cbind, lapply(metrics, "[[", "response"))
	cv.c.matrices = do.call(c, lapply(metrics, "[[", "confusion.matrix"))
	return(
		list(
			cv.metrics = cv.metrics, cv.prediction = cv.prediction,
			cv.response = cv.response, cv.confusion.matrices = cv.c.matrices
		)
	)
}
