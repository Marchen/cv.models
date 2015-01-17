
#===============================================================================
#	いろいろなモデル性能評価指標の計算関数
#===============================================================================
# MSE
calc.mse <- function(response, prediction){
	if (is.factor(response)){
		return(NA)
	}
	return(mean((prediction - response) ^ 2))
}
# RMSE
calc.rmse <- function(response, prediction){
	return(sqrt(calc.mse(response, prediction)))
}
# R二乗
calc.r.squared <- function(response, prediction, method = "pearson"){
	if (is.factor(response)){
		return(NA)
	}
	return(cor(response, prediction, method = method) ^ 2)
}
# informedness
calc.informedness <- function(metrics){
	return(metrics[, "sensitivity"] + metrics[, "specificity"] - 1)
}
# markedness
calc.markedness <- function(metrics){
	return(metrics[, "ppv"] + metrics[, "npv"] - 1)
}
# Matthews correlation coefficient
calc.mcc <- function(metrics){
	mcc = sqrt(
		(metrics[, "sensitivity"] + metrics[, "specificity"] - 1)
		* (metrics[, "ppv"] + metrics[, "npv"] - 1)
	)
	return(mcc)
}
# pROC::coordsで計算できる指標
calc.coords.metrics <- function(roc.object, coords.ret){
	coords.metrics <- coords(
		roc.object, x = "best", best.method = "youden", ret = coords.ret
	)
	# TODO: Youdenがタイだったときの処理
	if (is.matrix(coords.metrics)){
		coords.metrics <- t(coords.metrics)
	} else {
		metrics.name <- names(coords.metrics)
		coords.metrics <- matrix(coords.metrics, nrow = 1)
		colnames(coords.metrics) <- metrics.name
	}
	return(coords.metrics)
}

#-------------------------------------------------------------------------------
#	すべての指標を計算する。引数はcv.performanceを参照。
#-------------------------------------------------------------------------------
calc.all.metrics <- function(
	response, prediction, cv.metrics, model.type, cor.method = NULL
){
	coords.ret <- c(
		"threshold", "specificity", "sensitivity", "accuracy",
		"tn", "tp", "fn", "fp", "npv", "ppv",
		"1-specificity", "1-sensitivity", "1-accuracy", "1-npv", "1-ppv"
	)
	# 結果を初期化
	result <- matrix(nrow = 1, ncol = 0)
	# 指標を計算
	coords.dependent.metrics <- c("informedness", "markedness", "auc", "mcc")
	if (
		model.type == "classification"
 		| any(c(coords.ret, coords.dependent.metrics) %in% cv.metrics)
	){
		# YoudenのJで最適な閾値が決まらないと各指標が複数になる。複数になるか
		# を先に判定するため、まずcoordsに依存する指標を計算してしまう。
		require(pROC)
		roc.object <- roc(response, prediction)
		if (any(c("informedness", "mcc", coords.ret) %in% cv.metrics)){
			result <- calc.coords.metrics(roc.object, coords.ret)
			result <- cbind(result, informedness = calc.informedness(result))
			result <- cbind(result, mcc = calc.mcc(result))
		}
		result <- cbind(result, auc = roc.object$auc)
	}
	result <- cbind(result, mse = calc.mse(response, prediction))
	result <- cbind(result, rmse = calc.rmse(response, prediction))
	result <- cbind(
		result, r.squared = calc.r.squared(response, prediction, cor.method)
	)
	# 結果を整形
	rownames(result) <- NULL
	result <- data.frame(result)
	return(result)
}

#-------------------------------------------------------------------------------
#	モデルの性能評価指標を計算する関数。
#
#	Args:
#		response: 応答変数の値を入れたベクトル。
#		prediction: モデルの予測値を入れたベクトル。
#		cv.metrics:
#			クロスバリデーションで計算する指標の名前を表す文字列ベクトル。
#			複数指定可能。
#			pROC::coordsで計算できる指標全てと
#			"informedness": sensitivity + specificity - 1
#			"auc": AUC: Area under curve
#			"mcc": Matthews correlation coefficient
#			"mse": Mean square error
#			"rmse": root mean square error
#			"r.squared": R二乗値
#			に対応。
#		positive.class: 陽性として扱うクラスのラベル。
#		model.type: "regression" or "classification"
#		cor.method:
#			R二乗値を計算するときの方法。デフォルトはSpearmanの積率相関係数。
#
#	Value:
#		計算した指標が入った行列。行が指標。もし、最適な閾値をYoudenの方法で決定
#		出来ないときには複数の指標が列で返る。
#-------------------------------------------------------------------------------
cv.performance <- function(
	response, prediction, cv.metrics, positive.class, model.type,
	cor.method = NULL
){
	metrics <- calc.all.metrics(
		response, prediction, cv.metrics, model.type, cor.method
	)
	if (model.type == "classification"){
		c.matrix <- confusion.matrix(
			response, prediction, metrics[, "threshold"], positive.class
		)
	} else {
		c.matrix <- NULL
	}
	metrics = metrics[cv.metrics]			# 必要な指標だけ返す。
	# 最適な閾値が決まらず、結果が複数になったとき、predictionとresponseを複製
	prediction <- do.call(cbind, rep(list(prediction), nrow(metrics)))
	response <- do.call(data.frame, rep(list(response), nrow(metrics)))
	colnames(response) <- NULL
	result <- list(
		metrics = metrics, cv.prediction = prediction, response = response,
		confusion.matrix = c.matrix
	)
	class(result) <- "cv.performance"
	return(result)
}





