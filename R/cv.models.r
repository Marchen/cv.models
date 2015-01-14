#-------------------------------------------------------------------------------
#	cv.modelsオブジェクトを作るための補助関数。
#
#	Args:
#		各パラメーターに対応。詳細は↓参照。
#
#	Value:
#		cv.modelsオブジェクト。
#-------------------------------------------------------------------------------
cv.models.object <- function(
	model.function, function.name, package.name, data, args.model, args.predict,
	cv.metrics, cv.prediction, seed, positive.label
){
	object <- list(
		model.function = model.function, function.name = function.name,
		package.name = package.name, data = data,
		args.model = args.model, args.predict = args.predict,
		cv.metrics = cv.metrics, cv.prediction = cv.prediction, seed = seed,
		positive.label = positive.label
	)
	class(object) <- "cv.models"
	return(object)
}

#'	@export
#-------------------------------------------------------------------------------
#	モデルの性能に影響するパラメーターの候補を組み合わせてモデルを作り、
#	クロスバリデーションで性能評価を行う。
#
#	Args:
#		model.function: モデルを作成する関数。
#		args.model:
#			モデルの構築に渡される引数。
#			gbmのようにモデルの性能に影響するパラメーターをベクトルで複数指定
#			すると、それぞれの候補に対して指標が計算される。dataは自動的に
#			置き換えられるので、指定しても意味がありません。
#		data:
#			モデル作成に使うデータ。これを分割してクロスバリデーションを行う。
#		args.predict:
#			predict関数に渡される引数。gbmのn.treesのようにモデルの性能に影響
#			するパラメーターはget.tunable.args()関数で対応することで、複数の値
#			それぞれに対して指標を計算することが出来ます。
#		cv.folds: クロスバリデーションの分割数。
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
#		n.cores:
#			計算に使うコアの数。何も指定しないと全てのコアを使って計算します。
#		seed:
#			結果を固定したいときには乱数の種を指定する。ここで種子を固定すると、
#			get.best.models()関数の結果も固定される。種子を固定すると結果は
#			クラスターを使っても使わなくても同じになる。
#		positive.lael:
#			陽性として扱うクラスを表す文字列。指定されなかった場合は
#			(TRUE, FALSE), (1, 0), (+, -), (+, 0)のセットの左側を陽性として扱い、
#			を自動的に陽性としてデータの取得を試みる。それでも陽性が決定できない
#			場合、クラスの１番目を陽性として扱う。
#			因子が３クラス以上だった場合、１列目の確率を用いる。
#		check.args:
#			これがTRUEだと正しい結果が得られるように、モデル構築、推定値計算に
#			使われるパラメーターを修正し、応答変数の型の変換を行います。
#			FALSEにすると、モデル構築・predict関数の全ての挙動はユーザーの指定した
#			パラメーターのままになり、整合性のチェックが行われません。
#		function.name:
#			モデル構築に使われる関数名。通常は自動的に設定されるので指定する必要
#			はありません（cv.modelsからこの関数を呼び出すときのために実装されて
#			います）。
#		package.name:
#			モデル構築に使われる関数の入ったパッケージ名。通常は自動的に設定
#			されるので、指定する必要はありません。gamパッケージとmgcvパッケージ
#			のgam関数のように、異なるパッケージに入っている同名の関数を
#			呼び出したいとき、明示的にパッケージ名を指定するために使います。
#
#	Value:
#		cv.modelsオブジェクト。以下の値を持つ。
#			model.function: モデルを作成するときに使う関数。
#			function.name: モデルを作成する関数名。
#			package.name: モデル作成関数が含まれているパッケージの名前。
#			data: クロスバリデーションに使われたデータ。
#			args.model: モデル構築に渡されたパラメーターの候補。
#			args.predict: predict関数に渡されたパラメーターの候補。
#			cv.metrics: クロスバリデーションで計算された性能評価指標。
#			cv.prediction: クロスバリデーションで計算された予測値。
#			seed: 計算に使った乱数の種子。
#-------------------------------------------------------------------------------
cv.models <- function(
	model.function, args.model, data, args.predict = list(), cv.folds = 10,
	cv.metrics = c("auc"), n.cores = NULL, seed = NULL, positive.label = NULL,
	dredge = NULL, check.args = TRUE,
	function.name = as.character(substitute(model.function)),
	package.name = get.package.name(function.name)

){
	dummy <- make.dummy(function.name)
	# パラメーター候補の組み合わせを作る。
	expanded.args <- expand.tunable.args(dummy, args.model, "model")
	# 候補パラメーターの数によって、並列計算する場所を変える。
	n.cores.cv <- ifelse(length(expanded.args) < n.cores, n.cores, 1)
	n.cores.param.tune <- ifelse(length(expanded.args) < n.cores, 1, n.cores)
	# モデルの性能をクロスバリデーション。
	cl <- init.cluster(n.cores.param.tune)
	cl$library(package.name)
	metrics <- cl$lapply(
		expanded.args, cross.validation, model.function = model.function,
		data = data, args.predict = args.predict, cv.folds = cv.folds,
		cv.metrics = cv.metrics, n.cores = n.cores.cv, seed = seed,
		positive.label = positive.label, check.args = check.args,
		function.name = function.name, package.name = package.name
	)
	# 候補パラメーターをCVの結果に結合。
	cv.metrics <- merge.tunable.args(
		function.name , lapply(metrics, "[[", "cv.metrics"), args.model, "model"
	)
	cv.metrics <-do.call(rbind, cv.metrics)
	# CVの予測値を取り出し。
	cv.prediction <- do.call(cbind, lapply(metrics, "[[", "cv.prediction"))
	result <- cv.models.object(
		model.function, function.name, package.name, data,
		args.model, args.predict, cv.metrics, cv.prediction, seed,
		positive.label
	)
	cl$close()
	return(result)
}

#'	@export
#-------------------------------------------------------------------------------
#	cv.modelsクラス用のprint。
#
#	Args:
#		x: cv.modelsオブジェクト。
#		...: 使われていません。
#-------------------------------------------------------------------------------
print.cv.models <- function(x, ...){
	cat("Result of cross validation\n")
	cat(sprintf("Function name: %s\n", x$function.name))
	cat("Cross validation metrics:\n")
	print(x$cv.metrics)
	cat("\n")
}
