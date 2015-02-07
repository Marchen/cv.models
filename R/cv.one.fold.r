
#-------------------------------------------------------------------------------
#	１つのfoldでクロスバリデーションする関数。
#
#	Args:
#		model.function: モデルを作る関数。glmとか、gbmとか。
#		args.model: モデル構築に使われる引数。
#		args.predict:
#			predict関数に渡される引数。gbmのn.treesみたいに予測性能に影響する
#			パラメーターはc()関数で複数与えるとそれぞれの値を使って、予測値を
#			計算する。ただし、そのクラス用のget.tunable.args関数を作って対応
#			する必要がある。
#		data: モデル構築に使うデータ
#		cv.group: クロスバリデーションのグループ。
#		cv.index: クロスバリデーションでテストデータにするグループ番号。
#		seed: 乱数の種子。
#		positive.class: 応答変数が因子型の時、陽性として扱うクラスを表す文字列。
#		
#	Value:
#		テストデータを使った予測値と応答変数の実測値が入った行列。
#		実測値の列名はyになる。予測値の列名はモデル依存。
#-------------------------------------------------------------------------------
cv.one.fold <- function(
	model.function, args.model, args.predict, data, cv.group, cv.index, seed,
	positive.class = NULL
){
	# モデル構築
	if (!is.null(seed)) set.seed(seed)
	data.test <- data[cv.group == cv.index, ]
	data.train <- data[cv.group != cv.index, ]
	args.model$data <- data.train
	model <- do.call(model.function, args.model)
	# 指標が正しく計算できるように、predictの引数を修正
	args.predict$object <- model
	args.predict$newdata <- data.test
	# 予測値を計算
	predictions <- do.call(predict, args.predict)
	predictions <- format.prediction(model, predictions)
	# 予測値の中から陽性の確率を取り出す。
	response <- data.test[[get.response.name(model)]]
	predictions <- get.positive.prob(response, predictions, positive.class)
	# 結果を整形
	result <- cbind(y = response, as.data.frame(predictions))
	rownames(result) <- rownames(data.test)
	return(result)
}
