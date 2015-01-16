#-------------------------------------------------------------------------------
#	モデルの応答変数が因子型だったとき、predictの結果から陽性として扱う因子の
#	確率を取り出す。
#
#	Args:
#		response: 応答変数の生データ。
#		prediction: predictの結果
#		positive.class:
#			陽性として扱うクラスを表す文字列。指定されなかった場合は
#			(TRUE, FALSE), (1, 0), (+, -), (+, 0)のセットの左側を陽性として扱い、
#			を自動的に陽性としてデータの取得を試みる。それでも陽性が決定できない
#			場合、クラスの１番目を陽性として扱う。
#			因子が３クラス以上だった場合、１列目の確率を用いる。
#
#	Value:
#		responseが因子型でない場合、そのままpredictionを返す。
#		responseが因子型の場合、陽性の上の方法で判定した陽性の確率を返す。
#-------------------------------------------------------------------------------
get.positive.prob <- function(response, prediction, positive.class = NULL){
	# 因子型以外はそのまま値を返す。
	if (!is.factor(response)){
		return(prediction)
	}
	# 陽性として使うクラスが見つからなかったら、１番目を返す。
	positive.class <- get.positive.class(response, positive.class)
	if (is.null(positive.class)){
		return(prediction[, 1)
	} else {
		return(prediction[, positive.class])
	}
}


#-------------------------------------------------------------------------------
#	factor型の変数で陽性のラベルとして扱う物を決定する。
#
#	Args:
#		response: 応答変数の生データ。
#		prediction: predictの結果
#		positive.class:
#			陽性として扱うクラスを表す文字列。指定されなかった場合は
#			(TRUE, FALSE), (1, 0), (+, -), (+, 0)のセットの左側を陽性として扱い、
#			を自動的に陽性としてデータの取得を試みる。それでも陽性が決定できない
#			場合、クラスの１番目を陽性として扱う。
#			因子が３クラス以上だった場合、１列目の確率を用いる。
#	Value:
#		陽性のラベルを決定できたときにはそのラベルを、決定できなかったときには
#		NULLを返す。
#-------------------------------------------------------------------------------
get.positive.class <- function(response, positive.class = NULL){
	# どのラベルが陽性か指定されていたら、そのラベルの確率を返す。
	if (!is.null(positive.class)){
		return(positive.class)
	}
	# ３クラス以上の因子型は１番目の因子が陽性だと仮定して値を返す。
	if (nlevels(response) > 2){
		warning("Number of classes of response variable was > 2 and 'positive.class' was not specified! \nCalculations were done by assuming the first level of the response variable is the positive case.")
		return(NULL)
	}
	# TRUE, 1, + がラベルに含まれていたら、それらを陽性と仮定して値を返す。
	classes <- levels(response)
	for (i in list(c("TRUE", "FALSE"), c("1", "0"), c("+", "-"), c("+", "0"))){
		if (identical(classes, i) | identical(classes, rev(i))){
			return(i[1])
		}
	}
	# それでも指標を決定できないときには１番目が陽性だと仮定して値を返す。
	warning("'positive.class' was not specified and positive class label could not be determined. \nMetrics were calculated by assuming the first level of the response variable is the positive case.")
	return(NULL)
}

