#-------------------------------------------------------------------------------
#'	Standardize result of predict method.
#'
#'	This is a generic function which standardize result of \emph{predict} method.
#'	This function is an internal function and not intended to be used directly by users.
#'
#'	@inheritParams modify.args.model
#'	@param prediction result of \emph{predict} function.
#'	@return a vector containing reuslt of \emph{predict} method. If the
#'	\emph{object} is a instance of \code{\link[gbm]{gbm}}, this function returns
#'	a matrix containing the result of predict function.
#-------------------------------------------------------------------------------
#	predictから返ってくるデータを正規化する総称関数。
#
#	Args:
#		object: モデルオブジェクト
#		prediction: predictの結果。
#
#	Value:
#		predictの結果の入ったベクトル。
#		gbmでn.treesが複数指定されたときには行列が返る。
#-------------------------------------------------------------------------------
format.prediction <- function(object, prediction){
	UseMethod("format.prediction")
}

#-------------------------------------------------------------------------------
#'	@describeIn format.prediction Default S3 method.
#'	@method format.prediction default
#-------------------------------------------------------------------------------
#	defaultはpredictionをそのまま返す。
#-------------------------------------------------------------------------------
format.prediction.default <- function(object, prediction){
	return(prediction)
}

#-------------------------------------------------------------------------------
#'	@describeIn format.prediction BinaryTree class of \code{\link[party]{ctree}} function in \emph{party} package.
#'	@method format.prediction BinaryTree
#-------------------------------------------------------------------------------
#	ctree, cforestは応答変数が因子型だと、結果がリストで返ってくるので
#	rbindして結果を行列に変換して、列名を調整する。
#-------------------------------------------------------------------------------
format.prediction.BinaryTree <- function(object, prediction){
	if (get.response.class(object) == "factor"){
		prediction <- do.call(rbind, prediction)
		# 列名がない/列名に応答変数名がつく問題に対処。
		response <- object@responses@variables[[get.response.name(object)]]
		colnames(prediction) <- levels(response)
	}
	return(prediction)
}

#-------------------------------------------------------------------------------
#'	@describeIn format.prediction RandomForest class of \code{\link[party]{cforest}} function in \emph{party} package.
#'	@method format.prediction RandomForest
#-------------------------------------------------------------------------------
format.prediction.RandomForest <- function(object, prediction){
	return(format.prediction.BinaryTree(object, prediction))
}

#-------------------------------------------------------------------------------
#'	@describeIn format.prediction svm class of \code{\link[e1071]{svm}} function in \emph{e1071} package.
#'	@method format.prediction svm
#-------------------------------------------------------------------------------
#	svmは確率を属性で返すので、それを取り出して返す。
#-------------------------------------------------------------------------------
format.prediction.svm <- function(object, prediction){
	if (get.response.class(object) == "factor"){
		prediction = attr(prediction, "probabilities")
	}
	return(prediction)
}

#-------------------------------------------------------------------------------
#'	@describeIn format.prediction svm class of \code{\link[e1071]{svm}} function in \emph{e1071} package.
#'	@method format.prediction svm
#-------------------------------------------------------------------------------
#	rangerはいろんな情報を持ったオブジェクトを返すので、
#	中から予測値を取り出して返す。
#-------------------------------------------------------------------------------
format.prediction.ranger <- function(object, prediction) {
	if (object$treetype == "Classification"){
		nclass <- length(object$forest$class.values)
		result <- matrix(nrow = nrow(prediction$prediction), ncol = nclass)
		for (i in object$forest$class.values){
			result[, i] <- rowSums(prediction$prediction == i)
		}
		result <- result / prediction$num.trees
		colnames(result) <- object$forest$levels
		return(result)
	} else {
		return(prediction$prediction)
	}
}


