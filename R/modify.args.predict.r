#-------------------------------------------------------------------------------
#'	Modify parameters used for predict.
#'
#'	This function modifies parameters used for prediction. 
#'	This function is an internal function and not intended to be used directly 
#'	by users.
#'
#'	@inheritParams modify.args.model
#'	@return Modified args.predict list.
#'
#'	@details
#'	This function conduct following modification on parameters used for
#	prediction according to the class of the \emph{object}.
#'	\describe{
#'		\item{default}{
#'			By default, this function set \emph{type} in \emph{args.predict} to
#'			\emph{"prob"} if class of response variable is factor. Otherwise,
#'			this function set \emph{type} to \emph{"response"}.
#'		}
#'		\item{\code{\link[tree]{tree}}, \code{\link[rpart]{rpart}}}{
#'			This function do nothing.
#'		}
#'		\item{\code{\link[randomForest]{randomForest}}}{
#'			If type of model is classification, this function set \emph{type} in
#'			\emph{args.predict} to \emph{"prob"}. Otherwise, this function set
#'			\emph{type} to \emph{"response"}.
#'		}
#'		\item{\code{\link[e1071]{svm}}}{
#'			If the class of response variable is factor, this function set
#'			\emph{probability} in \emph{args.model} to TRUE.
#'		}
#'	}
#'
#'	@return A list containing modified parameters for predictions.
#-------------------------------------------------------------------------------
#	predictに使う引数を指標計算で正しく動くように修正する総称関数。
#
#	Args:
#		object: モデルオブジェクト。
#		args.predict: predictに渡される引数が入ったリスト。
#		check.args: これがTRUEだったらこの関数は何もしない。
#-------------------------------------------------------------------------------
modify.args.predict <- function(object, args.predict, check.args){
	UseMethod("modify.args.predict")
}

#-------------------------------------------------------------------------------
#'	@describeIn modify.args.predict Default S3 method.
#'	@method modify.args.predict default
#-------------------------------------------------------------------------------
#	defaultは応答変数が因子型だったらtypeを"prob"に、それ以外の場合は"response"に
#	書き換える。
#	OK: glm, lm, gbm, cforest, svm, lmer, glmer, lme
#-------------------------------------------------------------------------------
modify.args.predict.default <- function(object, args.predict, check.args){
	if (check.args){
		if (get.response.class(object) == "factor"){
			args.predict$type <- "prob"
		} else {
			args.predict$type <- "response"
		}
	}
	return(args.predict)
}

#-------------------------------------------------------------------------------
#'	@describeIn modify.args.predict
#'	Method for \code{\link[tree]{tree}} class of \emph{tree} package.
#'	@method modify.args.predict tree
#-------------------------------------------------------------------------------
modify.args.predict.tree <- function(object, args.predict, check.args){
	return(args.predict)
}

#-------------------------------------------------------------------------------
#'	@describeIn modify.args.predict
#'	Method for \code{\link[rpart]{rpart}} class of \emph{rpart} package.
#'	@method modify.args.predict rpart
#-------------------------------------------------------------------------------
modify.args.predict.rpart <- function(object, args.predict, check.args){
	return(args.predict)
}

#-------------------------------------------------------------------------------
#'	@describeIn modify.args.predict
#'	Method for \code{\link[randomForest]{randomForest}} class of
#'	\emph{randomForest} package.
#'	@method modify.args.predict randomForest
#-------------------------------------------------------------------------------
modify.args.predict.randomForest <- function(object, args.predict, check.args){
	if (check.args){
		if (object$type == "classification"){
			args.predict$type <- "prob"		# 識別問題ならprob
		} else {
			args.predict$type <- "response"	# 回帰ならresponse
		}
	}
	return(args.predict)
}

#-------------------------------------------------------------------------------
#'	@describeIn modify.args.predict
#'	Method for \code{\link[e1071]{svn}} class of \emph{e1071} package.
#'	@method modify.args.predict svm
#-------------------------------------------------------------------------------
modify.args.predict.svm <- function(object, args.predict, check.args){
	if (check.args){
		if (get.response.class(object) == "factor"){
			# 因子型だったら各クラスの確率も計算させる。
			args.predict$probability = TRUE
		}
	}
	return(args.predict)
}


