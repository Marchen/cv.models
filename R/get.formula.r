#-------------------------------------------------------------------------------
#'	Check an object is formula.
#'
#'	@param x an object.
#'	@return returns TRUE if \emph{x} is formula otherwise returns FALSE.
#-------------------------------------------------------------------------------
#	変数がformulaかを調べる。
#
#	Args:
#		x: 変数。
#-------------------------------------------------------------------------------
is.formula <- function(x){
	return(is(x, "formula"))
}

#-------------------------------------------------------------------------------
#'	Get formula from parameters.
#'
#'	This internal function retrieves formula from arguments used for modeling.
#'
#'	@inheritParams modify.args.model
#-------------------------------------------------------------------------------
#	モデル構築に使われる引数からモデル式をあらわすformulaを取得する。
#
#	Args:
#		object: モデルオブジェクト。計算には使われない。
#		args.model: モデル構築に使われる引数を入れたリスト。
#-------------------------------------------------------------------------------
get.formula <- function(object, args.model){
	UseMethod("get.formula")
}

#-------------------------------------------------------------------------------
#'	@describeIn get.formula Default S3 method.
#'	This function is used for handling a result of
#'		\code{\link[stats]{lm}}, \code{\link[stats]{glm}},
#'		\code{\link[lme4]{lmer}}, \code{\link[lme4]{glmer}}, 
#'		\code{\link[party]{ctree}}, \code{\link[party]{cforest}}, 
#'		\code{\link[randomForest]{randomForest}}, \code{\link[gbm]{gbm}}, 
#'		\code{\link[e1071]{svm}}, \code{\link[tree]{tree}}, 
#'		\code{\link[rpart]{rpart}}, \code{\link[gam]{gam}} in \emph{gam} package,
#'		\code{\link[mgcv]{gam}} in \emph{mgcv} package and
#'		\code{\link[mgcv]{gamm}}.
#'	@method get.formula default
#-------------------------------------------------------------------------------
get.formula.default <- function(object, args.model){
	if (!is.null(args.model$formula)){
		return(args.model$formula)
	} else {
		return(args.model[sapply(args.model, is.formula)][[1]])
	}
}

#-------------------------------------------------------------------------------
#'	@describeIn get.formula
#'	Method for \code{\link[nlme]{lme}} function in \emph{nlme} package.
#'	@method get.formula lme
#-------------------------------------------------------------------------------
get.formula.lme <- function(object, args.model){
	if (!is.null(args.model$fixed)){
		return(args.model$fixed)
	}
	# 引数に含まれるformulaのなかでrandomを削除して、１番目にあるものを返す。
	formulae <- args.model[sapply(args.model, is.formula)]
	formulae$random <- NULL
	return(formulae[[1]])
}


