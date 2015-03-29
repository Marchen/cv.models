#'	Modify settings of modeling.
#'
#'	Internal function that hodifis parameters used for modeling.
#'
#'	@param cv.dummy
#'		A \emph{cv.dummy} object created by \code{\link{make.dummy}} function.
#'		Because this function is always called before construction of model, 
#'		'real' object created by actual model funcions cannot be used.
#'	@param args.model A list containing parameters used for modeling.
#'	@param args.predict A list containing parameters used for prediction.
#'	@param data A data.frame containng data used for modeling.
#'
#'	@details
#'	\describe{
#'		\item{\code{\link[e1071]{svm}}}{
#'			This function set \emph{probability} in \emph{args.model} to TRUE.
#'		}
#'		\item{\code{\link[gbm]{gbm}}}{
#'			If the maximum value of n.trees specified in \emph{args.predict}
#'			is larger than the value of n.trees specified in \emph{args.model},
#'			this function change the value of n.trees in \emph{args.model} to the
#'			maximum value.
#'		}
#'		\item{
#'			merMod object created by \code{\link[lme4]{lmer}} and 
#'			\code{\link[lme4]{glmer}} functions, \code{\link[gam]{gam}} 
#'			in \emph{gam} and \emph{mgcv} packages, \code{\link[mgcv]{gamm}}
#'		}{
#'			This function expands '.' in specified formula and makes formula
#'			without '.'.
#'		}
#'	}
#'
#'	@return A list containing modified parameters for model construction.
#-------------------------------------------------------------------------------
#	モデルが性能指標を正しく計算するように、モデル構築の引数を修正する総称関数。
#	モデル構築前に呼ばれるので、実際のモデルオブジェクトでは動かない。
#
#	Args:
#		cv.dummy: make.dummy()関数で作ったcv.dummyオブジェクト。
#		args.model: モデル構築に使われる引数。
#		args.predict: predictに使われる引数。
#-------------------------------------------------------------------------------
modify.args.model <- function(cv.dummy, args.model, args.predict, data){
	UseMethod("modify.args.model")
}

#-------------------------------------------------------------------------------
#'	@describeIn modify.args.model
#'	Default S3 method.
#'	@method modify.args.model default
#-------------------------------------------------------------------------------
modify.args.model.default <- function(cv.dummy, args.model, args.predict, data){
	return(args.model)
}

#-------------------------------------------------------------------------------
#'	@describeIn modify.args.model
#'	Method for \code{\link[e1071]{svm}} object in \emph{e1071} package.
#'	@method modify.args.model svm
#-------------------------------------------------------------------------------
#	確率を返すように挙動を変更する。
#-------------------------------------------------------------------------------
modify.args.model.svm <- function(cv.dummy, args.model, args.predict, data){
	args.model$probability = TRUE
	return(args.model)
}

#-------------------------------------------------------------------------------
#'	@describeIn modify.args.model
#'	Method for \code{\link[gbm]{gbm}} object in \emph{gbm} package.
#'	@method modify.args.model gbm
#-------------------------------------------------------------------------------
#	モデル構築用のn.treesがpredict用のn.treesがよりも少なかったら、
#	自動的にn.treesを増やす。
#-------------------------------------------------------------------------------
modify.args.model.gbm <- function(cv.dummy, args.model, args.predict, data){
	if (!is.null(args.predict$n.trees)){
		n.trees.predict <- max(args.predict$n.trees)
		n.trees.model <- ifelse(
			is.null(args.model$n.trees), 100, args.model$n.trees
		)
		if (n.trees.model < n.trees.predict){
			args.model$n.trees <- n.trees.predict
		}
	}
	return(args.model)
}

#-------------------------------------------------------------------------------
#'	@describeIn modify.args.model
#'	Method for lmerMod class created by \code{\link[lme4]{lmer}} function in
#'	\emph{lme4} package.
#'	@method modify.args.model lmerMod
#-------------------------------------------------------------------------------
#	formulaの.を展開して、.のない式に変換する。
#-------------------------------------------------------------------------------
modify.args.model.lmerMod <- function(cv.dummy, args.model, args.predict, data){
	return(expand.dot(cv.dummy, args.model, data))
}

#-------------------------------------------------------------------------------
#'	@describeIn modify.args.model
#'	Method for glmerMod class created by \code{\link[lme4]{glmer}} function in 
#'	\emph{lme4} package.
#'	@method modify.args.model gmerMod
#-------------------------------------------------------------------------------
#	formulaの.を展開して、.のない式に変換する。
#-------------------------------------------------------------------------------
modify.args.model.glmerMod <- function(cv.dummy, args.model, args.predict, data){
	return(expand.dot(cv.dummy, args.model, data))
}

#-------------------------------------------------------------------------------
#'	@describeIn modify.args.model
#'	Method for \code{\link[mgcv]{gam}} in \emph{mgcv} package and 
#'	\code{\link[gam]{gam}} functions in \emph{gam} package.
#'	@method modify.args.model gam
#-------------------------------------------------------------------------------
#	formulaの.を展開して、.のない式に変換する。
#-------------------------------------------------------------------------------
modify.args.model.gam <- function(cv.dummy, args.model, args.predict, data){
	return(expand.dot(cv.dummy, args.model, data))
}

#-------------------------------------------------------------------------------
#'	@describeIn modify.args.model
#'	Method for \code{\link[mgcv]{gamm}} in \emph{mgcv} package.
#'	@method modify.args.model gamm
#-------------------------------------------------------------------------------
#	formulaの.を展開して、.のない式に変換する。
#-------------------------------------------------------------------------------
modify.args.model.gamm <- function(cv.dummy, args.model, args.predict, data){
	return(expand.dot(cv.dummy, args.model, data))
}

#-------------------------------------------------------------------------------
#'	Make a list containing arguments.
#'
#'	This function handle delayed evaluation of the \emph{cluster} argument of 
#'	\code{\link{[glmmML]glmmML}} function.
#-------------------------------------------------------------------------------
#	glmmMLのclusterの遅延評価に対応するための関数。
#-------------------------------------------------------------------------------
args.model <- function(..., cluster = NULL){
	result <- list(...)
	# glmmMLのclusterの遅延評価に対応するための処理
	if (!missing(cluster)) result$cluster <- as.name(substitute(cluster))
	return(result)
}



