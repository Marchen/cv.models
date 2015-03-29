#-------------------------------------------------------------------------------
#'	(Internal) Detect type of model.
#'
#'	This internal function detects type of model (classification/regression).
#'
#'	@inheritParams modify.args.model
#'	@return a character "classification" or "regression".
#-------------------------------------------------------------------------------
#	モデルが識別なのか回帰なのかを判定する。
#
#	Args:
#		cv.dummy: cv.dummyオブジェクト
#		args.model: モデル構築に使われるパラメーター。
#-------------------------------------------------------------------------------
detect.model.type <- function(cv.dummy, args.model, data){
	UseMethod("detect.model.type")
}

#-------------------------------------------------------------------------------
#'	@describeIn detect.model.type
#'	default S3 method. Intended to be used for \code{\link[party]{ctree}},
#'	\code{\link[party]{cforest}}, \code{\link[randomForest]{randomForest}},
#'	\code{\link[e1071]{svm}}, \code{\link[tree]{tree}} and 
#'	\code{\link[rpart]{rpart}} 
#'	@method detect.model.type default
#-------------------------------------------------------------------------------
detect.model.type.default <- function(cv.dummy, args.model, data){
	return(get.model.type.from.response.var(cv.dummy, args.model, data))
}

#-------------------------------------------------------------------------------
#'	@describeIn detect.model.type
#'	method for \code{\link[stats]{lm}} in \emph{stats} package.
#'	@method detect.model.type lm
#-------------------------------------------------------------------------------
detect.model.type.lm <- function(cv.dummy, args.model, data){
	return("regression")
}

#-------------------------------------------------------------------------------
#'	@describeIn detect.model.type
#'	method for \code{\link[stats]{glm}} in \emph{stats} package.
#'	@method detect.model.type glm
#-------------------------------------------------------------------------------
detect.model.type.glm <- function(cv.dummy, args.model, data){
	return(get.model.type.from.family(args.model))
}

#-------------------------------------------------------------------------------
#'	@describeIn detect.model.type
#'	method for \code{\link[nlme]{lme}} in \emph{nlme} package.
#'	@method detect.model.type lme
#-------------------------------------------------------------------------------
detect.model.type.lme <- function(cv.dummy, args.model, data){
	return("regression")
}

#-------------------------------------------------------------------------------
#'	@describeIn detect.model.type
#'	method for \code{\link[lme4]{lmer}} in \emph{lme4} package.
#'	@method detect.model.type lmerMod
#-------------------------------------------------------------------------------
detect.model.type.lmerMod <- function(cv.dummy, args.model, data){
	return("regression")
}

#-------------------------------------------------------------------------------
#'	@describeIn detect.model.type
#'	method for \code{\link[lme4]{glmer}} in \emph{lme4} package.
#'	@method detect.model.type glmerMod
#-------------------------------------------------------------------------------
detect.model.type.glmerMod <- function(cv.dummy, args.model, data){
	return(get.model.type.from.family(args.model))
}

#-------------------------------------------------------------------------------
#'	@describeIn detect.model.type
#'	method for \code{\link[mgcv]{gam}} in \emph{mgcv} package and 
#'	\code{\link[gam]{gam}} in \emph{gam} package.
#'	@method detect.model.type gam
#-------------------------------------------------------------------------------
detect.model.type.gam <- function(cv.dummy, args.model, data){
	return(get.model.type.from.family(args.model))
}

#-------------------------------------------------------------------------------
#'	@describeIn detect.model.type
#'	method for \code{\link[mgcv]{gamm}} in \emph{mgcv} package.
#'	@method detect.model.type gamm
#-------------------------------------------------------------------------------
detect.model.type.gamm <- function(cv.dummy, args.model, data){
	return(get.model.type.from.family(args.model))
}

#-------------------------------------------------------------------------------
#'	@describeIn detect.model.type
#'	method for \code{\link[gbm]{gbm}} in \emph{gbm} package.
#'	@method detect.model.type gbm
#-------------------------------------------------------------------------------
detect.model.type.gbm <- function(cv.dummy, args.model, data){
	# 分布が指定されているとき
	if (!is.null(args.model$distribution)){
		# 以下が識別、それ以外は回帰として扱う。
		classification.families <- c(
			"bernoulli", "huberized", "multinomial", "adaboost"
		)
		if (args.model$distribution %in% classification.families){
			return("classification")
		} else {
			return("regression")
		}
	}
	# 分布が指定されていなかったら、gbmと同じように推定。
	response <- get.response.var(cv.dummy, data, args.model)
	if (nlevels(as.factor(response)) == 2){
		# 2クラスだったら識別。
		return("classification")
	}
	if (is(response, "factor")){
		# 応答変数が因子だったら識別。
		return("classification")
	}
	return("regression")
}

#-------------------------------------------------------------------------------
#'	(Internal) Detect type of model from family.
#'
#'	This internal function detects type of model from glm and mgcv::gam family.
#'
#'	@inheritParams modify.args.model
#'	@return a character "classification" or "regression".
#-------------------------------------------------------------------------------
get.model.type.from.family <- function(args.model){
	# familyがなかったらデフォルトは正規分布なので、回帰。
	if (is.null(args.model$family)){
		return("regression")
	}
	# familyを文字列に変換。
	family <- format.family(args.model$family, type = "character")
	# glmとgamの以下のfamilyが識別。それ以外は回帰
	classification.families <- c(
		"binomial", "quasibinomial", "negbin", "ocat", "nb", "betar", "cox.ph"
	)
	if (family %in% classification.families){
		return("classification")
	}
	return("regression")
}

#-------------------------------------------------------------------------------
#'	(Internal) Detect type of model from class of response variable.
#'
#'	This internal function detects type of model from class of response variable.
#'
#'	@inheritParams modify.args.model
#'	@return a character "classification" or "regression".
#-------------------------------------------------------------------------------
get.model.type.from.response.var <- function(cv.dummy, args.model, data){
	if (is(get.response.var(cv.dummy, data, args.model), "factor")){
		return("classification")
	} else {
		return("regression")
	}
}

