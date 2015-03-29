#-------------------------------------------------------------------------------
#'	(Internal) Detect type of model.
#'
#'	This internal function detects type of model (classification/regression).
#'
#'	@inheritParams modify.args.model
#'	@return a character "classification" or "regression".
#-------------------------------------------------------------------------------
#	���f�������ʂȂ̂���A�Ȃ̂��𔻒肷��B
#
#	Args:
#		cv.dummy: cv.dummy�I�u�W�F�N�g
#		args.model: ���f���\�z�Ɏg����p�����[�^�[�B
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
	# ���z���w�肳��Ă���Ƃ�
	if (!is.null(args.model$distribution)){
		# �ȉ������ʁA����ȊO�͉�A�Ƃ��Ĉ����B
		classification.families <- c(
			"bernoulli", "huberized", "multinomial", "adaboost"
		)
		if (args.model$distribution %in% classification.families){
			return("classification")
		} else {
			return("regression")
		}
	}
	# ���z���w�肳��Ă��Ȃ�������Agbm�Ɠ����悤�ɐ���B
	response <- get.response.var(cv.dummy, data, args.model)
	if (nlevels(as.factor(response)) == 2){
		# 2�N���X�������环�ʁB
		return("classification")
	}
	if (is(response, "factor")){
		# �����ϐ������q�������环�ʁB
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
	# family���Ȃ�������f�t�H���g�͐��K���z�Ȃ̂ŁA��A�B
	if (is.null(args.model$family)){
		return("regression")
	}
	# family�𕶎���ɕϊ��B
	family <- format.family(args.model$family, type = "character")
	# glm��gam�̈ȉ���family�����ʁB����ȊO�͉�A
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

