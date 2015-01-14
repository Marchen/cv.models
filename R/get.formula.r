#-------------------------------------------------------------------------------
#'	Check an object is formula.
#'
#'	@param x an object.
#'	@value returns TRUE if \emph{x} is formula otherwise returns FALSE.
#-------------------------------------------------------------------------------
#	�ϐ���formula���𒲂ׂ�B
#
#	Args:
#		x: �ϐ��B
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
#	���f���\�z�Ɏg����������烂�f����������킷formula���擾����B
#
#	Args:
#		object: ���f���I�u�W�F�N�g�B�v�Z�ɂ͎g���Ȃ��B
#		args.model: ���f���\�z�Ɏg�����������ꂽ���X�g�B
#-------------------------------------------------------------------------------
get.formula <- function(object, args.model){
	UseMethod("get.formula")
}

#-------------------------------------------------------------------------------
#'	@describeIn get.formula
#'	@method get.formula default
#'		Default S3 method. This function is used for handling a result of
#'		\code{\link[stats]{lm}}, \code{\link[stats]{glm}},
#'		\code{\link[lme4]{lmer}}, \code{\link[lme4]{glmer}}, 
#'		\code{\link[party]{ctree}}, \code{\link[party]{cforest}}, 
#'		\code{\link[randomForest]{randomForest}}, \code{\link[gbm]{gbm}}, 
#'		\code{\link[e1071]{svm}}, \code{\link[tree]{tree}}, 
#'		\code{\link[rpart]{rpart}}, \code{\link[gam]{gam}} in \emph{gam} package,
#'		\code{\link[mgcv]{gam}} in \emph{mgcv} package and
#'		\code{\link[mgcv]{gamm}}.
#-------------------------------------------------------------------------------
get.formula.default <- function(object, args.model){
	if (!is.null(args.model$formula)){
		return(args.model$formula)
	} else {
		return(args.model[[sapply(args.model, is.formula)]])
	}
}

#-------------------------------------------------------------------------------
#'	@describeIn get.formula
#'	@method get.formula lme
#		Method for \code{\link[nlme]{lme}} function in \emph{nlme} package.
#-------------------------------------------------------------------------------
get.formula.lme <- function(object, args.model){
	if (!is.null(args.model$fixed)){
		return(args.model$fixed)
	}
	# �����Ɋ܂܂��formula�̂Ȃ���random���폜���āA�P�Ԗڂɂ�����̂�Ԃ��B
	formulae <- args.model[sapply(args.model, is.formula)]
	formulae$random <- NULL
	return(formulae[[1]])
}


