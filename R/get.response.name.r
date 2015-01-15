#-------------------------------------------------------------------------------
#'	Get name of response variable from parameters.
#'
#'	This internal function retrieves the name of the response variable in 
#'	specified parameters used for modeling.
#'
#'	@inheritParams modify.args.model
#-------------------------------------------------------------------------------
#	�����ϐ��̖��O��Ԃ����̊֐�
#
#	Args:
#		object: ���f���I�u�W�F�N�g�B
#		args.model: ���f���\�z�Ɏg�����������ꂽ���X�g�B
#
#	Value:
#		�����ϐ��̖��O��\��������B
#-------------------------------------------------------------------------------
get.response.name <- function(object, args.model = NULL){
	if (is(object, "cv.models.dummy") & is.null(args.model)){
		stop("True object or non NULL args.model must be specified!")
	}
	UseMethod("get.response.name")
}

#-------------------------------------------------------------------------------
#'	@describeIn get.response.name
#'		Default S3 method. This function handles result of \code{\link[stats]{lm}}, 
#'		\code{\link[stats]{glm}}, \code{\link[nlme]{lme}} and 
#'		\code{\link[randomForest]{randomForest}} functions. 
#'	@method get.response.name default
#-------------------------------------------------------------------------------
get.response.name.default <- function(object, args.model = NULL){
	if (is(object, "cv.models.dummy")){
		return(as.character(get.formula(object, args.model))[2])
	} else {
		return(as.character(object$terms[[2]]))
	}
}

#-------------------------------------------------------------------------------
#'	@describeIn get.response.name
#		Method for \code{\link[gbm]{gbm}} function in \emph{gbm} package.
#'	@method get.response.name gbm
#-------------------------------------------------------------------------------
get.response.name.gbm <- function(object, args.model = NULL){
	if (is(object, "cv.models.dummy")){
		return(get.response.name.default(object, args.model))
	} else {
    	return(object$response.name)
	}
}

#-------------------------------------------------------------------------------
#'	@describeIn get.response.name
#		Method for \code{\link[party]{cforest}} function in \emph{party} package.
#'	@method get.response.name RandomForest
#-------------------------------------------------------------------------------
get.response.name.RandomForest <- function(object, args.model = NULL){
    if (is(object, "cv.models.dummy")){
		return(get.response.name.default(object, args.model))
	} else {
		return(names(object@responses@variables))
	}
}

#-------------------------------------------------------------------------------
#'	@describeIn get.response.name
#		Method for \code{\link[party]{ctree}} function in \emph{party} package.
#'	@method get.response.name BinaryTree
#-------------------------------------------------------------------------------
get.response.name.BinaryTree <- function(object, args.model = NULL){
    if (is(object, "cv.models.dummy")){
		return(get.response.name.default(object, args.model))
	} else {
	    return(names(object@responses@variables))
	}
}

#-------------------------------------------------------------------------------
#'	@describeIn get.response.name
#		Method for \code{\link[lme4]{lmer}} function in \emph{lme4} package.
#'	@method get.response.name lmerMod
#-------------------------------------------------------------------------------
get.response.name.lmerMod <- function(object, args.model = NULL){
    if (is(object, "cv.models.dummy")){
		return(get.response.name.default(object, args.model))
	} else {
    	return(names(object@frame)[1])
	}
}

#-------------------------------------------------------------------------------
#'	@describeIn get.response.name
#		Method for \code{\link[lme4]{glmer}} function in \emph{glme4} package.
#'	@method get.response.name glmerMod
#-------------------------------------------------------------------------------
get.response.name.glmerMod <- function(object, args.model = NULL){
    if (is(object, "cv.models.dummy")){
		return(get.response.name.default(object, args.model))
	} else {
	    return(names(object@frame)[1])
	}
}

#-------------------------------------------------------------------------------
#'	@describeIn get.response.name
#		Method for \code{\link[mgcv]{gamm}} function in \emph{mgcv} package.
#'	@method get.response.name gamm
#-------------------------------------------------------------------------------
get.response.name.gamm <- function(object, args.model = NULL){
    if (is(object, "cv.models.dummy")){
		return(get.response.name.default(object, args.model))
	} else {
    	get.response.name(object$gam)
	}
}

