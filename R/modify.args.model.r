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
#	���f�������\�w�W�𐳂����v�Z����悤�ɁA���f���\�z�̈������C�����鑍�̊֐��B
#	���f���\�z�O�ɌĂ΂��̂ŁA���ۂ̃��f���I�u�W�F�N�g�ł͓����Ȃ��B
#
#	Args:
#		cv.dummy: make.dummy()�֐��ō����cv.dummy�I�u�W�F�N�g�B
#		args.model: ���f���\�z�Ɏg��������B
#		args.predict: predict�Ɏg��������B
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
#	�m����Ԃ��悤�ɋ�����ύX����B
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
#	���f���\�z�p��n.trees��predict�p��n.trees���������Ȃ�������A
#	�����I��n.trees�𑝂₷�B
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
#	formula��.��W�J���āA.�̂Ȃ����ɕϊ�����B
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
#	formula��.��W�J���āA.�̂Ȃ����ɕϊ�����B
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
#	formula��.��W�J���āA.�̂Ȃ����ɕϊ�����B
#-------------------------------------------------------------------------------
modify.args.model.gam <- function(cv.dummy, args.model, args.predict, data){
	return(expand.dot(cv.dummy, args.model, data))
}

#-------------------------------------------------------------------------------
#'	@describeIn modify.args.model
#'	Method for \code{\link[mgcv]{gamm}} in \emph{mgcv} package.
#'	@method modify.args.model gamm
#-------------------------------------------------------------------------------
#	formula��.��W�J���āA.�̂Ȃ����ɕϊ�����B
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
#	glmmML��cluster�̒x���]���ɑΉ����邽�߂̊֐��B
#-------------------------------------------------------------------------------
args.model <- function(..., cluster = NULL){
	result <- list(...)
	# glmmML��cluster�̒x���]���ɑΉ����邽�߂̏���
	if (!missing(cluster)) result$cluster <- as.name(substitute(cluster))
	return(result)
}



