#'	Modify settings of modeling.
#'
#'	Internal function to modify parameters of modeling.
#'
#'	@param object
#'		result of model function or \emph{cv.models.dummy} object created by
#'		\code{\link{make.dummy}} function.
#'	@param args.model a list containing parameters used for modeling.
#'	@param args.predict a list containing parameters used for prediction.
#'	@param data a data.frame containng data used for modeling.
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
#'		\item{\code{\link[lme4]{lmer}}, \code{\link[lme4]{glmer}}}{
#'			This function expands '.' in specified formula and makes formula
#'			without '.'.
#'		}
#'	}
#'
#'	@return A list containing modified parameters for model construction.
#-------------------------------------------------------------------------------
#	���f�������\�w�W�𐳂����v�Z����悤�ɁA���f���\�z�̈������C�����鑍�̊֐��B
#
#	Args:
#		object: �I�u�W�F�N�g�B�v�Z�Ɏg��Ȃ��̂Ń_�~�[��OK�B
#		args.model: ���f���\�z�Ɏg��������B
#		args.predict: predict�Ɏg��������B
#-------------------------------------------------------------------------------
modify.args.model <- function(object, args.model, args.predict, data){
	UseMethod("modify.args.model")
}

#-------------------------------------------------------------------------------
#'	@describeIn modify.args.model
#'	Default S3 method.
#'	@method modify.args.model default
#-------------------------------------------------------------------------------
modify.args.model.default <- function(object, args.model, args.predict, data){
	return(args.model)
}

#-------------------------------------------------------------------------------
#'	@describeIn modify.args.model
#'	Method for \code{\link[e1071]{svm}} object in \emph{e1071} package.
#'	@method modify.args.model svm
#-------------------------------------------------------------------------------
#	�m����Ԃ��悤�ɋ�����ύX����B
#-------------------------------------------------------------------------------
modify.args.model.svm <- function(object, args.model, args.predict, data){
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
modify.args.model.gbm <- function(object, args.model, args.predict, data){
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
#'	Method for lmerMod made by \code{\link[lme4]{lmer}} function in \emph{lme4} 
#'	package.
#'	@method modify.args.model lmerMod
#-------------------------------------------------------------------------------
#	formula��.��W�J���āA.�̂Ȃ����ɕϊ�����B
#-------------------------------------------------------------------------------
modify.args.model.lmerMod <- function(object, args.model, args.predict, data){
	# ���̏���
	f <- get.formula(object, args.model)
	f <- terms.formula(f, data = data)
	attributes(f) <- NULL
	f <- as.formula(f)
	args.model[[which(sapply(args.model, is.formula))]] <- f
	return(args.model)
}

#-------------------------------------------------------------------------------
#'	@describeIn modify.args.model
#'	Method for glmerMod made by \code{\link[lme4]{glmer}} function in \emph{lme4} 
#'	package.
#'	@method modify.args.model glmerMod
#-------------------------------------------------------------------------------
#	formula��.��W�J���āA.�̂Ȃ����ɕϊ�����B
#-------------------------------------------------------------------------------
modify.args.model.glmerMod <- function(object, args.model, args.predict, data){
	# ���̏���
	return(modify.args.model.lmerMod(object, args.model, args.predict, data))
}

