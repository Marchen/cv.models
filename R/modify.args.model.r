#'	Modify settings of modeling.
#'
#'	Internal function to modify parameters of modeling.
#'
#'	@param object
#'		result of model function or \emph{cv.models.dummy} object created by
#'		\code{\link{make.dummy}} function.
#'	@param args.model a list containing parameters used for modeling.
#'	@param args.predict a list containing parameters used for prediction.
#'	@param check.args if TRUE, this function do nothing.
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
#		check.args: ���ꂪFALSE�Ȃ�A���̊֐��͉������Ȃ��B
#-------------------------------------------------------------------------------
modify.args.model <- function(object, args.model, args.predict, check.args){
	UseMethod("modify.args.model")
}

#-------------------------------------------------------------------------------
#'	@describeIn modify.args.model
#'	@method modify.args.model default Default S3 method.
#-------------------------------------------------------------------------------
modify.args.model.default <- function(
	object, args.model, args.predict, check.args
){
	return(args.model)
}

#-------------------------------------------------------------------------------
#'	@describeIn modify.args.model
#'	@method modify.args.model svm 
#-------------------------------------------------------------------------------
#	�m����Ԃ��悤�ɋ�����ύX����B
#-------------------------------------------------------------------------------
modify.args.model.svm <- function(
	object, args.model, args.predict, check.args
){
	if (check.args){
		args.model$probability = TRUE
	}
	return(args.model)
}

#-------------------------------------------------------------------------------
#'	@describeIn modify.args.model
#'	@method modify.args.model gbm
#-------------------------------------------------------------------------------
modify.args.model.gbm <- function(object, args.model, args.predict, check.args){
	if (check.args){
		# ���f���\�z�p��n.trees��predict�p��n.trees���������Ȃ�������A
		# �����I��n.trees�𑝂₷�B
		if (!is.null(args.predict$n.trees)){
			n.trees.predict <- max(args.predict$n.trees)
			n.trees.model <- ifelse(
				is.null(args.model$n.trees), 100, args.model$n.trees
			)
			if (n.trees.model < n.trees.predict){
				args.model$n.trees <- n.trees.predict
			}
		}
	}
	return(args.model)
}

