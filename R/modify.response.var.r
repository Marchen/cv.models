#-------------------------------------------------------------------------------
#'	Modify class of response variable.
#'
#'	Some functions require factor response variable for binary classification.
#'	This function change the class of response variable to attain correct result. 
#'	This function is an internal function and not intended to be used directly 
#'	by users.
#'
#'	@inheritParams modify.args.model
#'	@param response The response variable to be modified.
#'
#'	@details
#'	If the object is a object of \code{\link[tree]{tree}},
#'	\code{\link[rpart]{rpart}}, \code{\link[randomForest]{randomForest}} or
#'	\code{\link[e1071]{svm}} and unique values of response variable are (TRUE
#'	and FALSE) or (0 and 1), this function change the class of response variable
#'	to factor.
#'
#'	@return A vector containing modified/unmodified response variable
#-------------------------------------------------------------------------------
#	�����ϐ���TRUE/FALSE��������A0/1�̓�l�������肵���Ƃ��A���q�^�Ƃ��ė^���Ȃ�
#	�Ǝ��ʖ��Ƃ��Ĉ���Ȃ����f���̂��߁A�����ϐ��̌^��ϊ����鑍�̊֐��B
#	tree, rpart, randomForest, svm�֐��ɂ͕ϊ�����������B
#
#	Args:
#		object: ���f���I�u�W�F�N�g�B�_�~�[��OK�B
#		response: �C�����鉞���ϐ��B
#		check.args: ���ꂪFALSE��������C�����s��Ȃ��B
#-------------------------------------------------------------------------------
modify.response.var <- function(object, response, check.args){
	UseMethod("modify.response.var")
}

#-------------------------------------------------------------------------------
#'	@describeIn modify.response.var Default S3 method.
#'	@method modify.response.var default
#-------------------------------------------------------------------------------
#	default��response�����̂܂ܕԂ��B
#-------------------------------------------------------------------------------
modify.response.var.default <- function(object, response, check.args){
	return(response)
}

#-------------------------------------------------------------------------------
#'	@describeIn modify.response.var
#'	Method for \code{\link[tree]{tree}} class of \emph{tree} package.
#'	@method modify.response.var tree
#-------------------------------------------------------------------------------
#	tree, rpart, randomForest, svm �͉����ϐ���TRUE/FALSE�A0/1������������A
#	���q�^�ɕϊ����ĕԂ��B
#-------------------------------------------------------------------------------
modify.response.var.tree <- function(object, response, check.args){
	return(response.var.to.factor(check.args, response))
}

#-------------------------------------------------------------------------------
#'	@describeIn modify.response.var
#'	Method for \code{\link[rpart]{rpart}} class in \emph{rpart} package.
#'	@method modify.response.var rpart
#-------------------------------------------------------------------------------
modify.response.var.rpart <- function(object, response, check.args){
	return(response.var.to.factor(check.args, response))
}

#-------------------------------------------------------------------------------
#'	@describeIn modify.response.var
#'	Method for \code{\link[randomForest]{randomForest}} class in
#'	\emph{randomForest} package.
#'	@method modify.response.var randomForest
#-------------------------------------------------------------------------------
modify.response.var.randomForest <- function(object, response, check.args){
	return(response.var.to.factor(check.args, response))
}

#-------------------------------------------------------------------------------
#'	@describeIn modify.response.var
#'	Method for \code{\link[e1071]{svm}} class in \emph{svm} package.
#'	@method modify.response.var svm
#-------------------------------------------------------------------------------
modify.response.var.svm <- function(object, response, check.args){
	return(response.var.to.factor(check.args, response))
}

#-------------------------------------------------------------------------------
#'	Make response variable to factor.
#'
#'	Internal function making response variable to factor.
#'	@inheritParams modify.response.var
#'	@return A vector containing modified/unmodified response variable
#-------------------------------------------------------------------------------
#	�����ϐ��ϊ����������s��������֐��B
#-------------------------------------------------------------------------------
response.var.to.factor <- function(check.args, response){
	if (!check.args){
		return(response)
	}
	if (is(response, "factor")){
		return(response)
	}
	unique.values <- sort(unique(response))
	if (
		identical(unique.values, c(FALSE, TRUE)) | identical(unique.values, 0:1)
	){
		response <- as.factor(response)
	}
	return(response)
}
