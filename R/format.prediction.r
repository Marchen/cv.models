#-------------------------------------------------------------------------------
#'	Standardize result of predict method.
#'
#'	This is a generic function which standardize result of \emph{predict} method.
#'	This function is an internal function and not intended to be used directly by users.
#'
#'	@param object model object used for the prediction.
#'	@param prediction result of \emph{predict} function.
#'	@return a vector containing reuslt of \emph{predict} method.
#-------------------------------------------------------------------------------
#	predict����Ԃ��Ă���f�[�^�𐳋K�����鑍�̊֐��B
#
#	Args:
#		object: ���f���I�u�W�F�N�g
#		prediction: predict�̌��ʁB
#
#	Value:
#		predict�̌��ʂ̓������x�N�g���B
#		gbm��n.trees�������w�肳�ꂽ�Ƃ��ɂ�
#-------------------------------------------------------------------------------
format.prediction <- function(object, prediction){
	UseMethod("format.prediction")
}

#-------------------------------------------------------------------------------
#'	@describeIn format.prediction Default S3 method.
#'	@method format.prediction default
#-------------------------------------------------------------------------------
#	default��prediction�����̂܂ܕԂ��B
#-------------------------------------------------------------------------------
format.prediction.default <- function(object, prediction){
	return(prediction)
}

#-------------------------------------------------------------------------------
#'	@describeIn format.prediction BinaryTree class of \code{\link[party]{ctree}} function in \emph{party} package.
#'	@method format.prediction BinaryTree
#-------------------------------------------------------------------------------
#	ctree, cforest�͉����ϐ������q�^���ƁA���ʂ����X�g�ŕԂ��Ă���̂�
#	rbind���Č��ʂ��s��ɕϊ����āA�񖼂𒲐�����B
#-------------------------------------------------------------------------------
format.prediction.BinaryTree <- function(object, prediction){
	if (get.response.class(object) == "factor"){
		prediction <- do.call(rbind, prediction)
		# �񖼂��Ȃ�/�񖼂ɉ����ϐ����������ɑΏ��B
		response <- object@responses@variables[[get.response.name(object)]]
		colnames(prediction) <- levels(response)
	}
	return(prediction)
}

#-------------------------------------------------------------------------------
#'	@describeIn format.prediction RandomForest class of \code{\link[party]{cforest}} function in \emph{party} package.
#'	@method format.prediction RandomForest
#-------------------------------------------------------------------------------
format.prediction.RandomForest <- function(object, prediction){
	return(format.prediction.BinaryTree(object, prediction))
}

#-------------------------------------------------------------------------------
#'	@describeIn format.prediction svm class of \code{\link[e1071]{svm}} function in \emph{e1071} package.
#'	@method format.prediction svm
#-------------------------------------------------------------------------------
#	svm�͊m���𑮐��ŕԂ��̂ŁA��������o���ĕԂ��B
#-------------------------------------------------------------------------------
format.prediction.svm <- function(object, prediction){
	if (get.response.class(object) == "factor"){
		prediction = attr(prediction, "probabilities")
	}
	return(prediction)
}

