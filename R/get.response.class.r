#-------------------------------------------------------------------------------
#	���f���I�u�W�F�N�g���牞���ϐ��̃f�[�^�^���擾����֐��B
#
#	Args:
#		object: ���f���I�u�W�F�N�g
#-------------------------------------------------------------------------------

get.response.class <- function(object){
	UseMethod("get.response.class")
}

get.response.class.default <- function(object){
	return(class(object$data[[get.response.name(object)]]))
}

get.response.class.BinaryTree <- function(object){
	return(class(object@data@get("response")[[1]]))
}

get.response.class.RandomForest <- function(object){
	return(class(object@data@get("response")[[1]]))
}

get.response.class.svm <- function(object){
	return(attr(object$terms,"dataClasses")[get.response.name(object)])
}


