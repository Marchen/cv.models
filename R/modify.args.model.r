#' Supporting generic function which modify 
#'
#' This function calcuate effective bust cap size ratio of your idol
#' when you can know her cap size.
#'
#' @param object idol object
#' @keywords bust
#' @export
#' @examples
#' my.idol     <- new("Idol")
#' my.idol.ecr <- effective.capsize(my.idol)
#-------------------------------------------------------------------------------
#	���f�������\�w�W�𐳂����v�Z����悤�ɁA���f���\�z�̈������C�����鑍�̊֐��B
#
#	Args:
#		object: �I�u�W�F�N�g�B�v�Z�Ɏg��Ȃ��̂Ń_�~�[��OK�B
#		args.model: ���f���\�z�Ɏg��������B
#-------------------------------------------------------------------------------

modify.args.model <- function(object, args.model){
	UseMethod("modify.args.model")
}
# default S3 method
modify.args.model.default <- function(object, args.model){
	return(args.model)
}
# svm
# �m����Ԃ��悤�ɋ�����ύX����B
modify.args.model.svm <- function(object, args.model){
	args.model$probability = TRUE
	return(args.model)
}


