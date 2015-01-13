#' Modify settings of modeling.
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
#		args.predict: predict�Ɏg��������B
#-------------------------------------------------------------------------------

modify.args.model <- function(object, args.model, args.predict){
	UseMethod("modify.args.model")
}

# default S3 method
modify.args.model.default <- function(object, args.model, args.predict){
	return(args.model)
}

# svm
# �m����Ԃ��悤�ɋ�����ύX����B
modify.args.model.svm <- function(object, args.model, args.predict){
	args.model$probability = TRUE
	return(args.model)
}

# gbm
modify.args.model.gbm <- function(object, args.model, args.predict){
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
	return(args.model)
}


