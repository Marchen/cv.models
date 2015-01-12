
#===============================================================================
#	�����ϐ��̖��O��Ԃ����̊֐�
#	�����Ń��f���ɑΉ�����悤�ɑ��̊֐���ǉ�����ƁA�V�����֐��ɑΉ��ł���B
#
#	Args:
#		object: ���f���I�u�W�F�N�g
#
#	Value:
#		�����ϐ��̖��O��\��������B
#===============================================================================
get.response.name <- function(object){
	UseMethod("get.response.name")
}

# lm, glm, lme, glmmML, randomForest, svm
get.response.name.default <- function(object){
	return(as.character(object$terms[[2]]))
}
# gbm
get.response.name.gbm <- function(object){
	return(object$response.name)
}
# cforest
get.response.name.RandomForest <- function(object){
	return(names(object@responses@variables))
}
# cforest
get.response.name.BinaryTree <- function(object){
	return(names(object@responses@variables))
}
# lmer
get.response.name.lmerMod <- function(object){
	return(names(object@frame)[1])
}
# glmer
get.response.name.glmerMod <- function(object){
	return(names(object@frame)[1])
}
# gamm
get.response.name.gamm <- function(object){
	get.response.name(object$gam)
}
