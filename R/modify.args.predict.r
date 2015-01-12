#===============================================================================
#	predict�Ɏg���������w�W�v�Z�Ő����������悤�ɏC�����鑍�̊֐��B
#
#	Args:
#		object: ���f���I�u�W�F�N�g�B
#		args.predict: predict�ɓn�������������������X�g
#===============================================================================
modify.args.predict <- function(object, args.predict){
	UseMethod("modify.args.predict")
}

# default��type��"response"�ɏ���������B
# OK: glm, lm, gbm, cforest, svm, lmer, glmer, lme, randomForest
modify.args.predict.default <- function(object, args.predict){
	if (get.response.class(object) == "factor"){
		args.predict$type <- "prob"
	} else {
		args.predict$type <- "response"
	}
	return(args.predict)
}

modify.args.predict.tree <- function(object, args.predict){
	return(args.predict)
}

modify.args.predict.rpart <- function(object, args.predict){
	return(args.predict)
}

modify.args.predict.randomForest <- function(object, args.predict){
	if (object$type == "classification"){
		args.predict$type <- "prob"		# ���ʖ��Ȃ�prob
	} else {
		args.predict$type <- "response"	# ��A�Ȃ�response
	}
	return(args.predict)
}

modify.args.predict.svm <- function(object, args.predict){
	if (get.response.class(object) == "factor"){
		# ���q�^��������e�N���X�̊m�����v�Z������B
		args.predict$probability = TRUE
	}
	return(args.predict)
}