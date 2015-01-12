
#===============================================================================
#	���낢��ȃ��f�����\�]���w�W�̌v�Z�֐�
#===============================================================================
# MSE
calc.mse <- function(response, prediction){
	if (is.factor(response)){
		return(NA)
	} else {
		return(mean((prediction - response) ^ 2))
	}
}
# RMSE
calc.rmse <- function(response, prediction){
	return(sqrt(calc.mse(response, prediction)))
}
# R���
calc.r.squared <- function(response, prediction, method = "pearson"){
	return(cor(response, prediction, method = method) ^ 2)
}
# informedness
calc.informedness <- function(metrics){
	return(metrics[, "sensitivity"] + metrics[, "specificity"] - 1)
}
# markedness
calc.markedness <- function(metrics){
	return(metrics[, "ppv"] + metrics[, "npv"] - 1)
}
# Matthews correlation coefficient
calc.mcc <- function(metrics){
	mcc = sqrt(
		(metrics[, "sensitivity"] + metrics[, "specificity"] - 1)
		* (metrics[, "ppv"] + metrics[, "npv"] - 1)
	)
	return(mcc)
}
# pROC::coords�Ōv�Z�ł���w�W
calc.coords.metrics <- function(roc.object, coords.ret){
	coords.metrics <- coords(
		roc.object, x = "best", best.method = "youden", ret = coords.ret
	)
	# TODO: Youden���^�C�������Ƃ��̏���
	if (is.matrix(coords.metrics)){
		coords.metrics <- t(coords.metrics)
	} else {
		metrics.name <- names(coords.metrics)
		coords.metrics <- matrix(coords.metrics, nrow = 1)
		colnames(coords.metrics) <- metrics.name
	}
	return(coords.metrics)
}

#-------------------------------------------------------------------------------
#	���f���̐��\�]���w�W���v�Z����֐��B
#
#	Args:
#		response: �����ϐ��̒l����ꂽ�x�N�g���B
#		prediction: ���f���̗\���l����ꂽ�x�N�g���B
#		cv.metrics:
#			�N���X�o���f�[�V�����Ōv�Z����w�W�̖��O��\��������x�N�g���B
#			�����w��\�B
#			pROC::coords�Ōv�Z�ł���w�W�S�Ă�
#			"informedness": sensitivity + specificity - 1
#			"auc": AUC: Area under curve
#			"mcc": Matthews correlation coefficient
#			"mse": Mean square error
#			"rmse": root mean square error
#			"r.squared": R���l
#			�ɑΉ��B
#		cor.method:
#			R���l���v�Z����Ƃ��̕��@�B�f�t�H���g��Spearman�̐ϗ����֌W���B
#
#	Value:
#		�v�Z�����w�W���������s��B�s���w�W�B�����A�œK��臒l��Youden�̕��@�Ō���
#		�o���Ȃ��Ƃ��ɂ͕����̎w�W����ŕԂ�B
#-------------------------------------------------------------------------------
calc.cv.metrics <- function(
	response, prediction, cv.metrics, cor.method = NULL
){
	coords.ret <- c(
		"threshold", "specificity", "sensitivity", "accuracy",
		"tn", "tp", "fn", "fp", "npv", "ppv",
		"1-specificity", "1-sensitivity", "1-accuracy", "1-npv", "1-ppv"
	)
	# ���ʂ�������
	result <- matrix(nrow = 1, ncol = 0)
	# �w�W���v�Z
	coords.dependent.metrics <- c("informedness", "markedness", "auc", "mcc")
	if (any(c(coords.ret, coords.dependent.metrics) %in% cv.metrics)){
		# Youden��J�ōœK��臒l�����܂�Ȃ��Ɗe�w�W�������ɂȂ�̂ŁA
		# ���coords�Ɉˑ�����w�W���v�Z���Ă��܂��B
		require(pROC)
		roc.object <- roc(response, prediction)
		if (any(c("informedness", "mcc", coords.ret) %in% cv.metrics)){
			result <- calc.coords.metrics(roc.object, coords.ret)
			result <- cbind(result, informedness = calc.informedness(result))
			result <- cbind(result, mcc = calc.mcc(result))
		}
		result <- cbind(result, auc = roc.object$auc)
	}
	result <- cbind(result, mse = calc.mse(response, prediction))
	result <- cbind(result, rmse = calc.rmse(response, prediction))
	result <- cbind(
		result, r.squared = calc.r.squared(response, prediction, cor.method)
	)
	# ���ʂ𐮌`
	rownames(result) <- NULL
	result <- data.frame(result)
	result = result[cv.metrics]			# �K�v�Ȏw�W�����Ԃ��B
	# Youden��J�ōœK��臒l�����܂炸�A���ʂ������ɂȂ����Ƃ��Aprediction�𕡐�
	prediction <- do.call(cbind, rep(list(prediction), nrow(result)))
	return(list(metrics = result, cv.prediction = prediction))
}
