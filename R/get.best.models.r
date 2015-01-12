

#-------------------------------------------------------------------------------
#	cv.models�I�u�W�F�N�g����metrics�Ŏw�肵���w�W�ōł����т̂悢���f����
#	���o���Bcv.models�I�u�W�F�N�g�Emetrics�͕����w��\�B
#
#	Args:
#		...: cv.models�I�u�W�F�N�g�B
#		metrics:
#			���f���ԂŔ�r���鐫�\�w�W�̖��O��������������x�N�g���B
#			�����w�肷��ƒl���^�C�������Ƃ��ɏ��Ԃɕ]�������B
#
#	Value:
#		�ȉ��̍\����cv.best.models�I�u�W�F�N�g�B
#		�^�C������ƃ��f���������ɂȂ�\��������̂ŁAconstruct.model()�֐���
#		���ʂ����X�g�ɓ���ĕԂ��B
#			list(
#				list(model, cv.metrics, function.name),
#				list(model, cv.metrics, function.name), ...
#			)
#			model: �\�z�������f���̃I�u�W�F�N�g�B
#			cv.metrics: ���̃��f���̃N���X�o���f�[�V�������\�w�W�B
#			cv.prediction: �N���X�o���f�[�V�����Ɏg�����\���l�B
#			function.name: �Ăяo�����֐��̖��O�B
#-------------------------------------------------------------------------------
get.best.models <- function(..., metrics = "auc"){
	# �����w�肳�ꂽcv.models�I�u�W�F�N�g�̒�����A���ꂼ��ōœK�Ȏw�W�����o���B
	# �w�W�̑��ɉ��Ԗڂ̎w�W����\��metrics.index���ǉ�����B
	get.best.metrics <- function(x, metrics){
		if (!is(x, "cv.models")) stop("Please specify 'cv.models' object!")
		met <- cbind(x$cv.metrics, metrics.index = 1:nrow(x$cv.metrics))
		met <- met[
			get.best.metrics.index(met[metrics]), c(metrics, "metrics.index")
		]
		return(met)
	}
	objects <- list(...)
	best.metrics <- lapply(objects, get.best.metrics, metrics)
	# ���Ԗڂ̃��f���I�u�W�F�N�g����\��object.index��ǉ����Č����B
	best.metrics <- mapply(
		cbind, best.metrics, object.index = 1:length(objects), SIMPLIFY = FALSE
	)
	best.metrics <- do.call(rbind, best.metrics)
	# �œK�ȃ��f���̎w�W���v�Z
	best.index <- get.best.metrics.index(best.metrics[metrics])
	best.metrics <- best.metrics[best.index, ]
	# ���f�����\�z
	object.index <- best.metrics[, "object.index"]
	metrics.index <- best.metrics[, "metrics.index"]
	make.models <- function(objects, object.index, metrics.index){
		return(construct.model(objects[[object.index]], metrics.index))
	}
	models <- mapply(
		make.models, object.index, metrics.index,
		MoreArgs = list(objects = objects), SIMPLIFY = FALSE
	)
	class(models) <- "cv.best.models"
	return(models)
}
