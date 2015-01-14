
#-------------------------------------------------------------------------------
#	�n���ꂽ�ݒ�Ń��f�������A�N���X�o���f�[�V�������s���B
#
#	Args:
#		model.function: ���f�����쐬����֐��B
#		args.model:
#			���f�����쐬����֐��ɓn���������Bdata��CV���s�����߂ɓ����I��
#			�u����������̂ŁA�w�肵�Ă��Ӗ�������܂���B
#		���̂ق���cv.models�֐��̐��������Ă��������B
#
#	Value:
#		�ȉ��̒l���܂܂ꂽ���X�g�B
#		metrics:
#			�v�Z�����w�W���������s��(matrix)��Ԃ��܂��B�e�񂪊e�w�W�ł��B
#			gbm�̂悤��predict�֐��Ő��\�ɉe������p�����[�^�[��ݒ�ł���֐�
#			�ł͂��̃p�����[�^�[���ƂɌv�Z�����w�W���s�ŕԂ��Ă��܂��B
#			�w�W�̌v�Z�̍ۂ�Youden�̕��@�ōœK��臒l���v�Z�ł��Ȃ������Ƃ��ɂ�
#			������̎w�W���Ԃ��Ă��܂��B
#		cv.prediction:
#			�w�W�v�Z�̌��ɂȂ����N���X�o���f�[�V�����̗\���l������܂��B
#-------------------------------------------------------------------------------
cross.validation <- function(
	model.function, args.model, data, args.predict = list(), cv.folds = 10,
	cv.metrics = c("auc"), n.cores = NULL, seed = NULL, positive.label = NULL,
	check.args = TRUE,
	function.name = as.character(substitute(model.function)),
	package.name = get.package.name(function.name)
){
	# �v�Z�N���X�^�[��������
	cl <- init.cluster(n.cores)
	cl$library(package.name)
	# �N���X�o���f�[�V�����ŗ\���l���v�Z
	set.seed(seed)
	args.model <- modify.args.model(
		make.dummy(function.name), args.model, args.predict, check.args
	)
	cv.result <- cl$lapply(
		1:cv.folds, cv.one.fold, model.function = model.function,
		args.model = args.model, args.predict = args.predict, data = data,
		cv.group = make.cv.group(data, cv.folds), seed = seed,
		positive.label = positive.label, check.args = check.args
	)
	cv.result <- do.call(rbind, cv.result)
	# �\���l���烂�f���̐��\�]���w�W���v�Z
	metrics <- cl$lapply(
		cv.result[-1], calc.cv.metrics, response = cv.result[[1]],
		cv.metrics = cv.metrics
	)
	cl$close()
	# ���ʂ𐮌`
	cv.metrics <- merge.tunable.args(
		function.name, lapply(metrics, "[[", "metrics"), args.predict, "predict"
	)
	cv.metrics <- do.call(rbind, cv.metrics)
	row.names(cv.metrics) <- NULL
	cv.prediction = do.call(cbind, lapply(metrics, "[[", "cv.prediction"))
	return(list(cv.metrics = cv.metrics, cv.prediction = cv.prediction))
}
