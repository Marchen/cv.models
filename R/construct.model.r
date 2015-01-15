

#-------------------------------------------------------------------------------
#	cv.models�I�u�W�F�N�g���烂�f�����\�z����⏕�֐��B
#
#	Args:
#		x: cv.models�I�u�W�F�N�g
#		metrics.index:
#			�����̃p�����[�^�[��₪����Ƃ��ɂ́A���f���\�z�Ɏg���p�����[�^�[��
#			�l���܂܂�Ă���cv.metrics�̃C���f�b�N�X���w�肷��B
#			���p�����[�^�[���Ȃ����1���w�肷���OK�B
#	Values:
#		�ȉ��̕ϐ����܂܂ꂽ���X�g�B
#			model: �\�z�������f���̃I�u�W�F�N�g�B
#			cv.metrics: ���̃��f���̃N���X�o���f�[�V�������\�w�W�B
#			cv.prediction: �N���X�o���f�[�V�����Ŏw�W���v�Z����Ƃ��̗\���l�B
#			function.name: �Ăяo�����֐��̖��O�B
#-------------------------------------------------------------------------------
construct.model <- function(x, metrics.index){
	# ���f���ɓn������������
	set.seed(x$seed)
	args <- x$args.model
	args$data <- x$data
	tunable.args <- get.tunable.args(
		make.dummy(x$function.name), x$cv.metrics[metrics.index, ], "model"
	)
	args[names(tunable.args)] <- tunable.args
	# ���f�����\�z
	result <- list(
		model = do.call(x$function.name, args),
		cv.metrics = x$cv.metrics[metrics.index, ],
		cv.prediction = x$cv.prediction[, metrics.index],
		cv.response = x$cv.response[, metrics.index],
		function.name = x$function.name
	)
	return(result)
}