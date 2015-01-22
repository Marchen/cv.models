#-------------------------------------------------------------------------------
#	cv.models�I�u�W�F�N�g����邽�߂̕⏕�֐��B
#
#	Args:
#		�e�p�����[�^�[�ɑΉ��B�ڍׂ́��Q�ƁB
#
#	Value:
#		cv.models�I�u�W�F�N�g�B
#-------------------------------------------------------------------------------
cv.models.object <- function(
	model.function, function.name, package.name, data, args.model, args.predict,
	cv.performance, seed, positive.class
){
	object <- list(
		model.function = model.function, function.name = function.name,
		package.name = package.name, data = data,
		args.model = args.model, args.predict = args.predict,
		cv.metrics = cv.performance$metrics,
		cv.prediction = cv.performance$prediction,
		cv.response = cv.performance$response,
		confusion.matrix = cv.performance$confusion.matrix,
		seed = seed, positive.class = positive.class
	)
	class(object) <- "cv.models"
	return(object)
}

#'	Cross validation and parameter selection.
#'	@export
#-------------------------------------------------------------------------------
#	���f���̐��\�ɉe������p�����[�^�[�̌���g�ݍ��킹�ă��f�������A
#	�N���X�o���f�[�V�����Ő��\�]�����s���B
#
#	Args:
#		model.function: ���f�����쐬����֐��B
#		args.model:
#			���f���̍\�z�ɓn���������B
#			gbm�̂悤�Ƀ��f���̐��\�ɉe������p�����[�^�[���x�N�g���ŕ����w��
#			����ƁA���ꂼ��̌��ɑ΂��Ďw�W���v�Z�����Bdata�͎����I��
#			�u����������̂ŁA�w�肵�Ă��Ӗ�������܂���B
#		data:
#			���f���쐬�Ɏg���f�[�^�B����𕪊����ăN���X�o���f�[�V�������s���B
#		args.predict:
#			predict�֐��ɓn���������Bgbm��n.trees�̂悤�Ƀ��f���̐��\�ɉe��
#			����p�����[�^�[��get.tunable.args()�֐��őΉ����邱�ƂŁA�����̒l
#			���ꂼ��ɑ΂��Ďw�W���v�Z���邱�Ƃ��o���܂��B
#		cv.folds: �N���X�o���f�[�V�����̕������B
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
#		n.cores:
#			�v�Z�Ɏg���R�A�̐��B�����w�肵�Ȃ��ƑS�ẴR�A���g���Čv�Z���܂��B
#		seed:
#			���ʂ��Œ肵�����Ƃ��ɂ͗����̎���w�肷��B�����Ŏ�q���Œ肷��ƁA
#			get.best.models()�֐��̌��ʂ��Œ肳���B��q���Œ肷��ƌ��ʂ�
#			�N���X�^�[���g���Ă��g��Ȃ��Ă������ɂȂ�B
#		positive.lael:
#			�z���Ƃ��Ĉ����N���X��\��������B�w�肳��Ȃ������ꍇ��
#			(TRUE, FALSE), (1, 0), (+, -), (+, 0)�̃Z�b�g�̍�����z���Ƃ��Ĉ����A
#			�������I�ɗz���Ƃ��ăf�[�^�̎擾�����݂�B����ł��z��������ł��Ȃ�
#			�ꍇ�A�N���X�̂P�Ԗڂ�z���Ƃ��Ĉ����B
#			���q���R�N���X�ȏゾ�����ꍇ�A�P��ڂ̊m����p����B
#		check.args:
#			���ꂪTRUE���Ɛ��������ʂ�������悤�ɁA���f���\�z�A����l�v�Z��
#			�g����p�����[�^�[���C�����A�����ϐ��̌^�̕ϊ����s���܂��B
#			FALSE�ɂ���ƁA���f���\�z�Epredict�֐��̑S�Ă̋����̓��[�U�[�̎w�肵��
#			�p�����[�^�[�̂܂܂ɂȂ�A�������̃`�F�b�N���s���܂���B
#		function.name:
#			���f���\�z�Ɏg����֐����B�ʏ�͎����I�ɐݒ肳���̂Ŏw�肷��K�v
#			�͂���܂���icv.models���炱�̊֐����Ăяo���Ƃ��̂��߂Ɏ��������
#			���܂��j�B
#		package.name:
#			���f���\�z�Ɏg����֐��̓������p�b�P�[�W���B�ʏ�͎����I�ɐݒ�
#			�����̂ŁA�w�肷��K�v�͂���܂���Bgam�p�b�P�[�W��mgcv�p�b�P�[�W
#			��gam�֐��̂悤�ɁA�قȂ�p�b�P�[�W�ɓ����Ă��铯���̊֐���
#			�Ăяo�������Ƃ��A�����I�Ƀp�b�P�[�W�����w�肷�邽�߂Ɏg���܂��B
#
#	Value:
#		cv.models�I�u�W�F�N�g�B�ȉ��̒l�����B
#			model.function: ���f�����쐬����Ƃ��Ɏg���֐��B
#			function.name: ���f�����쐬����֐����B
#			package.name: ���f���쐬�֐����܂܂�Ă���p�b�P�[�W�̖��O�B
#			data: �N���X�o���f�[�V�����Ɏg��ꂽ�f�[�^�B
#			args.model: ���f���\�z�ɓn���ꂽ�p�����[�^�[�̌��B
#			args.predict: predict�֐��ɓn���ꂽ�p�����[�^�[�̌��B
#			cv.metrics: �N���X�o���f�[�V�����Ōv�Z���ꂽ���\�]���w�W�B
#			cv.prediction: �N���X�o���f�[�V�����Ōv�Z���ꂽ�\���l�B
#			cv.response: �N���X�o���f�[�V�����Ɏg��ꂽ�����ϐ��̒l�B
#			seed: �v�Z�Ɏg���������̎�q�B
#			positive.class: �z���Ƃ��Ĉ����N���X�̃��x���B
#-------------------------------------------------------------------------------
cv.models <- function(
	model.function, args.model, data, args.predict = list(), cv.folds = 10,
	cv.metrics = c("auc"), n.cores = NULL, seed = NULL, positive.class = NULL,
	dredge = NULL, check.args = TRUE,
	function.name = as.character(substitute(model.function)),
	package.name = get.package.name(function.name)

){
	# �p�����[�^�[����������ۂ悤�ɏC������B
	dummy <- make.dummy(function.name, package.name)
	modified <- modify.args(check.args, dummy, args.model, args.predict, data)
	# �p�����[�^�[���̑g�ݍ��킹�����B
	expanded.args <- expand.tunable.args(dummy, modified$args.model, "model")
	# ���p�����[�^�[�̐��ɂ���āA����v�Z����ꏊ��ς���B
	cores <- assign.cores(expanded.args, n.cores)
	# ���f���̐��\���N���X�o���f�[�V�����B
	cl <- init.cluster(cores$param.tune)
	on.exit(cl$close())
	cl$library(package.name)
	performance <- cl$lapply(
		expanded.args, cross.validation, model.function = model.function,
		data = modified$data, args.predict = modified$args.predict,
		cv.folds = cv.folds, cv.metrics = cv.metrics, n.cores = cores$cv,
		seed = seed, positive.class = positive.class, cv.dummy = dummy
	)
	cl$close()
	# ���p�����[�^�[��CV�̌��ʂɌ����B
	performance <- merge.tunable.args(dummy, performance, args.model, "model")
	performance <- merge.cv.performances(performance)	
	# cv.models�I�u�W�F�N�g���쐬�B
	result <- cv.models.object(
		model.function, function.name, package.name, modified$data,
		modified$args.model, modified$args.predict, performance, seed,
		positive.class
	)
	return(result)
}

#'	@describeIn cv.models print method for \emph{cv.models} class.
#'	@export
#-------------------------------------------------------------------------------
#	cv.models�N���X�p��print�B
#
#	Args:
#		x: cv.models�I�u�W�F�N�g�B
#		...: �g���Ă��܂���B
#-------------------------------------------------------------------------------
print.cv.models <- function(x, ...){
	cat("Result of cross validation\n")
	cat(sprintf("Function name: %s\n", x$function.name))
	cat("Cross validation metrics:\n")
	print(x$cv.metrics)
	cat("\n")
}
