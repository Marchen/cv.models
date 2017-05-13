
model.perf <- function(
	model.function, args.model, data, args.predict = list(), cv.folds = 10,
	cv.metrics = c("auc"), n.cores = NULL, seed = NULL, positive.class = NULL,
	dredge = NULL, check.args = TRUE,
	function.name = as.character(substitute(model.function)),
	package.name = get.package.name(function.name)

){
	settings <- model.settings()
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


