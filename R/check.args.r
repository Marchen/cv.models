
#-------------------------------------------------------------------------------
#	cv.models�֐��̈����̐��������`�F�b�N����B
#
#	Args:
#		cv.models�Ɠ����B
#-------------------------------------------------------------------------------
check.args <- function(
	model.function, args.model, data, args.predict = list(), cv.folds = 10,
	cv.metrics = c("auc"), n.cores = NULL, seed = NULL,
	function.name = as.character(substitute(model.function)),
	package.name = get.package.name(function.name),
	args.dredge = NULL
){
	# dredge���w�肪�w�肳�ꂽ���ǁA�֐����Ή����Ă��Ȃ��Ƃ��Ɍx�����o���B
	if (
		!is.null(args.dredge)
		& function.name %in% c("lm", "glm", "lme", "lmer", "glmer", "gam", "gamm")
	){
		warnings(
			sprintf("'dredge' may not be compatible for '%1'", function.name)
		)
	}
}
