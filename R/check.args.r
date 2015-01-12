
#-------------------------------------------------------------------------------
#	cv.models関数の引数の整合性をチェックする。
#
#	Args:
#		cv.modelsと同じ。
#-------------------------------------------------------------------------------
check.args <- function(
	model.function, args.model, data, args.predict = list(), cv.folds = 10,
	cv.metrics = c("auc"), n.cores = NULL, seed = NULL,
	function.name = as.character(substitute(model.function)),
	package.name = get.package.name(function.name),
	args.dredge = NULL
){
	# dredgeが指定が指定されたけど、関数が対応していないときに警告を出す。
	if (
		!is.null(args.dredge)
		& function.name %in% c("lm", "glm", "lme", "lmer", "glmer", "gam", "gamm")
	){
		warnings(
			sprintf("'dredge' may not be compatible for '%1'", function.name)
		)
	}
}
