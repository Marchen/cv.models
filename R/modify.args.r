
#-------------------------------------------------------------------------------
#	cv.models関数の引数の整合性をチェックする。
#
#	Args:
#		cv.modelsと同じ。
#-------------------------------------------------------------------------------
check.consistency <- function(
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

modify.args <- function(
	check.args, cv.dummy, args.model, args.predict, data
){
	if (check.args){
		args.model <- modify.args.model(
			cv.dummy, args.model, args.predict, data
		)
		data[[get.response.name(cv.dummy, args.model)]] <- modify.response.var(
			cv.dummy, data[[get.response.name(cv.dummy, args.model)]]
		)
		args.predict <- modify.args.predict(
			cv.dummy, args.model, args.predict, data
		)
	}
	modified <- list(
		args.model = args.model, args.predict = args.predict, data = data
	)
	return(modified)
}


