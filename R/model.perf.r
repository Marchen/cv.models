
model.perf <- function(
	model.function, args.model, data, args.predict = list(), cv.folds = 10,
	cv.metrics = c("auc"), n.cores = NULL, seed = NULL, positive.class = NULL,
	dredge = NULL, check.args = TRUE,
	function.name = as.character(substitute(model.function)),
	package.name = get.package.name(function.name)

){
	settings <- model.settings()
	# パラメーターが整合性を保つように修正する。
	dummy <- make.dummy(function.name, package.name)
	modified <- modify.args(check.args, dummy, args.model, args.predict, data)
	# パラメーター候補の組み合わせを作る。
	expanded.args <- expand.tunable.args(dummy, modified$args.model, "model")
	# 候補パラメーターの数によって、並列計算する場所を変える。
	cores <- assign.cores(expanded.args, n.cores)
	# モデルの性能をクロスバリデーション。
	cl <- init.cluster(cores$param.tune)
	on.exit(cl$close())
	cl$library(package.name)
	performance <- cl$lapply(
		expanded.args, cross.validation, model.function = model.function,
		data = modified$data, args.predict = modified$args.predict,
		cv.folds = cv.folds, cv.metrics = cv.metrics, n.cores = cores$cv,
		seed = seed, positive.class = positive.class, cv.dummy = dummy
	)
	# 候補パラメーターをCVの結果に結合。
	performance <- merge.tunable.args(dummy, performance, args.model, "model")
	performance <- merge.cv.performances(performance)	
	# cv.modelsオブジェクトを作成。
	result <- cv.models.object(
		model.function, function.name, package.name, modified$data,
		modified$args.model, modified$args.predict, performance, seed,
		positive.class
	)
	return(result)
}


