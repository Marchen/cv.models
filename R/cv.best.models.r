#===============================================================================
#	cv.best.models.r
#
#	Auther: Michio Oguro
#
#	Description:
#		Extract model(s) with best predictive ability from cv.models object.
#===============================================================================
#	cv.modelsオブジェクトから予測力最大のモデルを取り出す。
#===============================================================================


#-------------------------------------------------------------------------------
#'	(Internal) Find the Index of Best Metrics
#'
#'	This function find the index of metrics indicating best predictive ability.
#'
#'	@param metrics
#'		a matrics of model performance metrics.
#-------------------------------------------------------------------------------
#	最適な指標のインデックスを取り出す補助関数。
#
#	Args:
#		metrics:
#			モデルパフォーマンス指標を格納した行列。
#-------------------------------------------------------------------------------
find.best.metrics.index <- function(metrics) {
	minimize <- c(
		"mse", "rmse", "fn", "fp",
		"1-specificity", "1-sensitivity", "1-accuracy", "1-npv", "1-ppv"
	)
	for (i in colnames(metrics)) {
		if (i %in% minimize) {
			metrics[[i]] <- -metrics[[i]]
		}
	}
	return(which.max.multi(metrics))
}


#-------------------------------------------------------------------------------
#'	(Internal) Extract a Model from cv.models Object
#'
#'	This function extract a model from \code{\link{cv.models}} object and make
#'	a \code{\link{cv.best.model}} object.
#'
#'	@export
#-------------------------------------------------------------------------------
extract.model <- function(object, index) {
	model.index <- calculate.model.index(object, index)
	predict.index <- calculate.predict.index(object, index)
	best <- object
	best$grid <- NULL
	best$grid.predict <- NULL
	best$metrics <- extract.metrics(best)[index, ]
	best$call <- best$cv.results[[model.index]]$call
	best$model <- eval(best$call, envir = best$envir)
	best$fits <- best$cv.results[[model.index]]$fits
	best$cv.group <- best$cv.results[[model.index]]$cv.group
	best$cv.results <- NULL
	class(best) <- "cv.best.model"
	return(best)
}

#'	@export
cv.best.models <- function(object, metrics) {
	best.index <- get.best.metrics.index(extract.metrics(object)[metrics])
	return(lapply(best.index, extract.model, object = object))
}


#'	@export
print.cv.best.model <- function(x, ...) {
	cat("Result of cross validation\n")
	cat(sprintf("Function name: %s\n", x$function.name))
	cat("Cross validation metrics:\n")
	print(x$metrics)
	cat("\n")
}
