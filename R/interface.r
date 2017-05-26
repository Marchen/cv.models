#'	@export
extract.fit <- function(object, index = 1){
	model.index <- calculate.model.index(object, index)
	predict.index <- calculate.predict.index(object, index)
	fits <- object$cv.results[[model.index]]$fits[[predict.index]]
	fits <- lapply(fits, as.data.frame)
	fits <- do.call(rbind, fits)
	return(fits)
}

#'	@export
plot.cv.models <- function(object, index = 1, ...){
	fits <- extract.fit(object, index)
	plot(
		fits$prediction, fits$response, xlab = "Prediction", ylab = "Reponse",
		...
	)
	curve(x * 1, add = TRUE)
}

#'	@export
#------------------------------------------------------------------------------
extract.metrics <- function(object) {
	metrics <- lapply(object$cv.results, "[[", "metrics")
	result <- do.call(rbind, metrics)
	rownames(result) <- NULL
	return(result)
}#'	print method for \emph{cv.models} class.
#'	@describeIn cv.models
#'	@export
#------------------------------------------------------------------------------
#	cv.modelsクラス用のprint。
#
#	Args:
#		x: cv.modelsオブジェクト。
#		...: 使われていません。
#------------------------------------------------------------------------------
print.cv.models <- function(x, ...) {
	cat("Result of cross validation\n")
	cat(sprintf("Function name: %s\n", x$function.name))
	cat("Cross validation metrics:\n")
	print(extract.metrics(x))
	cat("\n")
}
