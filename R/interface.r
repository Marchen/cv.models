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
}