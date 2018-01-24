#'	@export
extract.fit <- function(object, index = 1) {
	UseMethod("extract.fit")
}

#'	@export
extract.fit.cv.models <- function(object, index = 1) {
	fits <- lapply(object$cv.results[[index]]$fits, as.data.frame)
	fits <- do.call(rbind, fits)
	return(fits)
}

#'	@export
extract.fit.cv.result <- function(object, index = 1) {
	dfs <- lapply(object$fits, as.data.frame)
	result <- do.call(rbind, dfs)
	return(result)
}

#'	@export
extract.fit.cv.best.models <- function(object, index = 1) {
	return(extract.fit(object[[index]]))
}

#'	@export
plot.cv.models <- function(object, index = 1, ...) {
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

#'	@export
print.cv.result <- function(x, ...) {
	cat("Result of cross validation\n")
	cat(sprintf("Function name: %s\n", x$function.name))
	cat("Cross validation metrics:\n")
	print(x$metrics)
	cat("\n")
}


#'	@export
print.cv.best.models <- function(x, ...) {
	hr <- "------------------------------------------------"
	if (length(x) > 1) {
		msg <- sprintf("-> %s best models were found:", length(x))
	} else {
		msg <- "-> 1 best models was found:"
	}
	msg <- paste(
		hr, "Result of model selection by cross validation", msg, hr,
		sep = "\n"
	)
	cat(msg)
	for (i in 1:length(x)) {
		cat(paste(sprintf("\nModel index %s", i), hr, sep = "\n"), "\n")
		print(x[[i]])
	}
}

#'	print method for \emph{cv.models} class.
#'	@describeIn cv.models print method for cv.models.
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

