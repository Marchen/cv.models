#-----------------------------------------------------------------------------
#'	Extract predicted values
#'
#'	Extract predicted values from a \code{cv.models}, \code{cv.result} or
#'	\code{cv.best.models} object.
#'
#'	@param object
#'	an object of \code{cv.models}, \code{cv.result} or \code{cv.best.models}.
#'
#'	@param index
#'	an integer specifying index of model to extract.
#'	If the \code{object} is \code{cv.models}, index in the models created by
#'	hyper-parameter tuning.
#'	If the \code{object} is \code{cv.best.models}, index in the models having
#'	same performance measure.
#'	Ignored if the \code{object} is \code{cv.result}.
#'
#'	@return a data.frame having original response variable ("response"),
#'	predicted values ("prediction"), and index in the original data ("index").
#'
#'	@export
#-----------------------------------------------------------------------------
extract.fit <- function(object, index = 1) {
	UseMethod("extract.fit")
}

#-----------------------------------------------------------------------------
#'	@export
#'	@method extract.fit cv.models
#	@describeIn extract. fitS3 method for \code{cv.models}.
#-----------------------------------------------------------------------------
extract.fit.cv.models <- function(object, index = 1) {
	fits <- lapply(object$cv.results[[index]]$fits, as.data.frame)
	fits <- do.call(rbind, fits)
	return(fits)
}

#-----------------------------------------------------------------------------
#'	@export
#'	@method extract.fit cv.result
#	@describeIn extract.fit S3 method for \code{cv.result}.
#-----------------------------------------------------------------------------
extract.fit.cv.result <- function(object, index = 1) {
	dfs <- lapply(object$fits, as.data.frame)
	result <- do.call(rbind, dfs)
	return(result)
}

#-----------------------------------------------------------------------------
#'	@export
#'	@method extract.fit cv.best.models
#'	@describeIn extract.fit S3 method for \code{cv.best.models}.
#-----------------------------------------------------------------------------
extract.fit.cv.best.models <- function(object, index = 1) {
	return(extract.fit(object[[index]]))
}


#-----------------------------------------------------------------------------
#'	Plot predicted and actual values.
#'
#'	Draw scatter plot for predicted and actual values.
#'
#'	@param x
#'	a \code{cv.models} or \code{cv.best.models} object.
#'
#'	@param index
#'	an integer specifying index of the model.
#'	For the detail, see \code{index} argument of \code{\link{extract.fit}}.
#'
#'	@param ...
#'	other arguments passed to plot.
#'
#'	@export
#-----------------------------------------------------------------------------
plot.cv.models <- function(x, index = 1, ...) {
	fits <- extract.fit(x, index)
	graphics::plot(
		fits$prediction, fits$response, xlab = "Prediction", ylab = "Reponse",
		...
	)
	graphics::curve(x * 1, add = TRUE)
}


#------------------------------------------------------------------------------
#'	Extract performance measures
#'
#'	@param object
#'	a \code{cv.models} object.
#'
#'	@return data.frame having all performance measures.
#'
#'	@export
#------------------------------------------------------------------------------
extract.metrics <- function(object) {
	metrics <- lapply(object$cv.results, "[[", "metrics")
	result <- do.call(rbind, metrics)
	rownames(result) <- NULL
	return(result)
}


#------------------------------------------------------------------------------
#'	print method
#'	@param x a \code{cv.result} object.
#'	@param ... currently not used.
#'	@export
#------------------------------------------------------------------------------
print.cv.result <- function(x, ...) {
	cat("Result of cross validation\n")
	cat(sprintf("Function name: %s\n", x$function.name))
	cat("Cross validation metrics:\n")
	print(x$metrics)
	cat("\n")
}

#------------------------------------------------------------------------------
#'	print method
#'	@param x a \code{cv.best.models} object.
#'	@param ... currently not used.
#'	@export
#------------------------------------------------------------------------------
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

#------------------------------------------------------------------------------
#'	print method for \emph{cv.models} class.
#'	@param x a \code{cv.best.models} object.
#'	@param ... currently not used.
#'	@export
#------------------------------------------------------------------------------
print.cv.models <- function(x, ...) {
	cat("Result of cross validation\n")
	cat(sprintf("Function name: %s\n", x$function.name))
	cat("Cross validation metrics:\n")
	print(extract.metrics(x))
	cat("\n")
}

