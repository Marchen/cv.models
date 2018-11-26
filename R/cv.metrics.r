#------------------------------------------------------------------------------
#	An R6 Class calculating accuracy metrics of the model.
#------------------------------------------------------------------------------
cv.metrics.calculator <- R6::R6Class("cv.metrics.calculator")


#------------------------------------------------------------------------------
#	Join vectors of factor.
#
#	Args:
#		x:
#			a list of factors.
#
#	Returns:
#		vector of factor.
#------------------------------------------------------------------------------
cv.metrics.calculator$set(
	"private", "join.factor",
	function(x) {
		original.levels <- unique(lapply(x, levels))
		if (length(original.levels) != 1) {
			stop("By unknown reason, factor levels was changed.")
		} else {
			original.levels <- original.levels[[1]]
		}
		x <- lapply(x, as.character)
		x <- do.call(c, x)
		x <- factor(x, levels = original.levels)
		return(x)
	}
)


#------------------------------------------------------------------------------
#	Join results of cross validation.
#
#	This method join results of each fold of cross validation.
#	The resultant object is used in the case aggregate.method = "join".
#
#	Args:
#		fits:
#			result of cross validation.
#			Usually 'fits' field of cv.models object.
#
#	Details:
#		This method convert the structure of the fits.
#			list(
#				list(response1, prediction1, index1),
#				list(response2, prediction2, index2),
#				...
#			)
#			->
#			list(
#				list(
#					c(response1, response2, ...)
#					c(prediction1, prediction2, ...)
#					c(index1, index2, ...)
#				)
#			)
#------------------------------------------------------------------------------
cv.metrics.calculator$set(
	"private", "join.fits",
	function(fits) {
		result <- list()
		for (i in names(fits[[1]])) {
			current.field <- lapply(fits, "[[", i = i)
			data.is.factor <- any(sapply(current.field, is.factor))
			if (all(sapply(current.field, is.matrix))) {
				result[[i]] <- do.call(rbind, current.field)
			} else {
				if (data.is.factor) {
					result[[i]] <- private$join.factor(current.field)
				} else {
					result[[i]] <- do.call(c, current.field)
				}
			}
		}
		return(list(result))
	}
)


#------------------------------------------------------------------------------
#	Create metrics for all folds and returns it as a list of matrix/matrices.
#
#	Args:
#		fits:
#			result of cross validation. Usually 'fits' field of cv.models
#			object.
#		cal:
#			classification.metrics.calculator or
#			regression.metrics.calculator object.
#------------------------------------------------------------------------------
cv.metrics.calculator$set(
	"private", "calculate.metrics.of.folds",
	function(fits, cal) {
		metrics.of.folds <- lapply(fits, cal$calculate.metrics)
		metrics.of.folds <- swap.list.hierarchy(metrics.of.folds)
		metrics.table <- lapply(metrics.of.folds, do.call, what = rbind)
		return(metrics.table)
	}
)


#------------------------------------------------------------------------------
#	Calculate all metrics using single method of optimal.cutpoints.
#
#	Args:
#		fits:
#			result of cross validation. Usually 'fits' field of cv.models
#			object.
#		cal:
#			classification.metrics.calculator or
#			regression.metrics.calculator object.
#------------------------------------------------------------------------------
cv.metrics.calculator$set(
	"private", "aggregate.folds",
	function(metrics.tables) {
		# Calculate mean and SD of the metrics.
		result.mean <- lapply(metrics.tables, colMeans)
		result.sd <- lapply(metrics.tables, apply, 2, sd)
		for (i in 1:length(result.sd)) {
			names(result.sd[[i]]) <- paste0("sd.", names(result.sd[[i]]))
		}
		return(mapply(c, result.mean, result.sd, SIMPLIFY = FALSE))
	}
)


#------------------------------------------------------------------------------
#	Calculate all metrics for all methods of optimal.cutpoints.
#
#	Args:
#		object:
#			a cv.models object.
#------------------------------------------------------------------------------
cv.metrics.calculator$set(
	"public", "calculate.metrics",
	function(object, fits) {
		# If aggregate.method is "join", combine result of each fold.
		if (object$aggregate.method == "join") {
			fits <- lapply(fits, private$join.fits)
		}
		# Calculate metrics for each fold.
		if (object$adapter$model.type == "regression") {
			cal <- regression.metrics.calculator$new()
		} else {
			cal <- classification.metrics.calculator$new(object)
		}
		# Calculate metrics for all folds.
		# 'metrics' can be list with length > 1 (multiple sets of folds)
		# because multiple optimal threshold can be calculated by
		# OptimalCutpoints.
		metrics <- lapply(
			fits, private$calculate.metrics.of.folds, cal = cal
		)
		if (object$aggregate.method %in% c("mean", "join")) {
			metrics <- lapply(metrics, private$aggregate.folds)
			metrics <- swap.list.hierarchy(metrics)
		}
		result <- lapply(
			metrics, function(x) as.data.frame(do.call(rbind, x))
		)
		return(result)
	}
)


#------------------------------------------------------------------------------
#'	Calculate model evaluation metrics.
#'
#'	This function calculates several model evaluation metrics for both
#'	regression and classification models.
#'
#'	@param object
#'		a \code{cv.models} object.
#------------------------------------------------------------------------------
cv.metrics <- function(object, fits) {
	cal <- cv.metrics.calculator$new()
	return(cal$calculate.metrics(object, fits))
}
