#------------------------------------------------------------------------------
#	Hierarcy of calculation.
#
#	cv.models
#		lapply(fit.cv.models)
#			lapply(model.one.fold)
#			cv.metrics
#				cv.metrics.calculator$calculate.metrics
#					lapply($calculate.metrics.of.single.result)
#						lapply(cal.object$calculate.metrics)
#------------------------------------------------------------------------------

#------------------------------------------------------------------------------
apply.grid <- function(x, grid) {
	if (is.null(grid)) {
		return(list(x))
	}
	set.values <- function(index, x, grid) {
		for (i in colnames(grid)) {
			x[[i]] <- grid[index, i]
		}
		return(x)
	}
	grid <- expand.grid(grid)
	result <- lapply(1:nrow(grid), set.values, x = x, grid = grid)
	return(result)
}


#------------------------------------------------------------------------------
#	Using the parameter grid in the object, create list of cv.models objects
#	having different calls.
#	If the object does not have parameter grid, simply returns the object
#	in a list.
#
#	Input: cv.models
#	Output: list(cv.models, cv.models, ...)
#------------------------------------------------------------------------------
apply.grid.for.object <- function(object) {
	if (is.null(object$grid)) {
		return(list(object))
	}
	calls <- apply.grid(object$call, object$grid)
	assign.call <- function(call, object) {
		object$call <- call
		return(object)
	}
	objects <- lapply(calls, assign.call, object)
	return(objects)
}


#------------------------------------------------------------------------------
merge.grid.and.metrics <- function(grid, metrics) {
	if (is.null(grid)) {
		return(metrics)
	}
	grid <- expand.grid(grid)
	metrics <- cbind(grid, metrics)
	return(metrics)
}


#------------------------------------------------------------------------------
merge.grid.and.cv.results <- function(grid, cv.results) {
	if (is.null(grid)) {
		return(cv.results)
	}
	grid <- expand.grid(grid)
	grids <- lapply(1:nrow(grid), "[.data.frame", x = grid, )
	fun <- function(grid, cv.results) {
		result <- vector("list", length(cv.results))
		for (i in 1:length(cv.results)) {
			cv.results[[i]]$metrics <- merge.grid.and.metrics(
				grid, cv.results[[i]]$metrics
			)
		}
		return(cv.results)
	}
	return(mapply(fun, grids, cv.results, SIMPLIFY = FALSE))
}


#------------------------------------------------------------------------------
#	Returns:
#		list(response, prediction, original.row.index)
#------------------------------------------------------------------------------
make.prediction <- function(predict.args, model, object, row.index) {
	adapter <- model.adapter::model.adapter$new(model, envir = object$envir)
	fit <- do.call(adapter$predict, predict.args)$fit
	if (adapter$model.type == "regression") {
		fit <- fit[, "fit"]
		attributes(fit) <- NULL
	}
	# Make result.
	response <- object$adapter$y.vars[row.index, ]
	result <- list(
		response = response, prediction = fit, index = row.index
	)
	return(result)
}


#------------------------------------------------------------------------------
#	Returns:
#		list(
#			list(response, prediction, original.row.index),
#			list(response, prediction, original.row.index),
#			...
#		)
#		Each element of the list represents result for element in grid.predict.
#------------------------------------------------------------------------------
model.one.fold <- function(cv.index, object) {
	# Fix random number before using random process.
	set.seed.if.possible(object)
	# Create data for model construction.
	data.test <- object$adapter$data[object$cv.group == cv.index, ]
	data.train <- object$adapter$data[object$cv.group != cv.index, ]
	row.index <- (1:nrow(object$adapter$data))[object$cv.group == cv.index]
	# Construct model.
	child.env <- new.env(parent = object$envir)
	if (is.language(object$call$data)) {
		assign(as.character(object$call$data), data.train, envir = child.env)
	} else {
		object$call$data <- data.train
	}
	model <- eval(object$call, child.env)
	# Create prediction.
	type <- ifelse(
		object$adapter$model.type == "regression", "response", "prob"
	)
	predict.args <- c(
		list(newdata = data.test, type = type), object$predict.args
	)
	predict.args <- apply.grid(predict.args, object$grid.predict)
	return(lapply(predict.args, make.prediction, model, object, row.index))
}


#------------------------------------------------------------------------------
#	Change structure of list.
#
#	This function swap the hierarchical structure of list, i.e. swap first and
#	second level of list. Used for combining result of cross validation and
#	calculated metrics.
#	For example following conversion is done:
#
#		List of 10:					List of 5:
#			$List of 5		->			$List of 10:
#				$list()						$list()
#
#	Args:
#		x (list):
#			list of lists, should have same structure for all sub elements.
#------------------------------------------------------------------------------
swap.list.hierarchy <- function(x) {
	n <- length(x[[1]])
	return(lapply(1:n, function(i) lapply(x, "[[", i = i)))
}


#------------------------------------------------------------------------------
fit.cv.models <- function(object) {
	# Run cross validation.
	object$cv.group <- cv.group(object)
	cl.man <- cluster.manager(object, "cv")
	on.exit(cl.man$finalize())
	fits.of.folds <- cl.man$lapply(1:object$folds, model.one.fold, object)
	fits <- swap.list.hierarchy(fits.of.folds)
	# Calculate metrics.
	metrics <- cv.metrics(object, fits)
	metrics <- lapply(
		metrics, merge.grid.and.metrics, grid = object$grid.predict
	)
	if (object$adapter$model.type == "classification") {
		for (i in 1:length(object$cutpoint.options$methods)) {
			metrics[[i]] <- cbind(
				method = object$cutpoint.options$methods[i], metrics[[i]]
			)
		}
	}
	# Make a list of results of cross validation.
	# Each element of the list represent one result of cross validation.
	result <- vector("list", length(fits) * length(metrics))
	for (i in 1:length(fits)) {
		for (j in 1:length(metrics)) {
			obj <- object[c("call", "cv.group")]
			obj$fits <- fits[[i]]
			obj$metrics <- metrics[[j]][i, ]
			rownames(obj$metrics) <- NULL
			result[[(i - 1) * length(metrics) + j]] <- obj
		}
	}
	return(result)
}


#------------------------------------------------------------------------------
#	Create cv.models object from the arguments passed to cv.models function.
#------------------------------------------------------------------------------
cv.models.object <- function(
	call, folds, stratify, n.cores, seed, positive.class, package.name, envir,
	aggregate.method = c("mean", "join"), grid, grid.predict, group,
	cutpoint.options, ...
) {
	aggregate.method <- match.arg(aggregate.method)
	object <- list(
		call = call, folds = folds, stratify = stratify, n.cores = n.cores,
		seed = seed, positive.class = positive.class,
		package.name = package.name, envir = envir,
		aggregate.method = aggregate.method, grid = grid,
		grid.predict = grid.predict, group = group,
		cutpoint.options = cutpoint.options, predict.args = list(...),
		adapter = model.adapter::model.adapter$new(call, envir, package.name),
		cv.results = NULL
	)
	# Change number of folds when user defined group is specified.
	object$folds <- ifelse(
		is.null(group), folds, length(unique(cv.group(object)))
	)
	class(object) <- "cv.models"
	return(object)
}


#------------------------------------------------------------------------------
#'	Calculate metrics by cross validation.
#'
#'	Calculate metrics of predictive ability of the model by n-fold
#'	cross validation.
#'
#'	@param call
#'		a call of model function to be tested by cross validation.
#'	@param folds
#'		an integer specifying number of folds f cross validation.
#'	@param stratify
#'		if TRUE, class stratification is used when splitting folds.
#'	@param n.cores
#'		an integer specifying number of cores used for calculation.
#'		If NULL (default), all cores (including logical cores) are used.
#'	@param seed
#'		an integer specifying seed of random numbers. If specified,
#'		\code{cv.models} reproduce same results for the same setting.
#'		If NULL (default), the result of the cross validaiton differ everytime.
#'	@param positive.class
#'		name of positive class in the response variable of the model.
#'		Only used for classification model and ignored when the model is
#'		a regression model. Also even if the model is classification model,
#'		this is ignored if response variable is numeric or logical.
#'		For the detail about the way how cv.models determines the positive
#'		class, see Details section.
#'	@param package.name
#'		name of package which contains the model function.
#'		Usually users don't need to specify this argument, but if there are
#'		functions with same name in different package (e.g., \code{gam}
#'		in \code{\link[gam]{gam}} package and \code{\link[mgcv]{mgcv}}
#'		pacackages), this may be usefull.
#'	@param envir
#'		an environment where the call is evaluated.
#'	@param aggregate.method
#'		a character specifiying method for aggregating each fold of cross
#'		validation. Possible values area \code{"mean"} and \code{"join"}.
#'		If \code{"mean"} is specified, the metrics are calculated for each fold
#'		and after that mean values and SDs of the metrics of each fold area
#'		calculated. If \code{"join"} is specified, first all folds are combined
#'		into one and then the metrics are calculated only one time. In this
#'		case, SDs of the metrics are not calculated.
#'	@param grid
#'		a named list having vector of candidate hyper-parameters of the model.
#'		The metrics are calculated for all combination of the parameter in the
#'		list.
#'	@param grid.predict
#'		a named list having vector of candidate hyper-parameters of the model
#'		which are specified in \code{predict} method of the model.
#'	@param group
#'		a vector of integer, character, logical or factor specifying user
#'		defined grouping of folds. The vector sshould have the same length
#'		as the data used for modeling. When \code{gorup} is specified,
#'		\code{folds} and \code{stratify} are ignored and grouping of folds
#'		is determined solely basing on the \code{group}.
#'	@param cutpoint.options
#'		a list having options for
#'		\code{\link[OptimalCutpoints]{optimal.cutpoints}} by which threshold
#'		deviding positive and negative cases is calculated. Users can specify
#'		\code{methods}, \code{pop.prev} and \code{control}, and ther options
#'		are ignored. If more than one method are provided, metrics for all
#'		methods are calculated.
#'	@param ...
#'		parameters passed to the predict method of the model.
#'
#'	@details
#'		The positive class is determined in the order of following rule.\cr
#'		\enumerate{
#'			\item{
#'				If the response variable is numeric, 1 is used for the
#'				positive class (assuming it is a classification model with
#'				binary response variable).
#'			}
#'			\item{
#'				If the response variable is logical, TRUE is used for the
#'				positive class.
#'			}
#'			\item{
#'				If \code{positive.class} is supplied, the value of it is used.
#'			}
#'			\item{
#'				If the response variable is factor, the first level of the
#'				factor is used.
#'			}
#'			\item{
#'				If the response variable is character, the first item of
#'				unique values of it is used.
#'			}
#'		}
#'		If the rule above can't determine the positive class, the function
#'		produces error.\cr\cr
#'		to be continued...
#'
#'	@seealso
#'		For the detailed introduction of the function, see vignettes (
#'		\link[=../doc/cv.models.j.html]{Japanese}.
#'
#'	@examples
#'		# to be continued...
#'
#'	@export
#------------------------------------------------------------------------------
cv.models <- function(
	call, folds = 10, stratify = FALSE, n.cores = NULL, seed = NULL,
	positive.class = NULL, package.name = NULL, envir = parent.frame(),
	aggregate.method = c("mean", "join"), grid = NULL, grid.predict = NULL,
	group = NULL, cutpoint.options = list(methods = "Youden"), ...
) {
	# Find call.
	if (!is.language(call) & !is.symbol(substitute(call))) {
		call <- substitute(call)
	}
	call <- model.adapter::model.adapter$new(call, envir, package.name)$call
	object <- cv.models.object(
		call, folds, stratify, n.cores, seed, positive.class,
		package.name, envir, aggregate.method, grid, grid.predict,
		group, cutpoint.options, ...
	)
	objects <- apply.grid.for.object(object)
	cl.man <- cluster.manager(object, "grid")
	on.exit(cl.man$finalize())
	cv.results <- cl.man$lapply(objects, fit.cv.models)
	cv.results <- merge.grid.and.cv.results(grid, cv.results)
	object$cv.results <- do.call(c, cv.results)
	return(object)
}
