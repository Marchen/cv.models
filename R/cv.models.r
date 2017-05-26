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
	grids <- split(grid, 1:nrow(grid))
	fun <- function(grid, cv.result) {
		cv.result$metrics <- merge.grid.and.metrics(grid, cv.result$metrics)
		return(cv.result)
	}
	return(mapply(fun, grids, cv.results, SIMPLIFY = FALSE))
}


#------------------------------------------------------------------------------
make.prediction <- function(predict.args, object, row.index) {
	fit <- do.call(object$adapter$predict, predict.args)$fit
	if (object$adapter$model.type == "regression") {
		fit <- fit[, "fit"]
		attributes(fit) <- NULL
	}
	# 結果の作成
	response <- object$adapter$y.vars()[row.index,]
	result <- list(
		response = response, prediction = fit, index = row.index
	)
	return(result)
}


#------------------------------------------------------------------------------
model.one.fold <- function(cv.index, object) {
	# モデル構築用データを作成
	if (!is.null(object$seed)) {
		set.seed(object$seed)
	}
	data.test <- object$adapter$data[object$cv.group == cv.index, ]
	data.train <- object$adapter$data[object$cv.group != cv.index, ]
	row.index <- (1:nrow(object$adapter$data))[object$cv.group == cv.index]
	# モデル構築
	child.env <- new.env(parent = object$envir)
	assign(as.character(object$call$data), data.train, envir = child.env)
	model <- eval(object$call, child.env)
	# 予測値を計算
	type <- ifelse(
		object$adapter$model.type == "regression", "response", "prob"
	)
	predict.args <- c(
		list(model, newdata = data.test, type = type), object$predict.args
	)
	predict.args <- apply.grid(predict.args, object$grid.predict)
	return(lapply(predict.args, make.prediction, object, row.index))
}


#------------------------------------------------------------------------------
cv.result <- function(object) {
	object <- object[c("call", "cv.group", "fits", "metrics")]
	class(object) <- "cv.result"
	return(object)
}


#------------------------------------------------------------------------------
fit.cv.models <- function(object) {
	object$cv.group <- cv.group(object)
	cv.man <- cluster.manager(object, "cv")
	fits <- cv.man$lapply(1:object$folds, model.one.fold, object)
	rearrange <- function(i) {
		return(lapply(fits, "[[", i = i))
	}
	object$fits <- lapply(1:length(fits[[1]]), rearrange)
	object$metrics <- cv.metrics(object)
	object$metrics <- merge.grid.and.metrics(
		object$grid.predict, object$metrics
	)
	return(cv.result(object))
}


#------------------------------------------------------------------------------
cv.models.object <- function(
	call, folds, n.cores, seed, positive.class, package.name, envir,
	aggregate.method = c("mean", "join"), grid, grid.predict, ...
) {
	aggregate.method <- match.arg(aggregate.method)
	object <- list(
		call = call, folds = folds, n.cores = n.cores, seed = seed,
		positive.class = positive.class, package.name = package.name,
		envir = envir, aggregate.method = aggregate.method, grid = grid,
		grid.predict = grid.predict, predict.args = list(...),
		adapter = model.adapter(call, envir, package.name), cv.results = NULL
	)
	class(object) <- "cv.models"
	return(object)
}


#------------------------------------------------------------------------------
#'	cv.models.
#'	@export
cv.models <- function(
	call, folds = 10, n.cores = NULL, seed = NULL, positive.class = NULL,
	package.name = NULL, envir = parent.frame(),
	aggregate.method = c("mean", "join"), grid = NULL, grid.predict = NULL, ...
) {
	if (!is.null(seed)) {
		set.seed(seed)
	}
	object <- cv.models.object(
		substitute(call), folds, n.cores, seed, positive.class,
		package.name, envir, aggregate.method, grid, grid.predict, ...
	)
	objects <- apply.grid.for.object(object)
	cl.man <- cluster.manager(object, "grid")
	object$cv.results <- cl.man$lapply(objects, fit.cv.models)
	object$cv.results <- merge.grid.and.cv.results(grid, object$cv.results)
	return(object)
}


