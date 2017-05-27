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
	grids <- lapply(1:nrow(grid), "[.data.frame", x = grid,)
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
fit.cv.models <- function(object) {
	# クロスバリデーションを実行。
	object$cv.group <- cv.group(object)
	cl.man <- cluster.manager(object, "cv")
	on.exit(cl.man$finalize())
	fits <- cl.man$lapply(1:object$folds, model.one.fold, object)
	# クロスバリデーションの結果を整形。
	if (is.null(object$grid.predict)) {
		n.results <- 1
	} else {
		n.results <- nrow(expand.grid(object$grid.predict))
	}
	object$fits <- lapply(1:n.results, function(i) lapply(fits, "[[", i = i))
	# 性能評価指標を計算。
	object$metrics <- cv.metrics(object)
	object$metrics <- merge.grid.and.metrics(
		object$grid.predict, object$metrics
	)
	# 結果を整形して、１回のCV結果が１要素のリストに変換する。
	result <- vector("list", n.results)
	for (i in 1:n.results) {
		obj <- object[c("call", "cv.group", "fits", "metrics")]
		obj$fits <- obj$fits[[i]]
		obj$metrics <- obj$metrics[i,]
		rownames(obj$metrics) <- NULL
		result[[i]] <- obj
	}
	return(result)
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
	on.exit(cl.man$finalize())
	cv.results <- cl.man$lapply(objects, fit.cv.models)
	cv.results <- merge.grid.and.cv.results(grid, cv.results)
	object$cv.results <- do.call(c, cv.results)
	return(object)
}
