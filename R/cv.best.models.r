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
extract.result <- function(object, index, criteria = NULL) {
	best <- object
	best$grid <- NULL
	best$grid.predict <- NULL
	best$metrics <- best$cv.results[[index]]$metrics
	best$criteria <- criteria
	best$call <- best$cv.results[[index]]$call
	best$fits <- best$cv.results[[index]]$fits
	best$cv.group <- best$cv.results[[index]]$cv.group
	best$model <- eval(best$call, envir = best$envir)
	best$cv.results <- NULL
	class(best) <- "cv.result"
	return(best)
}


#'	@export
find.best.models <- function(object, criteria) {
	if (missing(criteria)) {
		criteria <- ifelse(
			object$adapter$model.type == "regression", "q.squared", "mcc"
		)
	}
	if (is.null(object$grid) & is.null(object$grid.predict)) {
		best.index <- 1
	} else {
		metrics <- extract.metrics(object)[criteria]
		best.index <- find.best.metrics.index(metrics)
	}
	result <- lapply(
		best.index, extract.result, object = object, criteria = criteria
	)
	class(result) <- "cv.best.models"
	return(result)
}
