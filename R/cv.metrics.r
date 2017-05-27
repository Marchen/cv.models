#==============================================================================
#	いろいろなモデル性能評価指標の計算クラスと関数。
#==============================================================================
#	cv.metrics.r
#
#	This file contains a class and function for calculating metrics of
#	model predictive ability.
#==============================================================================


#------------------------------------------------------------------------------
#'	A reference class calculating metrics of model predictive ability.
#'
#'	This class has several functions calculating metrics of model predictive
#'	ability. To obtain the metrics, initialize the object and call
#'	\emph{calculate.metrics} method so that the class object calculate
#'	appropriate metrics depending on the problem, i.e., classification or
#'	regression.
#'
#'	Following metrics are calculated for regression models.
#'
#'	\describe{
#'		\item{Mean squared error (MSE)}{
#'			\eqn{mean((prediction - response) ^ 2)}
#'		}
#'		\item{Root mean squared error (RMSE)}{
#'			\eqn{sqrt(mean((prediction - response) ^ 2))}
#'		}
#'		\item{R squared, \eqn{R^2}}{
#'			\eqn{Pearson's product moment correlation coefficient ^ 2}
#'
#'			Note that this definition of \eqn{R^2} may produce high values
#'			for models without predictive ability so that I recommend to use
#'			\eqn{Q ^ 2} instead of \eqn{R ^ 2}.
#'			See
#'			\href{http://www.russpoldrack.org/2012/12/the-perils-of-leave-one-out.html}{
#'				this
#'			},
#'			\href{http://not2hastie.tumblr.com}{this} and
#'			\href{https://metarabbit.wordpress.com/2014/03/06/evaluating-regression-with-cross-validation/}{
#'				this
#'			}.
#'		}
#'		\item{Q squared, \eqn{Q^2}}{
#'			\eqn{
#'				1 - \sum((prediction - response) ^ 2)
#'				/ \sum((response - mean(response)) ^ 2)
#'			}
#'		}
#'	}
#'
#'	Following metrics are calculated for regression models.
#'
#'
#'	@field fits
#'		a list of lists having \emph{response} and \emph{fit} field.
#'		\emph{response} field contains original values of response variable
#'		and \emph{fit} field contains predicted values of response variable of
#'		each validation set.
#'
#'	@field aggregate.method
#'		a character representing method for aggregation of the validation sets.
#'		Currently, \emph{"mean"} and \emph{join} are supported.
#'		If \emph{"mean"} is specified, this class calculate mean and SD of
#'		the metrics calculated for each validation set.
#'		If \emph{"join"} is specified, this class joins results of all
#'		validation sets and calculate one values of metrics.
#'
#'	@field positive.class
#'		a character specifing positive class of response variable.
#'		Only classification model. If not specified, the first level of the
#'		response variable is used for positive class. If response variable is
#'		binary (0/1) or logical (TRUE/FALSE), 1 and TRUE are used for the
#'		positive class.
#'
#'	@references
#'		\itemize{
#'			\item{}{
#'				Kvalseth, (1985). Cautionary Note about R2. The American
#'				Statistician, 39, 279-285.
#'			}
#'			\item{}{
#'				Powers, (2011). Evaluation: from Precision, Recall and
#'				F-measure to ROC, Informedness, Markedness and Correlation.
#'				Journal of Machine Learning Technologies, 2, 37.
#'			}
#'			\item{}{
#'				Quan, (1988). The prediction sum of squares as a general
#'				measure for regression diagnostics. J. Bus. Econ. Stat., 6,
#'				501-504.
#'			}
#'			\item{}{
#'				\url{http://www.russpoldrack.org/2012/12/the-perils-of-leave-one-out.html}
#'			}
#'			\item{}{
#'				\url{http://not2hastie.tumblr.com}
#'			}
#'			\item{}{
#'				\url{https://metarabbit.wordpress.com/2014/03/06/evaluating-regression-with-cross-validation/}
#'			}
#'		}
#------------------------------------------------------------------------------
#	モデルの性能評価指標を計算するクラス。
#
#	クラスを初期化して、calculate.metrics()を呼びだすと、モデルが扱う問題
#	（回帰・識別）に応じて適切な指標が計算される。
#------------------------------------------------------------------------------
cv.metrics.calculator <- setRefClass(
	"cv.metrics.calculator",
	fields = list(
		fits = "list",
		aggregate.method = "character",
		positive.class = "character",
		model.type = "character"
	)
)


#------------------------------------------------------------------------------
#	クラスを初期化する。
#------------------------------------------------------------------------------
cv.metrics.calculator$methods(
	initialize = function(object = NULL, ...) {
		"
		Initialize object.

		\\itemize{
			\\item{\\code{object}}{
				\\code{\\link{cv.models}} object with result of cross
				validation.
			}
			\\item{\\code{...}}{
				currently not used.
			}
		}
		"
		# When an object is initialized without options, consider it is an
		# initialization process of reference class and return NULL.
		if (is.null(object)) {
			return()
		}
		# Set parameters.
		.self$fits <- object$fits
		.self$aggregate.method = object$aggregate.method
		.self$positive.class <- determine.positive.class(object)
		.self$model.type <- object$adapter$model.type
	}
)


#------------------------------------------------------------------------------
#	MSE
#------------------------------------------------------------------------------
cv.metrics.calculator$methods(
	calc.mse = function(fit) {
		"
		Calculate mean squared error.

		\\itemize{
			\\item{\\code{fit}}{
				a list having \\code{response} and \\code{fit} fields
				representing actual and predicted values of the response
				variable.
			}
		}
		"
		if (is.factor(fit$response)) {
			return(NA)
		}
		return(mean((fit$prediction - fit$response) ^ 2))
	}
)


#------------------------------------------------------------------------------
#	RMSE
#------------------------------------------------------------------------------
cv.metrics.calculator$methods(
	calc.rmse = function(fit) {
		"
		Calculate root mean squared error.

		\\itemize{
			\\item{\\code{fit}}{
				a list having \\code{response} and \\code{fit} fields
				representing actual and predicted values of the response
				variable.
			}
		}
		"
		return(sqrt(calc.mse(fit)))
	}
)


#------------------------------------------------------------------------------
#	R^2
#------------------------------------------------------------------------------
cv.metrics.calculator$methods(
	calc.r.squared = function(fit) {
		"
		Calculate R squared.

		\\itemize{
			\\item{\\code{fit}}{
				a list having \\code{response} and \\code{fit} fields
				representing actual and predicted values of the response
				variable.
			}
		}
		"
		if (is.factor(fit$response)) {
			return(NA)
		}
		return(cor(fit$response, fit$prediction) ^ 2)
	}
)


#------------------------------------------------------------------------------
#	Q^2
#------------------------------------------------------------------------------
cv.metrics.calculator$methods(
	calc.q.squared = function(fit) {
		"
		Calculate Q squared.

		\\itemize{
			\\item{\\code{fit}}{
				a list having \\code{response} and \\code{fit} fields
				representing actual and predicted values of the response
				variable.
			}
		}
		"
		if (is.factor(fit$response)) {
			return(NA)
		}
		press <- sum((fit$prediction - fit$response) ^ 2)
		tss <- sum((fit$response - mean(fit$response)) ^ 2)
		return(1 - press / tss)
	}
)


#------------------------------------------------------------------------------
#	pROC::coordsで計算する指標を全て計算。
#------------------------------------------------------------------------------
cv.metrics.calculator$methods(
	calc.roc.metrics = function(fit) {
		"
		Calculate metrics depending on pROC::coords.

		\\itemize{
			\\item{\\code{fit}}{
				a list having \\code{response} and \\code{fit} fields
				representing actual and predicted values of the response
				variable.
			}
		}
		"
		require(pROC)
		all.metrics <- c(
			"threshold", "specificity", "sensitivity", "accuracy",
			"tn", "tp", "fn", "fp", "npv", "ppv",
			"1-specificity", "1-sensitivity", "1-accuracy", "1-npv", "1-ppv"
		)
		prob <- fit$prediction[, .self$positive.class]
		response <- as.numeric(fit$response == .self$positive.class)
		roc.object <- roc(response, prob)
		metrics <- coords(
			roc.object, x = "best", best.method = "youden", ret = all.metrics
		)
		# TODO: Youdenがタイだったときの処理
		if (is.matrix(metrics)) {
			metrics <- t(metrics)
		} else {
			metrics.names <- names(metrics)
			metrics <- matrix(metrics, nrow = 1)
			colnames(metrics) <- metrics.names
		}
		metrics <- cbind(
			metrics, informedness = .self$calc.informedness(metrics),
			markedness = .self$calc.markedness(metrics),
			mcc = .self$calc.mcc(metrics), auc = roc.object$auc
		)
		return(metrics)
	}
)


#------------------------------------------------------------------------------
#	Informedness
#------------------------------------------------------------------------------
cv.metrics.calculator$methods(
	calc.informedness = function(metrics) {
		"
		Calculate informedness.

		\\itemize{
			\\item{\\code{metrics}}{
				a matrix having result of \\code{calc.roc.metrics} method.
			}
		}
		"
		return(metrics[, "sensitivity"] + metrics[, "specificity"] - 1)
	}
)


#------------------------------------------------------------------------------
#	Markedness
#------------------------------------------------------------------------------
cv.metrics.calculator$methods(
	calc.markedness = function(metrics) {
		"
		Calculate markedness.

		\\itemize{
			\\item{\\code{metrics}}{
				a matrix having result of \\code{calc.roc.metrics} method.
			}
		}
		"
		return(metrics[, "ppv"] + metrics[, "npv"] - 1)
	}
)


#------------------------------------------------------------------------------
#	Matthew's correlation coefficient
#------------------------------------------------------------------------------
cv.metrics.calculator$methods(
	calc.mcc = function(metrics) {
		"
		Calculate Matthew's correlation coefficient (MCC).

		\\itemize{
			\\item{\\code{metrics}}{
				a matrix having result of \\code{calc.roc.metrics} method.
			}
		}
		"
		tp <- metrics[, "tp"]
		tn <- metrics[, "tn"]
		fp <- metrics[, "fp"]
		fn <- metrics[, "fn"]
		denom <- sqrt((tp + fn) * (tp + fp) * (tn + fp) * (tn + fn))
		mcc <- (tp * tn - fp * fn) / denom
		return(mcc)
	}
)


#------------------------------------------------------------------------------
#	全ての回帰モデルの性能評価指標を計算する。
#------------------------------------------------------------------------------
cv.metrics.calculator$methods(
	calc.metrics.for.regression = function(x) {
		"
		Calculate all metrics for regression models.

		\\describe{
			\\item{\\code{x}}{fits field of a object of this class.}
		}
		"
		if (.self$aggregate.method == "join") {
			x <- lapply(x, as.data.frame)
			x <- list(do.call(rbind, x))
		}
		mse <- sapply(x, .self$calc.mse)
		rmse <- sapply(x, .self$calc.rmse)
		r.squared <- sapply(x, .self$calc.r.squared)
		q.squared <- sapply(x, .self$calc.q.squared)
		result <- c(
			mse = mean(mse), rmse = mean(rmse),
			r.squared = mean(r.squared), q.squared = mean(q.squared),
			sd.mse = sd(mse), sd.rmse = sd(rmse),
			sd.r.squared = sd(r.squared), sd.q.squared = sd(q.squared)
		)
		return(result)
	}
)


#------------------------------------------------------------------------------
#	全ての識別モデルの性能評価指標を計算する。
#------------------------------------------------------------------------------
cv.metrics.calculator$methods(
	calc.metrics.for.classification = function(x) {
		"
		Calculate all metrics for classification models.

		\\describe{
			\\item{\\code{x}}{fits field of a object of this class.}
		}
		"
		if (.self$aggregate.method == "join") {
			x <- lapply(x, as.data.frame)
			x <- list(do.call(rbind, x))
		}
		result <- lapply(x, .self$calc.roc.metrics)
		if (any(sapply(result, nrow) > 1)) {
			warning("Best threshold was not determined by Youden's J.")
			result <- lapply(result, colMeans)
		}
		result <- do.call(rbind, result)
		result.mean <- colMeans(result)
		result.sd <- apply(result, 2, sd)
		names(result.sd) <- paste0("sd.", colnames(result))
		return(c(result.mean, result.sd))
	}
)


#------------------------------------------------------------------------------
#	全ての性能評価指標を計算する。
#------------------------------------------------------------------------------
cv.metrics.calculator$methods(
	metrics = function() {
		f <- .self$fits
		if (.self$model.type == "regression") {
			metrics <- lapply(f, .self$calc.metrics.for.regression)
		} else {
			metrics <- lapply(f, .self$calc.metrics.for.classification)
		}
		metrics <- do.call(rbind, metrics)
		return(metrics)
	}
)


#------------------------------------------------------------------------------
#'	Calculate model evaluation metrics.
#'
#'	This function calculates several model evaluation metrics for both
#'	regression and classification models.
#'
#'	@param adapter
#'		a model.adapter object for the model used for cross validation.
#'	@param object
#'		a \code{}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
cv.metrics <- function(object) {
	cal <- cv.metrics.calculator(object)
	return(cal$metrics())
}
