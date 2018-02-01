#------------------------------------------------------------------------------
#	 class calculating metrics for regression models.
#
#	This class has several functions calculating metrics of predictive ability
#	of regression models.
#	To obtain the metrics, initialize the object and call
#	\emph{calculate.metrics} method.
#
#	Following metrics are calculated for regression models.
#
#	\describe{
#		\item{Mean squared error (MSE)}{
#			\eqn{mean((prediction - response) ^ 2)}
#		}
#		\item{Root mean squared error (RMSE)}{
#			\eqn{sqrt(mean((prediction - response) ^ 2))}
#		}
#		\item{R squared, \eqn{R^2}}{
#			\eqn{Pearson's product moment correlation coefficient ^ 2}
#
#			Note that this definition of \eqn{R^2} may produce high values
#			for models without predictive ability so that I recommend to use
#			\eqn{Q ^ 2} instead of \eqn{R ^ 2}.
#			See
#			\href{http://www.russpoldrack.org/2012/12/the-perils-of-leave-one-out.html}{
#				this
#			},
#			\href{http://not2hastie.tumblr.com}{this} and
#			\href{https://metarabbit.wordpress.com/2014/03/06/evaluating-regression-with-cross-validation/}{
#				this
#			}.
#		}
#		\item{Q squared, \eqn{Q^2}}{
#			\eqn{
#				1 - \sum((prediction - response) ^ 2)
#				/ \sum((response - mean(response)) ^ 2)
#			}
#		}
#	}
#------------------------------------------------------------------------------
regression.metrics.calculator <- R6Class("regression.metrics.calculator")


#------------------------------------------------------------------------------
#	Calculate mean squared error (MSE).
#
#	Args:
#		fit:
#			a list having result of one fold of cross validation
#			with "response", "prediction" and "index" fields.
#------------------------------------------------------------------------------
regression.metrics.calculator$set(
	"private", "calc.mse",
	function(fit) {
		if (is.factor(fit$response)) {
			return(NA)
		}
		return(mean((fit$prediction - fit$response) ^ 2))
	}
)


#------------------------------------------------------------------------------
#	Calculate root mean squared error (RMSE).
#
#	Args:
#		fit:
#			a list having result of one fold of cross validation
#			with "response", "prediction" and "index" fields.
#------------------------------------------------------------------------------
regression.metrics.calculator$set(
	"private", "calc.rmse",
	function(fit) {
		return(sqrt(private$calc.mse(fit)))
	}
)


#------------------------------------------------------------------------------
#	Calculate R squared.
#
#	Args:
#		fit:
#			a list having result of one fold of cross validation
#			with "response", "prediction" and "index" fields.
#------------------------------------------------------------------------------
regression.metrics.calculator$set(
	"private", "calc.r.squared",
	function(fit) {
		if (is.factor(fit$response)) {
			return(NA)
		}
		return(cor(fit$response, fit$prediction) ^ 2)
	}
)


#------------------------------------------------------------------------------
#	Calculate Q squared.
#
#	Args:
#		fit:
#			a list having result of one fold of cross validation
#			with "response", "prediction" and "index" fields.
#------------------------------------------------------------------------------
regression.metrics.calculator$set(
	"private", "calc.q.squared",
	function(fit) {
		if (is.factor(fit$response)) {
			return(NA)
		}
		press <- sum((fit$prediction - fit$response) ^ 2)
		tss <- sum((fit$response - mean(fit$response)) ^ 2)
		return(1 - press / tss)
	}
)


#------------------------------------------------------------------------------
#	Calculate all metrics.
#
#	Args:
#		fit:
#			a list having result of one fold of cross validation
#			with "response", "prediction" and "index" fields.
#
#	Returns:
#		calculated metrics in following format.
#
#			list(matrix(mse, rmse, r.squared, q.squared))
#
#		Because classification.metrics.calculator can returns metrics for
#		different threshold determination method, the format of the result
#		should be a list having matrix.
#------------------------------------------------------------------------------
regression.metrics.calculator$set(
	"public", "calculate.metrics",
	function(fit) {
		metrics <- cbind(
			mse = private$calc.mse(fit),
			rmse = private$calc.rmse(fit),
			r.squared = private$calc.r.squared(fit),
			q.squared = private$calc.q.squared(fit)
		)
		# To have same result format with classification.metrics.calculator,
		# return the matrix of metrics in a list.
		return(list(metrics))
	}
)
