#==============================================================================
#	cluster.manager.r
#
#	Author: Michio Oguro
#
#	Deskription:
#		This file contains a reference class managing parallel processing.
#==============================================================================

#------------------------------------------------------------------------------
#'	(Internal) Manager Class of Parallel Processing
#'
#'	This class manage parallel processing cluster and parallel/one-by-one
#'	version of \code{lapply}. Because \code{\link{cv.models}} potentially
#'	has two computing intensive processs, i.e., cross validation and grid
#'	search of meta-parameters, this class optimize CPU usege of both processes.
#'
#'	@field cl
#'		a cluster handler produced by \code{\link[parallel]{makeCluster}}.
#'		If no cluster was initiated or cluster was already stopped,
#'		this field should have NULL.
#'
#'	@field n.cores
#'		an integer representing number of cores used for calculation.
#'		This is automatically determined by settings of cross validation.
#'
#'	@section Public methods:
#'
#'		\strong{\code{lapply(X, FUN, ...)}}
#'
#'			Parallel/One-by-one Version of \code{lapply}.
#'
#'		\strong{\code{finalize(...)}}
#'
#'			Stop cluster.
#'
#'	@section Private methods:
#'
#'		\strong{\code{initialize(object = NULL, type = c("cv", "grid"), ...)}}
#'
#'			Generate a new object.
#'
#'			\subsection{Args}{
#'				\describe{
#'					\item{\code{object = NULL}}{
#'						an object of \code{\link{cv.models}}.
#'					}
#'					\item{\code{type = c('cv', 'grid')}}{
#'						character representing type of parallel processing.
#'						\code{'cv'} denotes parallel processing for cross
#'						validation and \code{'grid'} denotes parallel
#'						processing for grid search of meta-parameters.
#'					}
#'				}
#'			}
#'
#'		\strong{\code{export.functions()}}
#'
#'			Export required functions to the cluster.
#'
#'		\strong{\code{detect.cores(object, type = c("cv", "grid"))}}
#'
#'			Determine number of cores used for the \\code{lapply} method.
#'
#'			\subsection{Args}{
#'				\describe{
#'					\item{\code{object}}{
#'						an object of \code{\link{cv.models-class}}.
#'					}
#'					\item{\code{type = c('cv', 'grid')}}{
#'						type of calculation.
#'					}
#'				}
#'			}
#------------------------------------------------------------------------------
cluster.manager.class <- R6::R6Class(
	"cluster.manager",
	private = list(
		cl = NULL,
		n.cores = NULL
	)
)


#------------------------------------------------------------------------------
cluster.manager.class$set(
	"public", "initialize",
	function(object = NULL, type = c("cv", "grid"), ...) {
		private$n.cores <- private$detect.cores(object, type)
		if (private$n.cores > 1) {
			# Initialize cluster
			private$cl <- parallel::makeCluster(private$n.cores)
			private$export.functions()
			parallel::clusterExport(
				private$cl, ls(envir = object$envir), envir = object$envir
			)
			parallel::clusterEvalQ(private$cl, library(model.adapter))
			parallel::clusterEvalQ(private$cl, library(cv.models))
			parallel::clusterCall(
				private$cl, library, object$adapter$package.name,
				character.only = TRUE
			)
		} else {
			private$cl <- NULL
			library(object$adapter$package.name, character.only = TRUE)
		}
	}
)


#------------------------------------------------------------------------------
cluster.manager.class$set(
	"public", "finalize",
	function(...) {
		if (!is.null(private$cl)) {
			parallel::stopCluster(private$cl)
			private$cl <- NULL
		}
	}
)


#------------------------------------------------------------------------------
cluster.manager.class$set(
	"private", "export.functions",
	function() {
		object.names <- c("apply.grid", "make.prediction")
		parallel::clusterExport(
			private$cl, object.names, loadNamespace("cv.models")
		)
	}
)


#------------------------------------------------------------------------------
cluster.manager.class$set(
	"private", "detect.cores",
	function(object, type = c("cv", "grid")) {
		type <- match.arg(type)
		if (is.null(object$n.cores)) {
			if (Sys.getenv("_R_CHECK_LIMIT_CORES_", "") == "TRUE") {
				cores <- 1
			} else {
				cores <- parallel::detectCores()
			}
		} else {
			cores <- as.integer(object$n.cores)
		}
		if (is.na(cores) | cores == 1L) {
			return(1L)
		} else if (is.null(object$grid)) {
			return(switch(type, cv = cores, grid = 1L))
		} else {
			grid <- expand.grid(object$grid)
			if (nrow(grid) >= object$folds) {
				return(switch(type, cv = 1L, grid = cores))
			} else {
				return(switch(type, cv = cores, grid = 1L))
			}
		}
	}
)


#------------------------------------------------------------------------------
cluster.manager.class$set(
	"public", "lapply",
	function(X, FUN, ...) {
		if (private$n.cores > 1) {
			return(parallel::parLapply(private$cl, X, FUN, ...))
		} else {
			return(base::lapply(X, FUN, ...))
		}
	}
)


#------------------------------------------------------------------------------
cluster.manager <- cluster.manager.class$new
