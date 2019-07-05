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
#------------------------------------------------------------------------------
cluster.manager <- setRefClass(
	"cluster.manager",
	fields = list(
		cl = "ANY",
		n.cores = "integer"
	)
)


#------------------------------------------------------------------------------
cluster.manager$methods(
	initialize = function(object = NULL, type = c("cv", "grid"), ...) {
		"
		Initialize Manager

		\\describe{
			\\item{\\code{object = NULL}}{
				an object of \\code{\\link{cv.models-class}}.
			}
			\\item{\\code{type = c('cv', 'grid')}}{
				character representing type of parallel processing.
				\\code{'cv'} denotes parallel processing for cross validation
				and \\code{'grid'} denotes parallel processing for grid
				search of meta-parameters.
			}
		}
		"
		.self$n.cores <- .self$detect.cores(object, type)
		if (.self$n.cores > 1) {
			# Initialize cluster
			.self$cl <- parallel::makeCluster(.self$n.cores)
			.self$export.functions()
			parallel::clusterExport(
				.self$cl, ls(envir = object$envir), envir = object$envir
			)
			parallel::clusterEvalQ(.self$cl, library(model.adapter))
			parallel::clusterEvalQ(.self$cl, library(cv.models))
			parallel::clusterCall(
				.self$cl, library, object$adapter$package.name,
				character.only = TRUE
			)
		} else {
			.self$cl <- NULL
			library(object$adapter$package.name, character.only = TRUE)
		}
	}
)


#------------------------------------------------------------------------------
cluster.manager$methods(
	finalize = function(...) {
		"
		Stops Cluster
		"
		if (!is.null(.self$cl)) {
			parallel::stopCluster(.self$cl)
			.self$cl <- NULL
		}
	}
)


#------------------------------------------------------------------------------
cluster.manager$methods(
	export.functions = function() {
		"
		Export required functions to the cluster.
		"
		object.names <- c("apply.grid", "make.prediction")
		parallel::clusterExport(
			.self$cl, object.names, loadNamespace("cv.models")
		)
	}
)


#------------------------------------------------------------------------------
cluster.manager$methods(
	detect.cores = function(object, type = c("cv", "grid")) {
		"
		Determine Number of Cores Used for the \\code{lapply} Method

		\\describe{
			\\item{\\code{object}}{
				an object of \\code{\\link{cv.models-class}}.
			}
			\\item{\\code{type = c('cv', 'grid')}}{
				type of calculation.
			}
		}
		"
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
cluster.manager$methods(
	lapply = function(X, FUN, ...) {
		"
		Parallel/One-by-one Version of \\code{lapply}
		"
		if (.self$n.cores > 1) {
			return(parallel::parLapply(.self$cl, X, FUN, ...))
		} else {
			return(base::lapply(X, FUN, ...))
		}
	}
)
