#==============================================================================
#	cluster.manager.r
#
#	Author: Michio Oguro
#
#	Deskription:
#		This file contains a reference class managing parallel processing.
#==============================================================================
#	このファイルにはクラスタ管理クラスが含まれています。
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
#'
#'	@field n.cores
#'		an integer representing number of cores used for calculation.
#'		This is automatically determined by settings of cross validation.
#------------------------------------------------------------------------------
#	並列計算管理クラス。
#
#	Fields:
#		cl:
#			makeCluster()で作られたクラスター。
#		n.cores:
#			計算に使うコア数。
#			Cross Validationの設定に応じて自動的に決定される。
#------------------------------------------------------------------------------
cluster.manager <- setRefClass(
	"cluster.manager",
	fields = list(
		cl = "ANY",
		n.cores = "integer"
	)
)


#------------------------------------------------------------------------------
#	クラスを初期化する。
#
#	Args:
#		object:
#			cv.modelsオブジェクト。
#		type:
#			並列計算の種類。
#				"cv": クロスバリデーションを実行する並列計算。
#				"grid": パラメーターのグリッドサーチを実行する並列計算。
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
		library(parallel)
		.self$n.cores <- .self$detect.cores(object, type)
		if (.self$n.cores > 1) {
			# Initialize cluster
			.self$cl <- makeCluster(.self$n.cores)
			.self$export.functions()
			clusterExport(
				.self$cl, ls(envir = object$envir), envir = object$envir
			)
			clusterEvalQ(.self$cl, library(model.adapter))
			clusterEvalQ(.self$cl, library(cv.models))
			clusterCall(
				.self$cl, library, object$adapter$package.name,
				character.only = TRUE
			)
			clusterCall(.self$cl, set.seed.if.possible, object$seed)
		} else {
			library(object$adapter$package.name, character.only = TRUE)
			set.seed.if.possible(object$seed)
		}
	}
)


#------------------------------------------------------------------------------
#	オブジェクト解放時にクラスターを開放する。
#------------------------------------------------------------------------------
cluster.manager$methods(
	finalize = function(...) {
		"
		Stops Cluster on Finalize
		"
		if (.self$n.cores > 1) {
			stopCluster(.self$cl)
		}
	}
)


#------------------------------------------------------------------------------
#	必要な関数をクラスターにエクスポートする。
#------------------------------------------------------------------------------
cluster.manager$methods(
	export.functions = function() {
		"
		Export Required Functions to the Cluster
		"
		.self$export(c("apply.grid", "make.prediction"))
	}
)


#------------------------------------------------------------------------------
#	オブジェクトをエクスポートする。
#------------------------------------------------------------------------------
cluster.manager$methods(
	export = function(names, envir = parent.frame(1L)) {
		"
		Export Required Functions to the Cluster
		"
		for (i in names) {
			clusterCall(.self$cl, assign, i, get(i, envir = envir))
		}
	}
)



#------------------------------------------------------------------------------
#	計算に使うコア数を決定する。
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
			cores <- detectCores()
		} else {
			cores <- as.integer(object$n.cores)
		}
		if (cores == 1L) {
			return(1L)
		} else if (!is.null(object$grid)) {
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
#	逐次実行、並列計算版のlapply。
#------------------------------------------------------------------------------
cluster.manager$methods(
	lapply = function(X, FUN, ...) {
		"
		Parallel/One-by-one Version of \\code{lapply}
		"
		if (.self$n.cores > 1) {
			return(clusterApply(.self$cl, X, FUN, ...))
		} else {
			return(base::lapply(X, FUN, ...))
		}
	}
)
