#-------------------------------------------------------------------------------
#'	(Internal) Wrapper function of clusterApplyLB.
#'
#'	This function wraps difference in arguments between 
#'	\code{\link[base]{lapply}} and \code{\link[parallel]{clusterApplyLB}}.
#'
#'	@param X same as \emph{lapply}.
#'	@param FUN same as \emph{lapply}.
#'	@param ... same as \emph{lapply}.
#-------------------------------------------------------------------------------
#	lapplyのラッパー
#-------------------------------------------------------------------------------
cl.lapply  <- function(X, FUN, ...){
	return(clusterApplyLB(cl = cl, x = X, fun = FUN, ...))
}

#-------------------------------------------------------------------------------
#'	(Internal) Wrapper function of clusterMap.
#'
#'	This function wraps difference in arguments between 
#'	\code{\link[base]{mapply}} and \code{\link[parallel]{clusterMap}}.
#'
#'	@param FUN same as \emph{mapply}.
#'	@param ... same as \emph{mapply}.
#'	@param MoreArgs same as \emph{mapply}.
#'	@param SIMPLIFY same as \emph{mapply}.
#'	@param USE.NAMES same as \emph{mapply}.
#-------------------------------------------------------------------------------
#	mapplyのラッパー
#-------------------------------------------------------------------------------
cl.mapply <- function(
	FUN, ..., MoreArgs = NULL, SIMPLIFY = TRUE, USE.NAMES = TRUE
){
	return(
		clusterMAP(
			cl = cl, fun = FUN, ..., MoreArgs = MoreArgs,
			RECYCLE = TRUE, SIMPLIFY = SIMPLIFY, USE.NAMES = USE.NAMES
		)
	)
}

#-------------------------------------------------------------------------------
#	クラスターでパッケージを読み込む関数。
#
#	Args:
#		package.names: パッケージ名を表す文字列
#-------------------------------------------------------------------------------
cl.library <- function(package.names){
	ncl.library(package.names)
	for (name in package.names){
		expr <- parse(text = sprintf("require(%s)", name))
		clusterExport(cl, "expr", environment())
		clusterEvalQ(cl, eval(expr))
		clusterEvalQ(cl, rm(expr))
	}
}

#-------------------------------------------------------------------------------
#	文字列で指定されたパッケージを読み込む関数。
#
#	Args:
#		package.names: パッケージ名を表す文字列。複数指定可能。
#-------------------------------------------------------------------------------
ncl.library <- function(package.names){
	for (name in package.names){
		expr <- parse(text = sprintf("require(%s)", name))
		eval(expr)
	}
}

#-------------------------------------------------------------------------------
#	クロスバリデーションとパラメーター選択に使うコアの数を割り当てる。
#	パラメーター選択の候補数が使用するコア数よりも多かったらパラメーター選択を、
#	少なかったらクロスバリデーションを並列計算する。
#
#	expanded.args.model: パラメーター選択の候補を入れたリスト。
#	n.cores: 計算に使うコアの数。
#-------------------------------------------------------------------------------
assign.cores <- function(expanded.args.model, n.cores){
	if (is.null(n.cores)){
		require(parallel)
		n.cores <- detectCores()
	}
	n.cores.cv <- ifelse(length(expanded.args.model) < n.cores, n.cores, 1)
	n.cores.param.tune <- ifelse(length(expanded.args.model) < n.cores, 1, n.cores)
	return(list(cv = n.cores.cv, param.tune = n.cores.param.tune))
}


#-------------------------------------------------------------------------------
#	クラスターで必要な関数を送信する関数。
#
#	Args:
#		cl: parallelパッケージのクラスターのハンドラ。
#-------------------------------------------------------------------------------
export.functions <- function(cl){
	export.objects <- c(
		"cv.one.fold", "init.cluster", "ncl.library", "make.cv.group",
		"get.package.name", "get.class.name", "get.args", "make.dummy",
		"merge.tunable.args", "predict.gamm", "predict.glmmML",
		"get.positive.prob", "get.positive.class",
		"get.model.type.from.response.var", "get.model.type.from.family",
		"merge.cv.performances", "is.formula", "confusion.matrix",
		"cv.performance", "format.family"
	)
	export.pattern <- paste(
		"^get\\.response\\.name.*", "^get\\.response\\.var", "^calc\\..*",
		"^get\\.tunable\\.args.*", "^modify\\.args\\.predict.*",
		"^modify\\.args\\.model.*", "^modify\\.response\\.var.*",
		"^get\\.formula.*", "^get\\.response\\.class.*",
		"^format\\.prediction.*", "^detect\\.model\\.type.*", sep = "|"
	)
	clusterExport(
		cl,
		c(export.objects, ls(pattern = export.pattern, envir = .GlobalEnv))
	)
}

#-------------------------------------------------------------------------------
#	parallelパッケージのクラスターを初期化する関数。
#	Args:
#		n.cores: 計算に使うコアの数。
#
#	Value:
#		以下のオブジェクトが含まれたリスト。
#		cl: クラスターのハンドラ。コア数が１のときにはNULLが入ってる。
#		lapply:
#			lapplyとして使える関数。コア数が１だとlapply()、コア数が２以上だと
#			clusterApplyLB()を使うラッパー関数が格納される。
#		mapply:
#			mapplyとして使える関数。コア数が１だとmapply()、コア数が２以上だと
#			clusterMap()を使うラッパー関数が格納される。
#		close:
#			クラスターを閉じる関数。コア数が１だと何もしない関数、コア数が２以上
#			だとstopCluster()を呼び出してクラスターを閉じるラッパーが格納される。
#-------------------------------------------------------------------------------
init.cluster <- function(n.cores, seed = NULL){
	require(parallel)
	if (is.null(n.cores)) n.cores <- detectCores()
	if (n.cores != 1){
		result <- list(cl = makeCluster(n.cores))
		export.functions(result$cl)
		# 関数を割り当てて、クラスタのハンドラを送り込む。
		result$lapply <- cl.lapply
		result$mapply <- cl.mapply
		result$library <- cl.library
		assign("cl", result$cl, envir = environment(cl.lapply))
		assign("cl", result$cl, envir = environment(cl.mapply))
		assign("cl", result$cl, envir = environment(cl.library))
		result$close <- function() stopCluster(result$cl)
	} else {
		result <- list(
			cl = NULL, lapply = lapply, mapply = mapply,
			close = function(){}, library = ncl.library
		)
	}
	return(result)
}


#-------------------------------------------------------------------------------
#'	Model prediction with parallel computing.
#'
#'	This function call predict method with parallel computation. When predict 
#'	is called, newdata is splitted into (nearly) equall number of rows
#'
#'	@param object a model object to be passed to predict method.
#'	@param newdata passed to predict method.
#'	@param ... other arguments passed to predict method.
#'	@param n.cores
#'		number of cores used for prediction. If not specified, the number of 
#'		logical cores of the machine is used.
#'	@param package.names 
#'		a character vector specifying package names to be loaded in the cluster.
#'
#-------------------------------------------------------------------------------
#	並列計算でpredictを実行する。
#
#	Args:
#		object, newdata, ...: predictに渡される。
#		n.cores: 並列計算で使うコアの数
#		package.names:
#			クラスターでロードするパッケージ名を表すベクトル。
#			指定しないとobjectを元に自動判定する。
#-------------------------------------------------------------------------------
parallel.predict <- function(
	object, newdata = NULL, ..., n.cores = NULL,
	package.names = get.package.name(object)
){
	cl <- init.cluster(n.cores)
	# コアが１個だけだったら、ふつうのpredict結果を返す。
	if (is.null(cl$cl)){
		return(predict(object, newdata = newdata, ...))
	}
	on.exit(cl$close())
	cl$library(package.names)
	newdata = split(newdata, cut(1:nrow(newdata), breaks = length(cl$cl)))
	result <- cl$lapply(newdata, FUN = predict, object = object, ...)
	result <- do.call(c, result)
	return(result)
}


