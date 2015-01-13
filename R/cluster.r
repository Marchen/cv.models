
#-------------------------------------------------------------------------------
#	lapplyのラッパー
#-------------------------------------------------------------------------------
cl.lapply  <- function(X, FUN, ...){
	return(clusterApplyLB(cl = cl, x = X, fun = FUN, ...))
}

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
#		package.name: パッケージ名を表す文字列
#-------------------------------------------------------------------------------
cl.library <- function(package.name){
	ncl.library(package.name)
	expr <- parse(text = sprintf("require(%s)", package.name))
	clusterExport(cl, "expr", environment())
	clusterEvalQ(cl, eval(expr))
	clusterEvalQ(cl, rm(expr))
}

#-------------------------------------------------------------------------------
#	文字列で指定されたパッケージを読み込む関数。
#
#	Args:
#		package.name: パッケージ名を表す文字列
#-------------------------------------------------------------------------------
ncl.library <- function(package.name){
	expr <- parse(text = sprintf("require(%s)", package.name))
	eval(expr)
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
		"merge.tunable.args", "predict.gamm", "get.positive.prob"
	)
	export.pattern <- paste(
		"^get\\.response\\.name.*", "^calc\\..*", "^get\\.tunable\\.args.*",
		"^modify\\.args\\.predict.*", "^modify\\.args\\.model.*",
		"^get\\.response\\.class.*", "^format\\.prediction.*", sep = "|"
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



