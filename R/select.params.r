# callの中身に新しい数式を設定する補助関数。
# object: cv.dummyオブジェクト。現状は特に使ってない。
# call: formulaを設定するcall。
# new.formula: 設定する新しいformula。
set.formula <- function(object, call, new.formula) {
	if (!is.null(eval(call$args.model$formula))) {
		call$args.model$formula <- new.formula
	} else {
		call$args.model[
			c(FALSE, sapply(eval(call$args.model), is.formula))
		][[1]] <- new.formula
	}
	return(call)
}


# クロスバリデーションを実行し、ベストモデルを取得し、callを保存する補助関数。
# call: 実行するcv.modelsのcall。
# metric: ベストモデルを選ぶ基準を表す文字列。
eval.model <- function(call, metric) {
	result <- eval(call)
	result$call <- call
	result$best <- get.best.models(result, metrics = metric)
	return(result)
}


# x: 除去する変数名
# call: モデルの呼び出し式
# metric: ベストモデルを選ぶ基準にする指標名。
next.model <- function(x, call, metric) {
	# モデル式を作成する。
	dummy <- make.dummy(as.character(call$model.function), call$package.name)
	args.model <- expand.dot(dummy, eval(call$args.model), eval(call$data))
	formula <- get.formula(dummy, args.model)
	formula <- update.formula(formula, sprintf(". ~ . - %s", x))
	call <- set.formula(dummy, call, formula)
	return(eval.model(call, metric))
}


#	オブジェクトのベストモデルから指定した指標を取り出す補助関数。
#	x: モデルオブジェクト
#	metric: 指標名を表す文字列。
get.metric <- function(x, metric) {
	if (is.data.frame(x$best[[1]]$cv.metrics)) {
		return(x$best[[1]]$cv.metrics[, metric])
	} else {
		return(x$best[[1]]$cv.metrics)
	}
}


# モデルから除去できる項の一覧を取得する。
#	call: cv.modelsの呼び出しを含むcall。
get.drop.terms <- function(call) {
	dummy <- make.dummy(as.character(call$model.function), call$package.name)
	args.model <- expand.dot(dummy, eval(call$args.model), eval(call$data))
	formula <- get.formula(dummy, args.model)
	drop.terms <- drop.scope(formula)
	return(drop.terms)
}


#==============================================================================
#	クロスバリデーションで計算した性能指標を使ってモデルの説明変数の選択を行う。
#	今のところ、Backward model selectionにだけ対応。
#
#	Args:
#		x:
#			パラメーターを選びたいフルモデルの性能評価を行う
#			cv.modelsの呼び出し。↓の例を参照。
#		metric:
#			パラメーター選択で用いる性能評価指標の名前。
#			２つ以上には対応していない。
#		n.cores:
#			モデル選択に計算に使うコアの数。
#			実際に使われる計算コアの数はこのn.cores × cv.modelsのn.coresになる。
#			例えば下の例だと、計算に使用されるコアは４×２＝８個になる。
#			全体で実際のコア数を超えない方が計算が速いと思う。
#			選択するパラメーターたぶん、cv.modelsのコアを１にｓ
#
#	Value:
#		以下の値を含んだリスト。
#		initial:
#			初期モデルの拡張版cv.modelsオブジェクト（※）。
#		models:
#			各ステップで計算した、パラメータを除去したモデルの性能を表す
#			拡張版cv.modelsオブジェクトが入ったリスト。
#			list(
#				list(cv.models1, cv.models2,...),
#				list(cv.models1, cv.models2, ...),
#				...
#			)
#		best:
#			最良モデルの性能指標が入った拡張cv.modelsオブジェクト。
#
#			（※）$bestにcv.best.modelオブジェクトが入っている。
#
#	Example:
#		# データの準備
#		data(iris)
#		set.seed(12345)
#		iris$random1 <- runif(nrow(iris))
#		iris$random2 <- runif(nrow(iris))
#		iris$random3 <- runif(nrow(iris))
#		iris$random4 <- runif(nrow(iris))
#
#		# パラメーター選択実行。
#		models <- select.params(
#			cv.models(
#				glm, args.model = list(Sepal.Length ~ ., family = gaussian),
#				data = iris, cv.metrics = "r.squared", n.cores = 4, seed = 12345
#			),
#			"r.squared", n.cores = 2
#		)
#
#		# 結果を見てみる。
#		summary(cv$best$best)
#
#	現時点でわかっている問題
#	・lmeみたいにformulaが複数あるモデルには正しく対応してない。
#	・mseやrmseのように、モデルの当てはまりがよくなると
#	  値が小さくなるような指標に対応してない。
#	・真剣にテストしてない。
#
#==============================================================================
select.params <- function(x, metric, n.cores = 1) {
	# フルモデルを作成
	call <- match.call(cv.models, substitute(x))
	result <- list(models = list())
	prev.model <- result$initial <- eval.model(call, metric)
	# 除去可能な変数リストを作成。
	drop.terms <- get.drop.terms(call)
	# クラスター準備
	if (n.cores > 1) {
		require(parallel)
		cl <- makeCluster(detectCores())
		on.exit(stopCluster(cl))
		clusterExport(cl, varlist = ls(.GlobalEnv, all = TRUE))
	}
	# インデックスを初期化
	index = 1
	while (1) {
		# 候補モデルを作成。
		if (n.cores > 1) {
			result$models[[index]] <- clusterApplyLB(
				cl, drop.terms, next.model, call = call, metric = metric
			)
		} else {
			result$models[[index]] <- lapply(
				drop.terms, next.model, call = call, metric = metric
			)
		}
		names(result$models[[index]]) <- drop.terms
		# 性能評価指標を抽出
		metrics <- sapply(result$models[[index]], get.metric, metric)
		# ひとつ前のモデルの性能が高かったら処理を抜ける。
		if (all(metrics < get.metric(prev.model))) {
			break
		}
		# 次のステップの準備
		prev.model <- result$models[[index]][[which.max(metrics)]]
		index <- index + 1
		drop.terms <- get.drop.terms(prev.model$call)
		call <- prev.model$call
		# すべての項がなくなったら処理を抜ける。
		if (length(drop.terms) == 0) {
			break
		}
	}
	result$best <- prev.model
	return(result)
}


#==============================================================================
#	実行例
#==============================================================================
if (0) {
	source("cvModels.r", encoding = "CP932")
	data(iris)
	#	set.seed(12345)
	iris$random1 <- runif(nrow(iris))
	iris$random2 <- runif(nrow(iris))
	iris$random3 <- runif(nrow(iris))
	iris$random4 <- runif(nrow(iris))

	cv <- select.params(
		cv.models(
			randomForest, args.model = list(Sepal.Length ~ .), data = iris,
			cv.metrics = c("r.squared"), n.cores = 1 #, seed = 12345
		),
		"r.squared", n.cores = 2
	)

	# 初期モデルの当てはまりを確認。
	get.metric(cv$initial, "r.squared")

	# 第１ステップの当てはまり。
	# random1を抜くのが一番当てはまりがよいので、random1が抜ける。
	sapply(cv$models[[1]], get.metric, "r.squared")

	# 第２ステップの当てはまり。
	# random3を抜くのが一番当てはまりがよいので、random3が抜ける。
	sapply(cv$models[[2]], get.metric, "r.squared")

	# 第３ステップの当てはまり。
	# random4を抜くのが一番当てはまりがよいので、random4が抜ける。
	sapply(cv$models[[3]], get.metric, "r.squared")

	# 第４ステップの当てはまり。
	# 前のステップのrandom4を抜かしたモデルがどのモデルよりも当てはまりがよいので、
	# ここで終了。
	sapply(cv$models[[4]], get.metric, "r.squared")

	# ベストモデルを確認。
	summary(cv$best$best)

	# ベストモデルを取り出し。
	best <- cv$best$best[[1]]$model
}
