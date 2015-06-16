#-------------------------------------------------------------------------------
#	gbm関数用の.model.adapterクラスのジェネレータークラス。
#-------------------------------------------------------------------------------
.model.adapter.gbm <- setRefClass(
	"model.adapter.gbm", contains = "model.adapter"
}

.model.adapter.gbm$methods(
	get.model.type = function(cv.dummy, args.model, data){
		# 分布が指定されているとき
		if (!is.null(args.model$distribution)){
			# 以下が識別、それ以外は回帰として扱う。
			classification.families <- c(
				"bernoulli", "huberized", "multinomial", "adaboost"
			)
			if (args.model$distribution %in% classification.families){
				return(MODEL_TYPE_CLASSIFICATION)
			} else {
				return(MODEL_TYPE_REGRESSION)
			}
		}
		# 分布が指定されていなかったら、gbmと同じように推定。
		response <- get.response.var(cv.dummy, data, args.model)
		if (nlevels(as.factor(response)) == 2){
			# 2クラスだったら識別。
			return(MODEL_TYPE_CLASSIFICATION)
		}
		if (is(response, "factor")){
			# 応答変数が因子だったら識別。
			return(MODEL_TYPE_CLASSIFICATION)
		}
		return(MODEL_TYPE_REGRESSION)
	}
)

#-------------------------------------------------------------------------------
#'	@describeIn modify.args.model
#'	Method for \code{\link[gbm]{gbm}} object in \emph{gbm} package.
#'	@method modify.args.model gbm
#-------------------------------------------------------------------------------
#	モデル構築用のn.treesがpredict用のn.treesがよりも少なかったら、
#	自動的にn.treesを増やす。
#-------------------------------------------------------------------------------
.model.adapter.gbm$methods(
	modify.args.model = function(){
		args.model <- settings$args.model
		args.predict <- settings$args.predict
		if (!is.null(args.predict$n.trees)){
			n.trees.predict <- max(args.predict$n.trees)
			n.trees.model <- ifelse(
				is.null(args.modesl$n.trees), 100, args.model$n.trees
			)
			if (n.trees.model < n.trees.predict){
				args.model$n.trees <- n.trees.predict
			}
		}
		settings$args.model.src <<- settings.args.model
		settings$args.model <<- args.model
	}
)

