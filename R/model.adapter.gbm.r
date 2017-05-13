#-------------------------------------------------------------------------------
#'	model.adapter class for gbm
#'
#'	This reference class contains methods for \code{\link[gbm]{gbm}} in 
#'	\emph{gbm} package.
#'	Following methods are overriden.
#-------------------------------------------------------------------------------
#	gbm関数用の.model.adapterクラスのジェネレータークラス。
#-------------------------------------------------------------------------------
.model.adapter.gbm <- setRefClass(
	"model.adapter.gbm", contains = "model.adapter"
)

#-------------------------------------------------------------------------------
#	モデルの種類を返す。
#-------------------------------------------------------------------------------
.model.adapter.gbm$methods(
	get.model.type = function(){
		"
		return a character vector specifying model type 
		(regression or classification).
		"
		# 分布が指定されているとき
		if (!is.null(settings$args.model$distribution)){
			# 以下が識別、それ以外は回帰として扱う。
			classification.families <- c(
				"bernoulli", "huberized", "multinomial", "adaboost"
			)
			if (settings$args.model$distribution %in% classification.families){
				return("classification")
			} else {
				return("regression")
			}
		}
		# 分布が指定されていなかったら、gbmと同じように推定。
		response <- get.response.var()
		if (nlevels(as.factor(response)) == 2){
			# 2クラスだったら識別。
			return("classification")
		}
		if (is(response, "factor")){
			# 応答変数が因子だったら識別。
			return("classification")
		}
		return("regression")
	}
)

#-------------------------------------------------------------------------------
#	モデル構築用のn.treesがpredict用のn.treesがよりも少なかったら、
#	自動的にn.treesを増やす。
#-------------------------------------------------------------------------------
.model.adapter.gbm$methods(
	modify.args.model = function(){
		"
		If the maximum value of n.trees specified in \\emph{args.predict}
		is larger than the value of n.trees specified in \\emph{args.model},
		this function change the value of n.trees in \\emph{args.model} to the
		maximum value.
		"
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

