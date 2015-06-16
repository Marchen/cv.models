#-------------------------------------------------------------------------------
#	svm関数用の.model.adapterクラスのジェネレータークラス。
#-------------------------------------------------------------------------------
.model.adapter.svm <- setRefClass(
	"model.adapter.svm", contains = "model.adapter",
	methods = list(
		modify.args.model = function(){
			args.model <- settings$args.model
			args.model$probability <- TRUE
			settings$args.model.src <<- settings$args.model
			settings$args.model <<- args.model
		}
	)
)

