#-------------------------------------------------------------------------------
#	lm関数用の.model.adapterクラスのジェネレータークラス。
#-------------------------------------------------------------------------------
.model.adapter.lm <- setRefClass(
	"model.adapter.lm", contains = "model.adapter",
	methods = list(
		get.model.type = function(cv.dummy, args.model, data){
			return(MODEL_TYPE_REGRESSION)
		}
	)
)

