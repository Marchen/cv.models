#-------------------------------------------------------------------------------
#	gam関数用の.model.adapterクラスのジェネレータークラス。
#-------------------------------------------------------------------------------
.model.adapter.gam <- setRefClass(
	"model.adapter.gam", contains = "model.adapter",
	methods = list(
		get.model.type = function(cv.dummy, args.model, data){
			return(get.model.type.from.family(args.model))
		}
	)
)

