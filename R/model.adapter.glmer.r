#-------------------------------------------------------------------------------
#	glmer関数用の.model.adapterクラスのジェネレータークラス。
#-------------------------------------------------------------------------------
.model.adapter.glmer <- setRefClass(
	"model.adapter.glmer", contains = "model.adapter",
	methods = list(
		get.model.type <- function(cv.dummy, args.model, data){
			return(get.model.type.from.family(args.model))
		}
	)
)

