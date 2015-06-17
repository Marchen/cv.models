#-------------------------------------------------------------------------------
#	lmer関数用の.model.adapterクラスのジェネレータークラス。
#-------------------------------------------------------------------------------
.model.adapter.lmer <- setRefClass(
	"model.adapter.lmer", contains = "model.adapter"
)

.model.adapter.lmer$methods(
	get.model.type = function(){
		return("regression")
	}
)

