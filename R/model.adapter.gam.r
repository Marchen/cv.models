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

#-------------------------------------------------------------------------------
#'	@describeIn expand.dot
#'	Method for \code{\link[gam]{gam}} function in \emph{gam} package and 
#'	\code{\link[mgcv]{gam}} function \emph{mgcv} package.
#'	@method expand.dot gam
#-------------------------------------------------------------------------------
.model.adapter.gam$methods(
	expand.dot = function(){
		# mgcv::gamとgam:gamで特殊文字の種類を変える。
		if (settings$package.name == "mgcv"){
			.self$callSuper(specials = c("s", "te", "ti", "t2"))
		} else {
			require(gam)
			.self$callSuper(specials = gam::gam.slist)
		}
	}
)


