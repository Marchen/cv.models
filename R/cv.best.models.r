
#-------------------------------------------------------------------------------
#	cv.best.modelsクラス用のsummaryとprintメソッド
#
#	Args:
#		x: cv.best.modelsクラスのオブジェクト
#		...: summary関数に送られる引数。
#-------------------------------------------------------------------------------
summary.cv.best.models <- function(x, ...){
	for (i in x){
		cat("=========================\n")
		cat("Function name: ", i$function.name, "\n")
		# callが長すぎる問題に対処。たくさんありすぎるなら、総称関数？
		temp <- i$model
		if (is(temp, "lme")){
			temp$call <- list(data="<Truncated too long data representation>")
		} else if (is(temp, "lmerMod") | is(temp, "glmerMod")){
			temp@call <- call("NULL")
		} else if (!is(temp, "RandomForest") & !is(temp, "BinaryTree")){
			temp$call <- "<Truncated too long call>"
		}
		if (
			is(temp, "randomForest") | is(temp, "RandomForest")
			| is(temp, "BinaryTree")
		){
			print(temp)
		} else {
			print(summary(temp, ...))
		}
		cat("-------------------------\n")
		cat("Cross validation metrics:\n")
		print(i$cv.metrics)
		cat("\n")
	}
}

print.cv.best.models <- function(x, ...){
	cat("Best model selected by cross validation\n")
	for (i in x){
		cat("Function name: ", i$function.name, "\n")
		cat("Cross validation metrics:\n")
		print(i$cv.metrics)
		cat("\n")
	}
}

predict.cv.best.models <- function(object, ..., index = 1){
	predict(object$model[[index]], ...)
}

