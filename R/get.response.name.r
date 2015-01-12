
#===============================================================================
#	応答変数の名前を返す総称関数
#	ここでモデルに対応するように総称関数を追加すると、新しい関数に対応できる。
#
#	Args:
#		object: モデルオブジェクト
#
#	Value:
#		応答変数の名前を表す文字列。
#===============================================================================
get.response.name <- function(object){
	UseMethod("get.response.name")
}

# lm, glm, lme, glmmML, randomForest, svm
get.response.name.default <- function(object){
	return(as.character(object$terms[[2]]))
}
# gbm
get.response.name.gbm <- function(object){
	return(object$response.name)
}
# cforest
get.response.name.RandomForest <- function(object){
	return(names(object@responses@variables))
}
# cforest
get.response.name.BinaryTree <- function(object){
	return(names(object@responses@variables))
}
# lmer
get.response.name.lmerMod <- function(object){
	return(names(object@frame)[1])
}
# glmer
get.response.name.glmerMod <- function(object){
	return(names(object@frame)[1])
}
# gamm
get.response.name.gamm <- function(object){
	get.response.name(object$gam)
}
