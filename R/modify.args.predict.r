#===============================================================================
#	predictに使う引数を指標計算で正しく動くように修正する総称関数。
#
#	Args:
#		object: モデルオブジェクト。
#		args.predict: predictに渡される引数が入ったリスト
#===============================================================================
modify.args.predict <- function(object, args.predict){
	UseMethod("modify.args.predict")
}

# defaultはtypeを"response"に書き換える。
# OK: glm, lm, gbm, cforest, svm, lmer, glmer, lme, randomForest
modify.args.predict.default <- function(object, args.predict){
	if (get.response.class(object) == "factor"){
		args.predict$type <- "prob"
	} else {
		args.predict$type <- "response"
	}
	return(args.predict)
}

modify.args.predict.tree <- function(object, args.predict){
	return(args.predict)
}

modify.args.predict.rpart <- function(object, args.predict){
	return(args.predict)
}

modify.args.predict.randomForest <- function(object, args.predict){
	if (object$type == "classification"){
		args.predict$type <- "prob"		# 識別問題ならprob
	} else {
		args.predict$type <- "response"	# 回帰ならresponse
	}
	return(args.predict)
}

modify.args.predict.svm <- function(object, args.predict){
	if (get.response.class(object) == "factor"){
		# 因子型だったら各クラスの確率も計算させる。
		args.predict$probability = TRUE
	}
	return(args.predict)
}