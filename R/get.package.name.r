#-------------------------------------------------------------------------------
#	関数名やクラスのオブジェクト名からその関数/そのオブジェクトを生成した関数が
#	含まれるパッケージ名を取得する関数。対応してない関数は関数名やクラス名の
#	１つめがパッケージ名だと仮定してパッケージ名を返す。
#
#	Args:
#		x: 関数名を表す文字列、もしくはモデルオブジェクト。
#
#	Value:
#		その関数が含まれる/そのオブジェクトを作成した関数が含まれるパッケージ名
#		を表す文字列。
#-------------------------------------------------------------------------------
get.package.name <- function(x){
	UseMethod("get.package.name", x)
}

get.package.name.default <- function(x){
	# gamはglmやlmを引き継いでるので、先に評価
	if (is(x, "gam")){
		return(ifelse(is.null(x$mgcv.conv), "gam", "mgcv"))
	}
	if (is(x, "lm")) return("stats")
	if (is(x, "lme")) return("nlme")
	if (is(x, "lmerMod")) return("lme4")
	if (is(x, "glmerMod")) return("lme4")
	if (is(x, "BinaryTree")) return("party")
	if (is(x, "RandomForest")) return("party")
	if (is(x, "randomForest")) return("randomForest")
	if (is(x, "gbm")) return("gbm")
	if (is(x, "svm")) return("e1071")
	if (is(x, "tree")) return("tree")
	if (is(x, "rpart")) return("rpart")
	if (is(x, "gamm")) return("mgcv")
	return(class(x)[1])	
}

get.package.name.character <- function(x){
	package.name <- switch(
		x,
		cforest	= "party",
		ctree	= "party",
		lm		= "stats",
		glm		= "stats",
		lme		= "nlme",
		lmer	= "lme4",
		glmer	= "lme4",
		svm		= "e1071",
		gam		= "mgcv",
		gamm	= "mgcv",
		x
	)
	return(package.name)
}


