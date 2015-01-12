#-------------------------------------------------------------------------------
#	関数名からパッケージ名を取得する関数。
#	対応してない関数は関数名がパッケージ名だと仮定して関数名を返す。
#
#	Args:
#		function.name: 関数名
#
#	Value:
#		その関数が含まれるパッケージ名を表す文字列。
#-------------------------------------------------------------------------------
get.package.name <- function(function.name){
	package.name <- switch(
		function.name,
		cforest	= "party",
		ctree	= "party",
		lm		= "stats",
		glm		= "stats",
		lme		= "nlme",
		lmer	= "lme4",
		glmer	= "lme4",
		svm		= "e1071",
		gam		= "mgcv",
		gamm	= "mgcv"
	)
	if (is.null(package.name)){
		package.name <- function.name
	}
	return(package.name)
}