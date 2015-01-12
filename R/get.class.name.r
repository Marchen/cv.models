#-------------------------------------------------------------------------------
#	関数名からできあがるクラス名を取得する関数。
#	対応してない関数は関数名がクラス名だと仮定して関数名を返す。
#
#	Args:
#		function.name: 関数名
#
#	Value:
#		その関数を実行したときに出来るオブジェクトのクラス名を表す文字列。
#-------------------------------------------------------------------------------
get.class.name <- function(function.name){
	class.name <- switch(
		function.name,
		cforest	= "RandomForest",
		ctree	= "BinaryTree",
		lmer	= "lmerMod",
		glmer	= "glmerMod"
	)
	if (is.null(class.name)){
		class.name <- function.name
	}
	return(class.name)
}