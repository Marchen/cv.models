#=======================================================================
#'	Calculate weight of data
#'
#'	Calculate weight of data for each class of data to have equal total 
#'	amount of weight. Weight of a class are calculated by 
#'	1/(number of observation for the class).
#'
#'	@param data a vector representing class of data.
#'	@export
#=======================================================================
#	dataで与えられたカテゴリーの間で点の重みが均等になるように、
#	重みを計算する。データの重みは1/出現数。
#	Args:
#		data	: 重みを計算する因子型の変数。
#=======================================================================
make.weight <- function(data){
	result <- 1 / tapply(data, data, length)
	return(result[as.character(data)])
}

