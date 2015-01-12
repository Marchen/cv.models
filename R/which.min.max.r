
#-------------------------------------------------------------------------------
#	複数変数対応バージョンのwhich.maxとwhich.min。
#	引数がベクトルだった場合、最大値・最小値の評価は引数に入れた順番で行われる。
#	引数がmatrixやdata.frameだった場合、評価は左の列から右の列の順番で行われる。
#	which.max・which.minとは違い、タイがあった場合、全てのインデックスを返す。
#
#	Args:
#		data: matrix、data.frame、同じ長さのベクトルが入ったリスト。
#		...: 数値ベクトル。
#
#	Multiple arguments version of which.min and which.max.
#	Evaluations of maximum/minimum are sequentially conducted in an order of 
#	variables provided in the arguments if the data were provided as numeric 
#	vectors.
#	If first argument is a matrix or data.frame, evaluation are sequentially
#	conducted left column to right column.
#	If there is a tie, this function returns ALL index of them. This behavior is
#	different from original which.min/which.max.
#
#	Args:
#		data: a matrix, data.frame or list containing same length of vectors.
#		...	: numeric vectors.
#-------------------------------------------------------------------------------
which.min.or.max.multi <- function(data, min.max.function){
	# 引数の長さをチェック
	# Check length of argument
	if (length(data) != 1 & !"data.frame" %in% class(data)){
		for (i in 2:length(data)){
			if (length(data[[1]]) != length(data[[i]])){
				stop("Length of the variables is not same!")
			}
		}
	}
	# 最大値/最小値を探索
	# search max/min value
	index <- 1:length(data[[1]])
	for (i in 1:length(data)){
		current.data <- data[[i]][index]
		index <- index[current.data == min.max.function(current.data)]
		if (length(index) == 1){
			return(index)
		}
	}
	return(index)
}
# generic
which.min.multi <- function(data){
	UseMethod("which.min.multi")
}
which.max.multi <- function(data){
	UseMethod("which.max.multi")
}
# list
which.min.multi.list <- function(data){
	which.min.or.max.multi(data, min)
}
which.max.multi.list <- function(data){
	which.min.or.max.multi(data, max)
}
# data.frame
which.min.multi.data.frame <- function(data){
	which.min.or.max.multi(data, min)
}
which.max.multi.data.frame <- function(data){
	which.min.or.max.multi(data, max)
}
# matrix
which.min.multi.matrix <- function(data){
	which.min.multi.list(data.frame(data))
}
which.max.multi.matrix <- function(data){
	which.max.multi.list(data.frame(data))
}
# numeric
which.min.multi.numeric <- function(data, ...){
	which.min.multi.list(list(data, ...))
}
which.max.multi.numeric <- function(data, ...){
	which.max.multi.list(list(data, ...))
}
