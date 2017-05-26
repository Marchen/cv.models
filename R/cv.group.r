#-------------------------------------------------------------------------------
#	CVに使う分割グループを作る関数。今のところ、どのグループに割り当てられるかは
#	データの順番に依存せずにランダム。
#
#	Args:
#		data: 列数を取得するためのデータフレーム。
#		cv.folds: CVのfold数
#
#	Value:
#		どの列がどのグループに入るかを示す、dataの列数と長さが同じ整数ベクトル。
#-------------------------------------------------------------------------------
cv.group <- function(object) {
	# 列の数を基にしてグループを作成してから、ランダムに並べ替える。
	cv.group <- ((1:nrow(object$adapter$data)) %% object$folds) + 1
	cv.group <- sample(cv.group, length(cv.group))
	return(cv.group)
}



