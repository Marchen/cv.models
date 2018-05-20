#-------------------------------------------------------------------------------
#	Create grouping for cross validation.
#
#	Args:
#		object (cv.models):
#			a cv.models object having settings.
#			'seed', 'adapter' and 'folds' fields are used.
#-------------------------------------------------------------------------------
cv.group <- function(object) {
	# Fix random number before using random process.
	set.seed.if.possible(object)
	# 列の数を基にしてグループを作成してから、ランダムに並べ替える。
	cv.group <- ((1:nrow(object$adapter$data)) %% object$folds) + 1
	cv.group <- sample(cv.group, length(cv.group))
	return(cv.group)
}
