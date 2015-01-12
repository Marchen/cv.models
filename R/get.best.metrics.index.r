
#-------------------------------------------------------------------------------
#	最適な指標のインデックスを取り出す補助関数。
#	Supporting function to get index of the best metrics.
#
#	Args:
#		metrics: A matrix containing metrics to be maximized/minimized.
#-------------------------------------------------------------------------------
get.best.metrics.index <- function(metrics){
	minimize <- c(
		"mse", "rmse", "fn", "fp",
		"1-specificity", "1-sensitivity", "1-accuracy", "1-npv", "1-ppv"
	)
	for (i in colnames(metrics)){
		if (i %in% minimize){
			metrics[[i]] <- -metrics[[i]]
		}
	}
	return(which.max.multi(metrics))
}