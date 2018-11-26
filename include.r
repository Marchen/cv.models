#===============================================================================
#	対応関数メモ
#
#	gbm:
#		distribution = "bernoulli"のとき、応答変数は0 or 1かTRUE or FALSEしか
#		受け付けない。因子型は受け付けない。
#
#	tree, rpart, randomForest:
#		0 or 1/TRUE or FALSEを連続値で予測するのと因子型で予測するので結果が違う
#	cforest, ctree
#		0 or 1/TRUE or FALSEを連続値で予測するのと因子型で予測するので結果は同じ
#
#===============================================================================

library(model.adapter)

#-------------------------------------------------------------------------------
#	スクリプトがあるディレクトリ名を返す関数。
#	http://stackoverflow.com/questions/1815606/rscript-determine-path-of-the-executing-script
#-------------------------------------------------------------------------------
get.this.file.dir <- function() {
	args <- commandArgs()
	with.file <- grepl("--file=", args)
	if (any(with.file)) {
		# The script was executed from Rscript.
		file.path <- sub("--file=", "", args[with.file])
	} else {
		# The script was sourced from R.
		file.path <- sys.frames()[[1]]$ofile
	}
	return(dirname(normalizePath(file.path)))
}


#-------------------------------------------------------------------------------
#	ソース読み込み
#-------------------------------------------------------------------------------

# predictをgammとglmmMLに対応させる
files <- list.files(file.path(get.this.file.dir(), "R"))
for (i in files) {
	source(file.path(get.this.file.dir(), "R", i), encoding = "UTF-8")
}

