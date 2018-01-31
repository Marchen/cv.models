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


#-------------------------------------------------------------------------------
#	スクリプトがあるディレクトリ名を返す関数。
#	http://stackoverflow.com/questions/1815606/rscript-determine-path-of-the-executing-script
#-------------------------------------------------------------------------------
get.this.file.dir <- function(){
	cmdArgs <- commandArgs(trailingOnly = FALSE)
	needle <- "--file="
	match <- grep(needle, cmdArgs)
	if (length(match) > 0) {
		# Rscript
		return(dirname(sub(needle, "", cmdArgs[match])))
	} else {
		# 'source'd via R console
		return(dirname(normalizePath(sys.frames()[[1]]$ofile)))
	}
}

library(model.adapter)
library(R6)
library(ranger)
library(gbm)
library(e1071)

#-------------------------------------------------------------------------------
#	ソース読み込み
#-------------------------------------------------------------------------------

# predictをgammとglmmMLに対応させる
source(file.path(get.this.file.dir(), "R", "cluster.manager.r"), encoding = "UTF-8")
source(file.path(get.this.file.dir(), "R", "cv.best.models.r"), encoding = "UTF-8")
source(file.path(get.this.file.dir(), "R", "classification.metrics.r"), encoding = "UTF-8")
source(file.path(get.this.file.dir(), "R", "regression.metrics.r"), encoding = "UTF-8")
source(file.path(get.this.file.dir(), "R", "cv.metrics.r"), encoding = "UTF-8")
source(file.path(get.this.file.dir(), "R", "cv.models.r"), encoding = "UTF-8")
source(file.path(get.this.file.dir(), "R", "utils.r"), encoding = "UTF-8")
source(file.path(get.this.file.dir(), "R", "methods.r"), encoding = "UTF-8")
source(file.path(get.this.file.dir(), "R", "which.min.max.r"), encoding = "UTF-8")

