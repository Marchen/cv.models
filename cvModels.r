#===============================================================================
#	いろいろなモデルに対してクロスバリデーションで性能指標を計算する。
#
#	今のところのクロスバリデーション対応関数
#		lm, glm, lme, glmmML, lmer, glmer, ctree, cforest, randomForest, 
#		gbm, svm, tree, rpart, mgcv::gam, mgcv::gamm, gam::gam
#
#	今のところのパラメーターチューニング対応関数
#		gbm (shrinkage, interaction.depth, n.minobsinnode, bag.fraction, n.trees)
#		randomForest (mtry)
#
#	対応不可能関数
#		MCMCglmm（predictがnewdataに対応してない）
#
#	各関数の注意点
#		全般:
#			predict関数のtypeは応答変数が因子型だと"prob"に、それ以外だと
#			"response"に自動的に書き換えられます。
#			応答変数が二項分布の時、応答変数をcbindで指定するのには対応していません。
#		lme:
#			predictがたまにNAを返すので、mseとかrmseが計算できないかも。
#			計算にna.omitを入れちゃう？
#		glmmML:
#			predictは自前で実装しています。
#		randomForest:
#			formulaを使ってモデルを指定する場合だけ対応。
#		svm:
#			formulaを使ってモデルを指定する場合だけに対応。
#			モデル作成時・predict時には自動的にprobability = TRUEが設定され、
#			各クラスの確率が計算されます。
#		lmer, glmer
#			ランダム効果のグループがCVのテストデータと学習データで違っちゃうと、
#			predictに失敗します。
#			そういうときは、args.predict = list(allow.new.levels = TRUE)を指定して、
#			新しいランダム効果のクラスが存在してもpredictが失敗しないように出来ます。
#			（どう考えるかはユーザーの目的次第だと思うので、自動設定はしていません）
#		tree, mgcv:
#			predictのtypeは書き換えていません。
#		gam:
#			デフォルトではmgcvパッケージのgam関数を呼びます。
#			gamパッケージのgam関数を呼びたいときには、package.name = "gam"を
#			指定してください。
#		gamm:
#			summaryやpredictにはgamm$gamオブジェクトを使っています。
#			predictするために、predict.gamm関数を定義してあり、予測時には
#			ランダム効果は無視して全体の関係式から予測値を計算しています。
#		gbm:
#			distribution = "bernoulli"の時には応答変数を0/1かTRUE/FALSEで
#			指定して下さい（gbmはそういう仕様になっています）。
#			因子型を指定すると、gbmの仕様上、計算がうまくいきません。
#
#	新しい関数への対応
#		・get.package.name()関数とget.class.name()関数を新しい関数に対応させる。
#		・必要なら、以下の関数を新しいモデル関数に対応させる。
#			detect.model.type
#			expand.dot
#			format.prediction
#			get.formula
#			get.response.class
#			get.response.name
#			get.tunable.args
#			modify.args.model
#			modify.args.predict
#			modify.response.var
#		・パラメーターチューニングに対応するには、get.tunable.args()関数を書く。
#		・必要ならprintとsummaryメソッドに対応を書く。
#
#	TODO:
#		☆☆☆
#			・positive classの判別がうまくいっていないっぽい。
#			・dredge & stepAIC
#			・get.positive.class関数の警告メッセージをcheck.args()関数へ移動。
#			・応答変数（とか）にlogをかましたりしたとき、うまく動かない問題。
#				きっとI(x^2)とかもだよね。
#			・cv.metrics()関数とかで、指標の再計算を出来るように。
#		☆☆
#			・glmとかの応答変数がcbindのとき
#			・detect.model.typeは応答変数がcbindの型のときうまく行かないはず。
#			・modify.args.predict.treeとrpartでtypeを書き換える。
#			・modify.args.predict.gbmでn.treesが指定されていなかったときの対応。
#		☆
#			・gbmとrandomForestはformulaを使わない方がパフォーマンスがよいらしい。
#				svmはformula使わなきゃないらしい。
#			・predicted probabilityのcolname
#			・predict.cv.best.models()
#			・cforestのチューニング対応？（要パラメーター選択）
#			・plot.cv.models関数 2d, 3d, 4d? parsp
#			・cv.more関数とmerge関数、more.metrics()関数
#
#-------------------------------------------------------------------------------
#	function		class			package
#	-----------------------------------------------
#	lm				lm				stats
#	glm				glm, lm			stats
#	lme				lme				nlme
#	glmmML			glmmML			glmmML
#	lmer			lmerMod			lme4
#	glmer			glmerMod		lme4
#	ctree			BinaryTree		party
#	cforest			RandomForest	party
#	randomForest	randomForest	randomForest
#	gbm				gbm				gbm
#	svm				svm.formula		e1071
#	tree			tree			tree
#	rpart			rpart			rpart
#	gam				gam				mgcv, gam
#	gamm			gamm			mgcv
#-------------------------------------------------------------------------------
#	使い方の例
#
#	# まずオブジェクトを作る
#	cvgbm <- cv.models(
#		gbm,									# 呼び出す関数を指定
#		args.model = list(						# モデル構築に使う引数を指定
#			formula = y ~ x1 + x2 + x3,
#			distribution = "bernoulli",
#			shrinkage=c(0.1, 0.01),				# チューニングするパラメーターは
#			interaction.depth = c(1, 3, 5)		# 複数指定可能（ただし、対応させれば）
#		),
#		args.predict = list(					# predictに渡す引数。
#			n.trees=c(1, 10, 100)				# チューニングするパラメーターは
#		),										# 複数指定可能（同上）
#		data = test.data,						# データは↑の引数に入れない
#		cv.metrics = c(							# 計算する性能評価指標を入れる
#			"auc", "mse",
#			"rmse", "informedness"
#		),
#		cv.folds = 10,							# CV分割数。デフォルトは10。
#		seed = 1,								# 結果を固定したいときにはseedを指定
#		n.cores = 4								# 計算に使うコア数を指定
#	)
#
#	cvglm <- cv.models(
#		glm,
#		args.model = list(formula = y ~ x1 + x2 + x3, family=binomial),
#		data = test.data, cv.metrics = c("auc", "mse", "rmse", "informedness"),
#		n.cores = 1
#	)
#
#	# CVの結果を見る。
#	cvgbm
#	cvglm
#
#	# ベストモデルの作成。この関数でベストモデルが作成される。
#	best1 <- get.best.models(
#		cvgbm,										# CV結果を指定
#		metrics = c("informedness","auc", "mse")	# モデル選択指標を指定
#													# 必ずmetrics = で名前指定。
#	)
#	summary(best1)
#
#	# 性能指標でタイが発生する可能性があるので、結果はオブジェクトが入ったリスト。
#	best1[[1]]$model			# ベストモデルのモデルオブジェクト
#	best1[[1]]$cv.metrics		# ベストモデルのCV指標
#	best1[[1]]$cv.prediction	# CV計算に使った予測値
#	best1[[1]]$cv.response		# CV計算に使った並べ替え済み応答変数
#	best1[[1]]$confusion.matrix	# 指標計算に使われたconfusion.matrix
#	best1[[1]]$function.name	# 一応保存されてる関数名
#
#	# 複数もOK
#	best2 <- get.best.models(
#		cvgbm, cvglm, metrics = c("informedness","auc", "mse")
#	)
#	summary(best2)
#
#
#-------------------------------------------------------------------------------
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

#-------------------------------------------------------------------------------
#	ソース読み込み
#-------------------------------------------------------------------------------

# predictをgammとglmmMLに対応させる
source(file.path(get.this.file.dir(), "R", "predict.method.r"), encoding = "CP932")

# 関数の違いを吸収する関数群
source(file.path(get.this.file.dir(), "R", "get.response.name.r"), encoding = "CP932")
source(file.path(get.this.file.dir(), "R", "get.response.class.r"), encoding = "CP932")
source(file.path(get.this.file.dir(), "R", "get.response.var.r"), encoding = "CP932")
source(file.path(get.this.file.dir(), "R", "get.args.r"), encoding = "CP932")
source(file.path(get.this.file.dir(), "R", "get.tunable.args.r"), encoding = "CP932")
source(file.path(get.this.file.dir(), "R", "expand.tunable.args.r"), encoding = "CP932")
source(file.path(get.this.file.dir(), "R", "merge.tunable.args.r"), encoding = "CP932")
source(file.path(get.this.file.dir(), "R", "format.prediction.r"), encoding = "CP932")
source(file.path(get.this.file.dir(), "R", "detect.model.type.r"), encoding = "CP932")
source(file.path(get.this.file.dir(), "R", "make.dummy.r"), encoding = "CP932")

# パラメーターの整合性を調整する関数群
source(file.path(get.this.file.dir(), "R", "modify.args.predict.r"), encoding = "CP932")
source(file.path(get.this.file.dir(), "R", "modify.args.model.r"), encoding = "CP932")
source(file.path(get.this.file.dir(), "R", "modify.response.var.r"), encoding = "CP932")
source(file.path(get.this.file.dir(), "R", "modify.args.r"), encoding = "CP932")

# クラス名・パッケージ名を取得する関数群
source(file.path(get.this.file.dir(), "R", "get.class.name.r"), encoding = "CP932")
source(file.path(get.this.file.dir(), "R", "get.package.name.r"), encoding = "CP932")

# クラスター関連関数群
source(file.path(get.this.file.dir(), "R", "cluster.r"), encoding = "CP932")

# モデル性能評価指標の計算関数群
source(file.path(get.this.file.dir(), "R", "cv.performance.r"), encoding = "CP932")
source(file.path(get.this.file.dir(), "R", "confusion.matrix.r"), encoding = "CP932")

# クロスバリデーション関連関数群
source(file.path(get.this.file.dir(), "R", "make.cv.group.r"), encoding = "CP932")
source(file.path(get.this.file.dir(), "R", "get.positive.prob.r"), encoding = "CP932")
source(file.path(get.this.file.dir(), "R", "cv.one.fold.r"), encoding = "CP932")
source(file.path(get.this.file.dir(), "R", "cross.validation.r"), encoding = "CP932")

# パラメーター選択関連関数群
source(file.path(get.this.file.dir(), "R", "run.dredge.r"), encoding = "CP932")
source(file.path(get.this.file.dir(), "R", "cv.models.r"), encoding = "CP932")

# ベストモデル選択関数群
source(file.path(get.this.file.dir(), "R", "which.min.max.r"), encoding = "CP932")
source(file.path(get.this.file.dir(), "R", "get.best.metrics.index.r"), encoding = "CP932")
source(file.path(get.this.file.dir(), "R", "construct.model.r"), encoding = "CP932")
source(file.path(get.this.file.dir(), "R", "get.best.models.r"), encoding = "CP932")
source(file.path(get.this.file.dir(), "R", "cv.best.models.r"), encoding = "CP932")

# その他ユーティリティ関数群
source(file.path(get.this.file.dir(), "R", "format.family.r"), encoding = "CP932")
source(file.path(get.this.file.dir(), "R", "make.weight.r"), encoding = "CP932")
source(file.path(get.this.file.dir(), "R", "utils.r"), encoding = "CP932")

#===============================================================================

# 関数の設定を保持するオブジェクトを操作
source(file.path(get.this.file.dir(), "R", "model.settings.r"), encoding = "CP932")

# モデルの違いを吸収するクラス
source(file.path(get.this.file.dir(), "R", "model.adapter.default.r"), encoding = "CP932")
source(file.path(get.this.file.dir(), "R", "model.adapter.gam.r"), encoding = "CP932")
source(file.path(get.this.file.dir(), "R", "model.adapter.gamm.r"), encoding = "CP932")
source(file.path(get.this.file.dir(), "R", "model.adapter.gbm.r"), encoding = "CP932")
source(file.path(get.this.file.dir(), "R", "model.adapter.glm.r"), encoding = "CP932")
source(file.path(get.this.file.dir(), "R", "model.adapter.glmer.r"), encoding = "CP932")
source(file.path(get.this.file.dir(), "R", "model.adapter.lm.r"), encoding = "CP932")
source(file.path(get.this.file.dir(), "R", "model.adapter.lme.r"), encoding = "CP932")
source(file.path(get.this.file.dir(), "R", "model.adapter.lmer.r"), encoding = "CP932")
source(file.path(get.this.file.dir(), "R", "model.adapter.svm.r"), encoding = "CP932")

# 新しい評価関数
source(file.path(get.this.file.dir(), "R", "model.perf.r"), encoding = "CP932")
