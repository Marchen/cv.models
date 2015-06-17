

#-------------------------------------------------------------------------------
#	モデルの違いを吸収するアダプタークラスのオブジェクトを作る関数。
#	Rのポリモーフィズムを使わず、自前でクラスの初期化を行う。
#
#	Args:
#		settings: model.settingsのインスタンス
#
#	Value:
#		model.adapterクラスのオブジェクト
#
#	model.adapter
#		Field:
#			settings: model.settingsクラスのオブジェクト
#		Methods:
#			initialize(model.settings)
#			get.model.type()
#				モデルの種類とクラス数を取得
#-------------------------------------------------------------------------------
model.adapter <- function(settings){
	if (settings$is.default){
		code <- ".model.adapter.default$new()"
	} else {
		code <- sprintf(".model.adapter.%s$new()", settings$function.name)
	}
	object <- eval(parse(text = code))
	return(object)	
}

.model.adapter.default <- setRefClass(
	"model.adapter",
	fields = list(
		settings = "model.settings"
	)
)

#-------------------------------------------------------------------------------
#'	(Internal) Detect type of model.
#'
#'	This internal function detects type of model (classification/regression).
#'
#'	@inheritParams modify.args.model
#'	@return a character "classification" or "regression".
#-------------------------------------------------------------------------------
#	モデルが識別なのか回帰なのかを判定する。
#
#	Args:
#		cv.dummy: cv.dummyオブジェクト
#		args.model: モデル構築に使われるパラメーター。
#-------------------------------------------------------------------------------
.model.adapter.default$methods(
	get.model.type = function(){
		return(get.model.type.from.response.var())
	}
)

#-------------------------------------------------------------------------------
#'	(Internal) Detect type of model from family.
#'
#'	This internal function detects type of model from glm and mgcv::gam family.
#'
#'	@inheritParams modify.args.model
#'	@return a character "classification" or "regression".
#-------------------------------------------------------------------------------
.model.adapter.default$methods(
	get.model.type.from.family = function(){
		# familyがなかったらデフォルトは正規分布なので、回帰。
		if (is.null(settings$args.model$family)){
			return("regression")
		}
		# familyを文字列に変換。
		family <- format.family(settings$args.model$family, type = "character")
		# glmとgamの以下のfamilyが識別。それ以外は回帰
		classification.families <- c(
			"binomial", "quasibinomial", "negbin", "ocat", "nb",
			"betar", "cox.ph"
		)
		if (family %in% classification.families){
			return("classification")
		}
		return("regression")
	}
)

#-------------------------------------------------------------------------------
#'	(Internal) Detect type of model from class of response variable.
#'
#'	This internal function detects type of model from class of response variable.
#'
#'	@inheritParams modify.args.model
#'	@return a character "classification" or "regression".
#-------------------------------------------------------------------------------
.model.adapter.default$methods(
	get.model.type.from.response.var = function(){
		if (is(get.response.var(), "factor")){
			return("classification")
		} else {
			return("regression")
		}
	}
)

#-------------------------------------------------------------------------------
#'	Modify settings of modeling.
#'
#'	Internal function that hodifis parameters used for modeling.
#'
#'	@details
#'	\describe{
#'		\item{\code{\link[e1071]{svm}}}{
#'			This function set \emph{probability} in \emph{args.model} to TRUE.
#'		}
#'		\item{\code{\link[gbm]{gbm}}}{
#'			If the maximum value of n.trees specified in \emph{args.predict}
#'			is larger than the value of n.trees specified in \emph{args.model},
#'			this function change the value of n.trees in \emph{args.model} to the
#'			maximum value.
#'		}
#'		\item{
#'			merMod object created by \code{\link[lme4]{lmer}} and 
#'			\code{\link[lme4]{glmer}} functions, \code{\link[gam]{gam}} 
#'			in \emph{gam} and \emph{mgcv} packages, \code{\link[mgcv]{gamm}}
#'		}{
#'			This function expands '.' in specified formula and makes formula
#'			without '.'.
#'		}
#'	}
#'
#'	@return A list containing modified parameters for model construction.
#-------------------------------------------------------------------------------
#	モデルが性能指標を正しく計算するように、モデル構築の引数を修正する総称関数。
#-------------------------------------------------------------------------------
.model.adapter.default$methods(
	modify.args.model = function(){}
)

#-------------------------------------------------------------------------------
#'	Get formula from parameters.
#'
#'	This internal function retrieves formula from arguments used for modeling.
#'
#'	@inheritParams modify.args.model
#-------------------------------------------------------------------------------
#	モデル構築に使われる引数からモデル式をあらわすformulaを取得する。
#	lmeだけ、別処理。
#
#	Args:
#		object: モデルオブジェクト。計算には使われない。
#		args.model: モデル構築に使われる引数を入れたリスト。
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#'	@describeIn get.formula Default S3 method.
#'	This function is used for handling a result of
#'		\code{\link[stats]{lm}}, \code{\link[stats]{glm}},
#'		\code{\link[lme4]{lmer}}, \code{\link[lme4]{glmer}}, 
#'		\code{\link[party]{ctree}}, \code{\link[party]{cforest}}, 
#'		\code{\link[randomForest]{randomForest}}, \code{\link[gbm]{gbm}}, 
#'		\code{\link[e1071]{svm}}, \code{\link[tree]{tree}}, 
#'		\code{\link[rpart]{rpart}}, \code{\link[gam]{gam}} in \emph{gam} package,
#'		\code{\link[mgcv]{gam}} in \emph{mgcv} package and
#'		\code{\link[mgcv]{gamm}}.
#'	@method get.formula default
#-------------------------------------------------------------------------------
.model.adapter.default$methods(
	get.formula = function(){
		args.model <- settings$args.model
		if (!is.null(args.model$formula)){
			return(args.model$formula)
		} else {
			return(args.model[sapply(args.model, is.formula)][[1]])
		}
	}
)

#-------------------------------------------------------------------------------
#'	(Internal) Expand dot in formula.
#'
#'	This internal function expand dot ('.') in model formula used for modeling.
#'
#'	@inheritParams modify.args.model
#'	@param specials A vector of character which passed to 
#'	\code{\link[stats]{terms.formula}} function.
#-------------------------------------------------------------------------------
#	args.modelの中のformulaの.を実際の変数に置き換える総称関数。
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#'	@describeIn expand.dot
#'	Default S3 method. Intended to be used for \emph{lmerMod} object created by
#'	\code{\link[lme4]{lmer}} and \emph{glmerMod} object created by 
#'	\code{\link[lme4]{glmer}} function in \emph{lme4} package.
#'	@method expand.dot default
#-------------------------------------------------------------------------------
.model.adapter.default$methods(
	expand.dot = function(specials = NULL){
		# 式の準備
		f <- get.formula()
		f <- terms(f, data = settings$data, specials = specials)
		attributes(f) <- NULL
		f <- as.formula(f)
		args.model <- settings$args.model
		args.model[[which(sapply(args.model, is.formula))]] <- f
		settings$args.model <<- args.model
	}
)

#-------------------------------------------------------------------------------
#'	Get name of response variable from parameters.
#'
#'	This internal function retrieves the name of the response variable in 
#'	specified parameters used for modeling.
#'
#'	@inheritParams modify.args.model
#-------------------------------------------------------------------------------
#	応答変数の名前を返す総称関数
#	いまのところ、関数の対応必要なし。
#
#	Args:
#		object: モデルオブジェクト。
#		args.model: モデル構築に使われる引数を入れたリスト。
#
#	Value:
#		応答変数の名前を表す文字列。
#
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#'	@describeIn get.response.name
#'		Default S3 method. This function handles result of \code{\link[stats]{lm}}, 
#'		\code{\link[stats]{glm}}, \code{\link[nlme]{lme}} and 
#'		\code{\link[randomForest]{randomForest}} functions. 
#'	@method get.response.name default
#-------------------------------------------------------------------------------
.model.adapter.default$methods(
	get.response.name = function(){
		return(as.character(get.formula())[2])
	}
)




#-------------------------------------------------------------------------------
#'	Get response variable from parameters.
#'
#'	This internal function retrieves  the response variable in specified 
#'	parameters used for modeling.
#'
#'	@inheritParams modify.args.model
#'	@param data data used for modeling.
#-------------------------------------------------------------------------------
#	応答変数を返す。今のところ、関数への対応必要なし。
#
#	Args:
#		object: モデルオブジェクト
#		data: モデル構築に使われるデータ。
#		args.model: モデル構築に使われるパラメーター。
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#'	@describeIn get.response.var
#'	@method get.response.var default Default S3 method.
#-------------------------------------------------------------------------------
.model.adapter.default$methods(
	get.response.var = function(){
		return(data[[get.response.name()]])
	}
)

