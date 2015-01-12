#-------------------------------------------------------------------------------
#	モデル作成に使うパラメーターのなかで、性能に影響するパラメーターがベクトルで
#	複数指定されたとき、その全ての組み合わせを作ってリストで返す補助関数。
#
#	Args:
#		object:
#			モデルオブジェクト。計算には使われないのでクラスが正しければ空でもOK。
#		args.model: モデル作成に使われる引数が入ったリスト。
#		type
#			"model": モデル構築に渡される引数だと仮定してパラメーターを取り出す。
#			"predict": predictに渡される引数だと仮定してパラメーターを取り出す。
#
#	Value:
#		モデル作成に使われる引数が入ったリストで複数指定された物が、その組み
#		合わせの一つで置き換えられたリストが入ったリスト。
#		list(args.model, args.model, args.model)という構造のデータ。
#-------------------------------------------------------------------------------
expand.tunable.args <- function(object, args, type){
	# 候補パラメーターの組み合わせを作成。
	tunable.args <- get.tunable.args(object, args, type)
	if (is.null(tunable.args)){
		return(list(args))
	}
	grid <- do.call(expand.grid, tunable.args)
	args[names(tunable.args)] <- NULL
	# モデル作成の引数候補を作成
	expanded.args <- replicate(nrow(grid), args, simplify = FALSE)
	expanded.args <- Map(c, expanded.args, split(grid, 1:nrow(grid)))
	return(expanded.args)
}