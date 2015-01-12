#-------------------------------------------------------------------------------
#	argsで指定されたリストの中からarg.namesで指定されたデータを取り出す補助関数。
#	argsの中にデータがないものは無視される（NULLやNAは返らない）。
#
#	Args:
#		args: データ入ったリスト。
#		arg.names: 取り出す変数名のベクトル。
#
#	Value:
#		データの入ったリスト。
#-------------------------------------------------------------------------------
get.args <- function(args, arg.names){
	arg.names <- arg.names[arg.names %in% names(args)]
	result <- args[arg.names]
	if (length(result) == 0 ){
		result <- NULL
	}
	return(result)
}