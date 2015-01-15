
#-------------------------------------------------------------------------------
#	関数名を元にして、その関数が返すオブジェクトの偽物を返す関数。
#
#	Args:
#		function.name: 関数名を表す文字列。
#-------------------------------------------------------------------------------
make.dummy <- function(function.name, package.name){
	object <- list(package = package.name)
	class(object) <- c(get.class.name(function.name), "cv.dummy")
	return(object)
}
