
#-------------------------------------------------------------------------------
#	�����ϐ��Ή��o�[�W������which.max��which.min�B
#	�������x�N�g���������ꍇ�A�ő�l�E�ŏ��l�̕]���͈����ɓ��ꂽ���Ԃōs����B
#	������matrix��data.frame�������ꍇ�A�]���͍��̗񂩂�E�̗�̏��Ԃōs����B
#	which.max�Ewhich.min�Ƃ͈Ⴂ�A�^�C���������ꍇ�A�S�ẴC���f�b�N�X��Ԃ��B
#
#	Args:
#		data: matrix�Adata.frame�A���������̃x�N�g�������������X�g�B
#		...: ���l�x�N�g���B
#
#	Multiple arguments version of which.min and which.max.
#	Evaluations of maximum/minimum are sequentially conducted in an order of 
#	variables provided in the arguments if the data were provided as numeric 
#	vectors.
#	If first argument is a matrix or data.frame, evaluation are sequentially
#	conducted left column to right column.
#	If there is a tie, this function returns ALL index of them. This behavior is
#	different from original which.min/which.max.
#
#	Args:
#		data: a matrix, data.frame or list containing same length of vectors.
#		...	: numeric vectors.
#-------------------------------------------------------------------------------
which.min.or.max.multi <- function(data, min.max.function){
	# �����̒������`�F�b�N
	# Check length of argument
	if (length(data) != 1 & !"data.frame" %in% class(data)){
		for (i in 2:length(data)){
			if (length(data[[1]]) != length(data[[i]])){
				stop("Length of the variables is not same!")
			}
		}
	}
	# �ő�l/�ŏ��l��T��
	# search max/min value
	index <- 1:length(data[[1]])
	for (i in 1:length(data)){
		current.data <- data[[i]][index]
		index <- index[current.data == min.max.function(current.data)]
		if (length(index) == 1){
			return(index)
		}
	}
	return(index)
}
# generic
which.min.multi <- function(data){
	UseMethod("which.min.multi")
}
which.max.multi <- function(data){
	UseMethod("which.max.multi")
}
# list
which.min.multi.list <- function(data){
	which.min.or.max.multi(data, min)
}
which.max.multi.list <- function(data){
	which.min.or.max.multi(data, max)
}
# data.frame
which.min.multi.data.frame <- function(data){
	which.min.or.max.multi(data, min)
}
which.max.multi.data.frame <- function(data){
	which.min.or.max.multi(data, max)
}
# matrix
which.min.multi.matrix <- function(data){
	which.min.multi.list(data.frame(data))
}
which.max.multi.matrix <- function(data){
	which.max.multi.list(data.frame(data))
}
# numeric
which.min.multi.numeric <- function(data, ...){
	which.min.multi.list(list(data, ...))
}
which.max.multi.numeric <- function(data, ...){
	which.max.multi.list(list(data, ...))
}
