#=======================================================================
#'	Calculate weight of data
#'
#'	Calculate weight of data for each class of data to have equal total 
#'	amount of weight. Weight of a class are calculated by 
#'	1/(number of observation for the class).
#'
#'	@param data a vector representing class of data.
#'	@export
#=======================================================================
#	data�ŗ^����ꂽ�J�e�S���[�̊Ԃœ_�̏d�݂��ϓ��ɂȂ�悤�ɁA
#	�d�݂��v�Z����B�f�[�^�̏d�݂�1/�o�����B
#	Args:
#		data	: �d�݂��v�Z������q�^�̕ϐ��B
#=======================================================================
make.weight <- function(data){
	result <- 1 / tapply(data, data, length)
	return(result[as.character(data)])
}

