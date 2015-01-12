#-------------------------------------------------------------------------------
#	�֐�������ł�������N���X�����擾����֐��B
#	�Ή����ĂȂ��֐��͊֐������N���X�����Ɖ��肵�Ċ֐�����Ԃ��B
#
#	Args:
#		function.name: �֐���
#
#	Value:
#		���̊֐������s�����Ƃ��ɏo����I�u�W�F�N�g�̃N���X����\��������B
#-------------------------------------------------------------------------------
get.class.name <- function(function.name){
	class.name <- switch(
		function.name,
		cforest	= "RandomForest",
		ctree	= "BinaryTree",
		lmer	= "lmerMod",
		glmer	= "glmerMod"
	)
	if (is.null(class.name)){
		class.name <- function.name
	}
	return(class.name)
}