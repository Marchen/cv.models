#-------------------------------------------------------------------------------
#	�֐�������p�b�P�[�W�����擾����֐��B
#	�Ή����ĂȂ��֐��͊֐������p�b�P�[�W�����Ɖ��肵�Ċ֐�����Ԃ��B
#
#	Args:
#		function.name: �֐���
#
#	Value:
#		���̊֐����܂܂��p�b�P�[�W����\��������B
#-------------------------------------------------------------------------------
get.package.name <- function(function.name){
	package.name <- switch(
		function.name,
		cforest	= "party",
		ctree	= "party",
		lm		= "stats",
		glm		= "stats",
		lme		= "nlme",
		lmer	= "lme4",
		glmer	= "lme4",
		svm		= "e1071",
		gam		= "mgcv",
		gamm	= "mgcv"
	)
	if (is.null(package.name)){
		package.name <- function.name
	}
	return(package.name)
}