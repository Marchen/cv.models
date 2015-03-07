#-------------------------------------------------------------------------------
#	�֐�����N���X�̃I�u�W�F�N�g�����炻�̊֐�/���̃I�u�W�F�N�g�𐶐������֐���
#	�܂܂��p�b�P�[�W�����擾����֐��B�Ή����ĂȂ��֐��͊֐�����N���X����
#	�P�߂��p�b�P�[�W�����Ɖ��肵�ăp�b�P�[�W����Ԃ��B
#
#	Args:
#		x: �֐�����\��������A�������̓��f���I�u�W�F�N�g�B
#
#	Value:
#		���̊֐����܂܂��/���̃I�u�W�F�N�g���쐬�����֐����܂܂��p�b�P�[�W��
#		��\��������B
#-------------------------------------------------------------------------------
get.package.name <- function(x){
	UseMethod("get.package.name", x)
}

get.package.name.default <- function(x){
	# gam��glm��lm�������p���ł�̂ŁA��ɕ]��
	if (is(x, "gam")){
		return(ifelse(is.null(x$mgcv.conv), "gam", "mgcv"))
	}
	if (is(x, "lm")) return("stats")
	if (is(x, "lme")) return("nlme")
	if (is(x, "lmerMod")) return("lme4")
	if (is(x, "glmerMod")) return("lme4")
	if (is(x, "BinaryTree")) return("party")
	if (is(x, "RandomForest")) return("party")
	if (is(x, "randomForest")) return("randomForest")
	if (is(x, "gbm")) return("gbm")
	if (is(x, "svm")) return("e1071")
	if (is(x, "tree")) return("tree")
	if (is(x, "rpart")) return("rpart")
	if (is(x, "gamm")) return("mgcv")
	return(class(x)[1])	
}

get.package.name.character <- function(x){
	package.name <- switch(
		x,
		cforest	= "party",
		ctree	= "party",
		lm		= "stats",
		glm		= "stats",
		lme		= "nlme",
		lmer	= "lme4",
		glmer	= "lme4",
		svm		= "e1071",
		gam		= "mgcv",
		gamm	= "mgcv",
		x
	)
	return(package.name)
}


