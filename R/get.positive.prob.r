#-------------------------------------------------------------------------------
#	���f���̉����ϐ������q�^�������Ƃ��Apredict�̌��ʂ���z���Ƃ��Ĉ������q��
#	�m�������o���B
#
#	Args:
#		response: �����ϐ��̐��f�[�^�B
#		prediction: predict�̌���
#		positive.class:
#			�z���Ƃ��Ĉ����N���X��\��������B�w�肳��Ȃ������ꍇ��
#			(TRUE, FALSE), (1, 0), (+, -), (+, 0)�̃Z�b�g�̍�����z���Ƃ��Ĉ����A
#			�������I�ɗz���Ƃ��ăf�[�^�̎擾�����݂�B����ł��z��������ł��Ȃ�
#			�ꍇ�A�N���X�̂P�Ԗڂ�z���Ƃ��Ĉ����B
#			���q���R�N���X�ȏゾ�����ꍇ�A�P��ڂ̊m����p����B
#
#	Value:
#		response�����q�^�łȂ��ꍇ�A���̂܂�prediction��Ԃ��B
#		response�����q�^�̏ꍇ�A�z���̏�̕��@�Ŕ��肵���z���̊m����Ԃ��B
#-------------------------------------------------------------------------------
get.positive.prob <- function(response, prediction, positive.class = NULL){
	# ���q�^�ȊO�͂��̂܂ܒl��Ԃ��B
	if (!is.factor(response)){
		return(prediction)
	}
	# �z���Ƃ��Ďg���N���X��������Ȃ�������A�P�Ԗڂ�Ԃ��B
	positive.class <- get.positive.class(response, positive.class)
	if (is.null(positive.class)){
		return(prediction[, 1)
	} else {
		return(prediction[, positive.class])
	}
}


#-------------------------------------------------------------------------------
#	factor�^�̕ϐ��ŗz���̃��x���Ƃ��Ĉ����������肷��B
#
#	Args:
#		response: �����ϐ��̐��f�[�^�B
#		prediction: predict�̌���
#		positive.class:
#			�z���Ƃ��Ĉ����N���X��\��������B�w�肳��Ȃ������ꍇ��
#			(TRUE, FALSE), (1, 0), (+, -), (+, 0)�̃Z�b�g�̍�����z���Ƃ��Ĉ����A
#			�������I�ɗz���Ƃ��ăf�[�^�̎擾�����݂�B����ł��z��������ł��Ȃ�
#			�ꍇ�A�N���X�̂P�Ԗڂ�z���Ƃ��Ĉ����B
#			���q���R�N���X�ȏゾ�����ꍇ�A�P��ڂ̊m����p����B
#	Value:
#		�z���̃��x��������ł����Ƃ��ɂ͂��̃��x�����A����ł��Ȃ������Ƃ��ɂ�
#		NULL��Ԃ��B
#-------------------------------------------------------------------------------
get.positive.class <- function(response, positive.class = NULL){
	# �ǂ̃��x�����z�����w�肳��Ă�����A���̃��x���̊m����Ԃ��B
	if (!is.null(positive.class)){
		return(positive.class)
	}
	# �R�N���X�ȏ�̈��q�^�͂P�Ԗڂ̈��q���z�����Ɖ��肵�Ēl��Ԃ��B
	if (nlevels(response) > 2){
		warning("Number of classes of response variable was > 2 and 'positive.class' was not specified! \nCalculations were done by assuming the first level of the response variable is the positive case.")
		return(NULL)
	}
	# TRUE, 1, + �����x���Ɋ܂܂�Ă�����A������z���Ɖ��肵�Ēl��Ԃ��B
	classes <- levels(response)
	for (i in list(c("TRUE", "FALSE"), c("1", "0"), c("+", "-"), c("+", "0"))){
		if (identical(classes, i) | identical(classes, rev(i))){
			return(i[1])
		}
	}
	# ����ł��w�W������ł��Ȃ��Ƃ��ɂ͂P�Ԗڂ��z�����Ɖ��肵�Ēl��Ԃ��B
	warning("'positive.class' was not specified and positive class label could not be determined. \nMetrics were calculated by assuming the first level of the response variable is the positive case.")
	return(NULL)
}

