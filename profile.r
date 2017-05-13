source("C:/Users/mic/Dropbox/��������/��������/0000.00.00 Shared/include.r")
source("C:/Users/mic/OneDrive/��������/0000.00.00 Shared/include.r")
source("C:/Users/mic/Dropbox/��������/��������/cvModels/cvModels.r")
source("C:/Users/mic/OneDrive/��������/cvModels/cvModels.r")


#===============================================================================
#	���f���̍\�z
#===============================================================================

load(results.path("UNO/�n��2015/data/soba-data-20150125.rdata"))
load(results.path("UNO/�n��2015/data/kuri-data-20150125.rdata"))
load(results.path("UNO/�n��2015/data/daizu-data-20150125.rdata"))

setwd(results.path("UNO/�n��2015/gbm/"))

library(gbm)


run.gbm <- function(data, formula, model.name){
	# cross validation
	assign(
		sprintf("%s.cv", model.name),
		cv.models(
			gbm,
			args.model = list(
				formula = formula, n.minobsinnode = c(10, 20), n.trees = 10000,
				interaction.depth = c(5, 10), distribution = "gaussian"
			),
			args.predict = list(n.trees = seq(1, 10000, by = 100)), data = data,
			cv.metrics = c("threshold"),n.cores = 1
		)
	)
	cv <- eval(parse(text = sprintf("%s.cv", model.name)))
	# gbm���s
	assign(
		sprintf("%s.best", model.name),
		get.best.models(cv, metrics = c("threshold"))
	)
	best <- eval(parse(text = sprintf("%s.best", model.name)))
	# importance
	assign(
		sprintf("%s.varimp", model.name),
		summary.gbm(
			best[[1]]$model, n.trees = best[[1]]$cv.metrics[, "n.trees"],
			plotit = FALSE
		)
	)
	save(
		list = c(
			sprintf("%s.cv", model.name), sprintf("%s.best", model.name),
			sprintf("%s.varimp", model.name)
		),
		file = sprintf("%s-gbm-20150201.rdata", model.name)
	)
}


make.models <- function(data, response, month, crop.name){
	# �g��Ȃ������ϐ�������
	data[[
		ifelse(response == "mean.yield", "yield.per.area", "mean.yield")
	]] <- NULL
	# �O��l���� �����l���20�{�傫�ȃf�[�^���폜
	data[[response]] <- ifelse(
		data[[response]] >= median(data[[response]], na.rm = TRUE) * 20,
		NA, data[[response]]
	)
	formula <- test ~ .
	# ������Ԃ����̃f�[�^���쐬
	params <- c("pre", "rhu", "srd", "tmn", "tmp", "tmx", "wsd")
	toron.params <- c(
		"ci", "wi", "pre.summer", "tmp.year", "tmx.year", "tmn.year",
		"srd.year", "srd.summer"
	)
	grid <- expand.grid(params = params, month = (1:12)[!1:12 %in% month])
	grid <- paste(grid$param, grid$month, sep = ".")
	data.all <- data[
		!grepl(paste0("^", grid, "$", collapse = "|"), colnames(data))
	]
	data.all <- data.all[!colnames(data.all) %in% toron.params]
	data.all <- na.omit(data.all)
	# �Ƃ��p�����[�^�[�����̃��f�����쐬
	grid <- expand.grid(params = params, month = 1:12)
	grid <- paste(grid$param, grid$month, sep = ".")
	data.toron <- data[!colnames(data) %in% grid]
	data.toron <- na.omit(data.toron)
	# �Ƃ�񃂃f���\�z
	model.name <- sprintf("%s.toron", crop.name)
	run.gbm(data.toron, formula, model.name)
	gc()
	# �S�C�󃂃f���\�z
	model.name <- sprintf("%s.all", crop.name)
	run.gbm(data.all, formula, model.name)
	gc()
}

soba$test <- soba$mean.yield > 80


Rprof()
make.models(soba[1:2000, ], "mean.yield", 3:11, "soba")
Rprof(NULL)

make.models(daizu, "mean.yield", 4:12, "daizu")

make.models(kuri, "yield.per.area", 1:12, "kuri")


