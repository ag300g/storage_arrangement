#' @description data preparation
#' @param sku_sku all the sku and sku relationship
#' @param sku_store all sku and store relationship
#' @param NO_store the number of the store be chosen
#' @return a list:glpk_param include all the params glpk used
#' @return a list:ori_net include 1, sku_sku network from the NO_store 2, 

Data_Process <- function(sku_sku,sku_store,NO_store){
  sku <- unique(sku_store[sku_store$V1_2==NO_store,"V1_1"])
  w_sku <- sku_store[sku_store$V1_2==NO_store,"V3"]
  # print(paste(paste(paste("与仓库",NO_store,sep=""),"相关的",sep=""),paste("sku数量:",length(sku),sep = ""),sep=""))
  little_sku_sku <- sku_sku[sku_sku$V1_1 %in% sku & sku_sku$V1_2 %in% sku,c(2,3,4)]
  # print(paste(paste(paste("这",length(sku)),"sku之间的连边数:"),dim(little_sku_sku)[1]))
  # print(paste("是独立点的sku数量为:",length(setdiff(sku,union(little_sku_sku$V1_1,little_sku_sku$V1_2)))))
  ## 不考虑孤立点
  sku_use_flag <- sku %in% union(little_sku_sku$V1_1,little_sku_sku$V1_2)
  sku_use <- sku[sku_use_flag]
  wii <- w_sku[sku_use_flag]
  wij <- little_sku_sku$V3
}