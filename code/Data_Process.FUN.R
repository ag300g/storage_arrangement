#' @description data preparation
#' @param sku_sku all the sku and sku relationship
#' @param sku_store all sku and store relationship
#' @param NO_store the number of the store be chosen
#' @return a list:glpk_param include all the params glpk used
#' @return a list:ori_net include:1, little_sku_sku network from the NO_store 
#'                                2, sku_use_flag

Data_Process <- function(sku_sku,sku_store,NO_store){
  sku <- sku_store[sku_store$V1_2==NO_store,"V1_1"] # do not have to unique
  w_sku <- sku_store[sku_store$V1_2==NO_store,"V3"] # correspondence to sku
  # print(paste(paste(paste("与仓库",NO_store,sep=""),"相关的",sep=""),paste("sku数量:",length(sku),sep = ""),sep=""))
  little_sku_sku <- sku_sku[sku_sku$V1_1 %in% sku & sku_sku$V1_2 %in% sku,c(2,3,4)]
  # print(paste(paste(paste("这",length(sku)),"sku之间的连边数:"),dim(little_sku_sku)[1]))
  # print(paste("是独立点的sku数量为:",length(setdiff(sku,union(little_sku_sku$V1_1,little_sku_sku$V1_2)))))
  ## 不考虑孤立点
  sku_non_isolated_flag <- sku %in% union(little_sku_sku$V1_1,little_sku_sku$V1_2)
  sku_non_isolated <- sku[sku_non_isolated_flag]
  wii <- w_sku[sku_non_isolated_flag]
  wij <- little_sku_sku$V3
  
  i_index <- match(little_sku_sku$V1_1,sku_non_isolated)
  j_index <- match(little_sku_sku$V1_2,sku_non_isolated)
  # setequal(sku_use,union(sku_sku1$V1_1,sku_sku1$V1_2))
  
  ## cost array
  c <- c(wii,wij)
  n_nodes <- length(wii)
  n_edges <- length(wij)
  ## constraint matrix
  ne <- 7*n_edges+n_nodes  ## non-zore number
  ## H1 right half
  h1r_ia <- 1:(2*n_edges) 
  h1r_ja <- n_nodes+rep(1:n_edges,each=2)
  h1r_ar <- rep(1,times=2*n_edges)
  ## H2 right half
  h2r_ia <- 2*n_edges+1:n_edges 
  h2r_ja <- n_nodes+1:n_edges
  h2r_ar <- rep(-1,times=n_edges)
  ## H1 left half
  h1l_ia <- 1:(2*n_edges)
  h1l_ja <- as.vector(rbind(i_index,j_index))
  h1l_ar <- rep(-1,times=2*n_edges)
  ## H2 left half
  h2l_ia <- 2*n_edges+rep(1:n_edges,each=2)
  h2l_ja <- as.vector(rbind(i_index,j_index))
  h2l_ar <- rep(1,times=2*n_edges)
  
  ar <- c(h1l_ar,h1r_ar,h2l_ar,h2r_ar,rep(1,n_nodes))
  ia <- c(h1l_ia,h1r_ia,h2l_ia,h2r_ia,rep(3*n_edges+1,n_nodes))
  ja <- c(h1l_ja,h1r_ja,h2l_ja,h2r_ja,1:n_nodes)
  
  p1 <- list(n_edges=n_edges,n_nodes=n_nodes,ne=ne,ar=ar,ia=ia,ja=ja,c=c)
  p2 <- list(little_sku_sku=little_sku_sku,sku_non_isolated=sku_non_isolated)
  return(list(p1=p1,p2=p2))
}