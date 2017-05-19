#' @description solve the subproblem using greedy algrithm
#' @param p the corresponding sku name and network'
#' @param NO_store the store number
Solve_Greedy <- function(p2){
  little_sku_sku <- p2$little_sku_sku
  w_sku <- p2$w_sku
  sku <- p2$sku
  edge_select <- little_sku_sku[which.max(little_sku_sku$V3),,drop=FALSE]
  sku_order <- order(little_sku_sku[which.max(little_sku_sku$V3),c(2,3),drop=FALSE])
  sku_select <- little_sku_sku[which.max(little_sku_sku$V3),c(2,3),drop=FALSE][sku_order]
  opt_seperate <- c(w_sku[sku %in% sku_select[1]],w_sku[sku %in% sku_select[2]],little_sku_sku[which.max(little_sku_sku$V3),1,drop=TRUE])
  opt <- sum(opt_seperate)
  return(list(opt=opt,opt_seperate=opt_seperate,sku_select=sku_select,edge_select=edge_select))
}
