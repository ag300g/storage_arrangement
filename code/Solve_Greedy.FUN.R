#' @description solve the subproblem using greedy algrithm
#' @param p the corresponding sku name and network'
#' @param NO_store the store number
Solve_Greedy <- function(p2){
  little_sku_sku <- p2$little_sku_sku
  
  edge_select <- little_sku_sku[which.max(little_sku_sku$V3),,drop=FALSE]
  sku_order <- order(little_sku_sku[which.max(little_sku_sku$V3),c(2,3),drop=FALSE])
  sku_select <- little_sku_sku[which.max(little_sku_sku$V3),c(2,3),drop=FALSE][sku_order]
  opt_seperate <- 
  opt <- 
    return(list(opt=opt,opt_seperate=opt_seperate,sku_select=sku_select,edge_select=edge_select))
}
