Solve_Glpk <- function(p1,p2){
  # load package
  library(glpkAPI)
  # preparing the model
  # data prepared: n_nodess,n_edges,ne,ar,ia,ja,c
  load("../data/n_nodes.RData")
  load("../data/n_edges.RData")
  load("../data/ar.RData")
  load("../data/ia.RData")
  load("../data/ja.RData")
  load("../data/c.RData")
  load("../data/ne.RData")
  lp <- initProbGLPK()
  # direction of optimization
  setObjDirGLPK(lp, GLP_MAX)
  # model data 
  nrows <- 3*n_edges+1
  ncols <- n_nodes+n_edges
  # add rows and columns
  addRowsGLPK(lp, nrows)
  addColsGLPK(lp, ncols)
  setColsKindGLPK(lp,1:ncols,rep(GLP_BV,ncols))
  # objective function
  obj <- c
  # upper and lower bounds of the rows
  rlower <- rep(0,3*n_edges)
  rupper <- c(rep(0,2*n_edges),rep(1,n_edges),2)
  rbtype <- rep(GLP_UP,nrows)
  # upper and lower bounds of the columns
  clower <- rep(0,ncols)
  cupper <- rep(1,ncols)
  
  setColsBndsObjCoefsGLPK(lp, c(1:ncols), clower, cupper, obj)
  setRowsBndsGLPK(lp, c(1:nrows), rlower, rupper,type=rbtype)
  # load constraint matrix
  loadMatrixGLPK(lp, ne, ia, ja, ar)
  
  # setSimplexParmGLPK(MSG_LEV, GLP_ON)
  # solve lp problem
  # setMIPParmGLPK(GLP_RF_CLQ, GLP_ON)
  setMIPParmGLPK(MSG_LEV, GLP_MSG_ON)
  setMIPParmGLPK(PRESOLVE, GLP_ON)
  setMIPParmGLPK(CLQ_CUTS , GLP_ON)
  system.time(solveMIPGLPK(lp))
  
  # retrieve the results
  result <- list()
  result[[1]] <- mipStatusGLPK(lp)
  result[[2]] <- mipColsValGLPK(lp)
  result[[3]] <- mipObjValGLPK(lp)
  names(result) <- c("status","optimal_solution","optimal_value")
  save(result,file="../data/reslut_67.RData")
  delProbGLPK(lp)
  # delProbGLPK(lp)
}