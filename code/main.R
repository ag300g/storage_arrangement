load("./data/storelist.RData")
load("./data/sku_sku.RData")
load("./data/sku_store.RData")

source(file="code/Data_Process.FUN.R",encoding="utf-8")
source(file="code/Solve_Glpk.FUN.R",encoding="utf-8")
source(file="code/Solve_Greedy.FUN.R",encoding="utf-8")

storelist
NO_store <- 75

p <- Data_Process(sku_sku,sku_store,NO_store)
glpkresult <- Solve_Glpk(p$p1,p$p2)
greedyresult <- Solve_Greedy(p$p2)
