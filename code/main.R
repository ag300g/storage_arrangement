load("./data/storelist.RData")
load("./data/sku_sku.RData")
load("./data/sku_store.RData")

source(file="code/Data_Process.FUN.R",encoding="utf-8")
source(file="code/Solve_Glpk.FUN.R",encoding="utf-8")
source(file="code/Solve_Greedy.FUN.R",encoding="utf-8")

storelist
store <- storelist$name[storelist$sku_NUM > 50 & storelist$sku_NUM < 1000]

comp <- list()
for (i in 1:length(store))
{ 
  NO_store <- store[i]
  p <- Data_Process(sku_sku,sku_store,NO_store)
  glpkresult <- Solve_Glpk(p$p1,p$p2)
  greedyresult <- Solve_Greedy(p$p2)
  comp[[i]] <- list(glpkresult=glpkresult,greedyresult=greedyresult)
}
names(comp) <- store
save(comp,file="./data/compare.RData")
