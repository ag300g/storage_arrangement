load("./data/storelist.RData")
load("./data/sku_sku.RData")
load("./data/sku_store.RData")

source(file="code/Data_Process.FUN.R",encoding="utf-8")

storelist
NO_store <- 157

a <- Data_Process(sku_sku,sku_store,NO_store)