data1 <- read.table("./data/kl_sku_sku_6_0401",sep = "\t",fileEncoding="utf-8",comment.char = "")
data2 <- read.table("./data/kl_sku_store_6_0401",sep = "\t",fileEncoding="utf-8",comment.char = "")

library(splitstackshape)
sku_sku <- as.data.frame(cSplit(data1, "V1", "#"))
sku_store <- as.data.frame(cSplit(data2, "V1", "#"))
dim(sku_sku)
dim(sku_store)

## sku_sku之间的关系是单向的,AB出现之后就不会再出现BA
head(sku_sku)
sku_sku[sku_sku[,3]==2518073 & sku_sku[,4]==2655649,]
## 查询每个仓库对应的sku的数量
store <- unique(sku_store$V1_2)
n_store <- length(store)
p_scale <- rep(0,n_store)
w_store <- rep(0,n_store)
names(p_scale) <- store
names(w_store) <- store
for(i in 1:n_store){
  p_scale[i] <- length(sku_store[sku_store$V1_2==store[i],1])
  w_store[i] <- sum(sku_store[sku_store$V1_2==store[i],2])
}
which.max(w_store)
which.max(p_scale)
# choose the NO.37 store as the target store
# choose the NO.67 store as the test store
save(sku_sku,file="./data/sku_sku.RData")
save(sku_store,file="./data/sku_store.RData")
storelist <- list()
storelist[[1]] <- store
storelist[[2]] <- p_scale
storelist[[3]] <- w_store
names(storelist) <- c("name","sku_NUM","weight")
save(storelist,file="./data/storelist.RData")