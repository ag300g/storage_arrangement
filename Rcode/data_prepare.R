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

###########################################################################
################################## 数据预处理 #############################
###########################################################################
## 选取67号仓库进行计算, M表示这个仓库关联的sku之间关系(mii表示自己和仓库的关系)
load("./data/storelist.RData")

NO_store <- 67
NO_store <- 157
sku <- sku_store[sku_store$V1_2==NO_store,"V1_1"]
w_sku <- sku_store[sku_store$V1_2==NO_store,"V3"]
print(paste(paste(paste("与仓库",NO_store,sep=""),"相关的",sep=""),paste("sku数量:",length(sku),sep = ""),sep=""))

## sku_sku1是与67号仓库有关的sku之间的关系图
sku_sku1 <- sku_sku[sku_sku$V1_1 %in% sku & sku_sku$V1_2 %in% sku,c(2,3,4)]
print(paste(paste(paste("这",length(sku)),"sku之间的连边数:"),dim(sku_sku1)[1]))

## 调查有多少与67号仓库有关的孤立点sku
print(paste("是独立点的sku数量为:",length(setdiff(sku,union(sku_sku1$V1_1,sku_sku1$V1_2)))))

## 不考虑孤立点
sku_use_flag <- sku %in% union(sku_sku1$V1_1,sku_sku1$V1_2) 

sku_use <- sku[sku_use_flag]
wii <- w_sku[sku_use_flag]
wij <- sku_sku1$V3

## sku_sku1的行数就是所有边的数量
## i_index[t]是第t条边的yij对应的i
## j_index[t]是第t条边的yij对应的j
## i和j以sku_use中位置为准
i_index <- match(sku_sku1$V1_1,sku_use)
j_index <- match(sku_sku1$V1_2,sku_use)
setequal(sku_use,union(sku_sku1$V1_1,sku_sku1$V1_2))

## cost array
c <- c(wii,wij)
n_nodes <- length(wii)
n_edges <- length(wij)
## constraint matrix
ne <- 7*n_edges+n_nodes  ## non-zore number
## H1的右半边
h1r_ia <- 1:(2*n_edges) 
h1r_ja <- n_nodes+rep(1:n_edges,each=2)
h1r_ar <- rep(1,times=2*n_edges)
## H2的右半边
h2r_ia <- 2*n_edges+1:n_edges 
h2r_ja <- n_nodes+1:n_edges
h2r_ar <- rep(-1,times=n_edges)
## H1的左半边
h1l_ia <- 1:(2*n_edges)
h1l_ja <- as.vector(rbind(i_index,j_index))
h1l_ar <- rep(-1,times=2*n_edges)
## H2的左半边
h2l_ia <- 2*n_edges+rep(1:n_edges,each=2)
h2l_ja <- as.vector(rbind(i_index,j_index))
h2l_ar <- rep(1,times=2*n_edges)

ar <- c(h1l_ar,h1r_ar,h2l_ar,h2r_ar,rep(1,n_nodes))
ia <- c(h1l_ia,h1r_ia,h2l_ia,h2r_ia,rep(3*n_edges+1,n_nodes))
ja <- c(h1l_ja,h1r_ja,h2l_ja,h2r_ja,1:n_nodes)

save(n_nodes,file = "./data/n_nodes.RData")
save(n_edges,file = "./data/n_edges.RData")
save(ar,file = "./data/ar.RData")
save(ia,file = "./data/ia.RData")
save(ja,file = "./data/ja.RData")
save(c,file = "./data/c.RData")
save(ne,file = "./data/ne.RData")

