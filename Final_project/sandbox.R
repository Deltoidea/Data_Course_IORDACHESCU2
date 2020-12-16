# convert to phydat format
alignment_phydat <- as.phyDat(alignment)

# distance matrix
dm  <- dist.ml(alignment_phydat)

treeNJ  <- NJ(dm)
plot(treeNJ)



tree <- pratchet(alignment_phydat)
tree <- nnls.phylo(tree, dm)
build_tree <- function(x){
  
  library(tidyverse)
  library(ShortRead)
  library(msa)
  library(phangorn)
  library(ggtree)
  library(purrr)
  
  
fa <- readDNAStringSet(x)

# do muscle alignment
alignment <- msa(fa)

#change tip labels
alignment@unmasked@ranges@NAMES <- 
  paste(alignment@unmasked@ranges@NAMES %>% str_split(pattern = " ") %>% map_chr(2),
        alignment@unmasked@ranges@NAMES %>% str_split(pattern = " ") %>% map_chr(3),
        sep = "_")

# convert to phydat format
alignment_phydat <- as.phyDat(alignment)

# distance matrix
dm  <- dist.ml(alignment_phydat)

treeNJ  <- NJ(dm)
plot(treeNJ)



tree <- pratchet(alignment_phydat)
tree <- nnls.phylo(tree, dm)

return(tree)
}


?geom_tiplab()
palette()



  plotTree(treeNJ,node.numbers=T,)+coord_polar()
rr.73 <- root(treeNJ,node = 73)