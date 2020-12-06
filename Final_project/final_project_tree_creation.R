library(tidyverse)
library(phangorn)
library(ape)
library(seqinr)
library(beepr)
library(msa)
library(ShortRead)
library(phytools)
library(ggtree)
library(purrr)
library(broom)


#testing ####
# function needs a string pointing to fasta file
# aligns and build parsimony tree from that fasta



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
# ####































#Read in the sequences ####

#turn the fasta file into a dnaStringset
df <- readDNAStringSet("./seq_match_ficus_metadata.fas", format="fasta",
                 nrec=-1L, skip=0L, seek.first.rec=FALSE, use.names=TRUE)
df
#align sequences

alignment <- msaMuscle(inputSeqs = df,gapOpening = 400,gapExtension = 1,maxiters = 20,
          type = "dna")
#save alignment to file
saveRDS(file = "./output/alignment",alignment)
str(alignment)
alignment@rowmask

  
##change names in alignment ####
alignment <- read_rds("./output/alignment")
#add metadata
meta <- read_csv(file = "./ficus_metadata.csv")
meta$fasta_id <- str_remove(meta$fasta_id,">")
one <- meta%>%select(fasta_id)

alignment@unmasked@ranges@NAMES <- 
  paste(alignment@unmasked@ranges@NAMES %>% str_split(pattern = " ") %>% 
          map_chr(1),alignment@unmasked@ranges@NAMES %>% str_split(pattern = " ") 
        %>% map_chr(2),
        alignment@unmasked@ranges@NAMES %>% str_split(pattern = " ") %>% map_chr(3),
        sep = "_")
str(alignment)
one <- meta%>%select(fasta_id)
one <- as.vector(one)
two <- alignment@unmasked@ranges@NAMES%>%as.character()%>%sort()
three <- cbind(sort(one),sort(two))
three <- data.frame(sort(one),sort(two))

sortednames <- alignment@unmasked@ranges@NAMES%>%sort()
alignment@unmasked@ranges@NAMES%>%sort() <- meta$fasta_id%>%sort()
alignment@unmasked@ranges@NAMES%>%unique()
meta%>%order()


#write the alignment to a phyDat file

df_phydat <- as.phyDat(x = alignment)

unique(df_phydat)
#prepare alignment for Phangorn
read.phyDat("./alignedficus.dna",format = "fasta",type = "DNA")
write.phyDat(x = df_phydat,file = "./alignedficus.dna",format = "fasta")
phydat <- read.phyDat( "./alignedficus.dna",format = "fasta")


#build distance alignment
da <- dist.alignment(alignment)
# Build some trees from the alignment

fdir <- system.file("./extdata/trees", package = "phangorn")
par(mar=c(.1,.1,.1,.1))

dm  <- dist.ml(phydat);dm
#create some trees
treeUPGMA  <- upgma(dm)
treeNJ  <- NJ(dm);treeNJ


# test to see which tree is better
parsimony(treeUPGMA, phydat)

parsimony(treeNJ, phydat)

#optimum model
mt <- modelTest(phydat);print(mt)

bestAICmod <- mt[which(mt$AIC==min(mt$AIC)),1]
#moving into maximum likelihood. 
fit <- pml(treeUPGMA, phydat)
fitHKY <- optim.pml(fit, model = bestAICmod, rearrangement = "ratchet",)
class(fitHKY)
bootstrap <- bootstrap.pml(fitHKY, bs=100, optNni=TRUE, multicore=TRUE, control = pml.control(trace=0))
plotBS(midpoint(fitHKY$tree),bootstrap,p=10,type = "p",)





plotBS(bootstrap, main="bootstrap",)


p <- ggtree(bootstrap,branch.length = "none")
p %<+% meta + 
  geom_tiplab(aes(fill = factor(Location)),
              color = "black", # color for label font
              geom = "label",  # labels not text
              label.padding = unit(0.15, "lines"), # amount of padding around the labels
              label.size = 0) +# size of label border
  theme(legend.position = c(0.5,0.2), 
        legend.title = element_blank(), # no title
        legend.key = element_blank()) # no keys
fit <- pml(treeNJ,data = phydat);fit

msaplot(p=p, fasta="./alignedficus.dna", window=c(150, 175))
ggsave("./tree.png",units = "in",height = 35,width = 40)
#test different models

env <- attr(mt, "env")
ls(envir=env)
(fit <- eval(get("HKY+G+I", env), env))
summary(mt)

#add metadata
meta <- read_csv(file = "./ficus_metadata.csv")
meta

#
