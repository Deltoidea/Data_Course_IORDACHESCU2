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
library(ggrepel)






#Read in the sequences ####

#turn the fasta file into a dnaStringset
df <- readDNAStringSet("./reducedseqmatch.fas", format="fasta",
                 nrec=-1L, skip=0L, seek.first.rec=FALSE, use.names=TRUE)
df
#align sequences

alignment <- msaClustalOmega(inputSeqs = df,gapOpening = 400,gapExtension = 1,maxiters = 20,
          type = "dna")
#save alignment to file for use in markdown
saveRDS(file = "./output/alignment",alignment)

  
#add metadata
meta <- read_csv(file = "./ficus_metadata.csv")
meta
##change names in alignment ####
alignment <- read_rds("./output/alignment")

alignment@unmasked@ranges@NAMES <- 
  paste(alignment@unmasked@ranges@NAMES %>% str_split(pattern = " ") %>% map_chr(2),
        alignment@unmasked@ranges@NAMES %>% str_split(pattern = " ") %>% map_chr(3),
        sep = "_")

unique(alignment@unmasked@ranges@NAMES)


#write the alignment to a phyDat file
df_phydat <- as.phyDat(x = alignment)


#prepare alignment for Phangorn
write.phyDat(x = df_phydat,file = "./alignedficus.dna",format = "fasta")

read.phyDat("./alignedficus.dna",format = "fasta",type = "DNA")

phydat <- read.phyDat( "./alignedficus.dna",format = "fasta")


#build distance alignment
dm  <- dist.ml(phydat,model = bestAICmod);dm
# Build some trees from the alignment

?dist.ml()

#create some trees
treeUPGMA  <- upgma(dm)
treeNJ  <- NJ(dm);treeNJ

ape::phydataplot(bootstrap)
# test to see which tree is better
parsimony(treeUPGMA, phydat)

parsimony(treeNJ, phydat)

class(treeNJ)

ggtree(treeNJ,)

#optimum model
mt <- modelTest(phydat);print(mt)
#save model test as rds object
saveRDS(mt,file = "./output/modeltest_object")
mt <- readRDS("./output/modeltest_object")
simplemt <- mt%>%filter(str_length(Model)==3)
simplemt[which(simplemt$AIC<=min(simplemt$AIC)),1]
bestAICmod <- simplemt[which(simplemt$AIC<=min(simplemt$AIC)),1]


bestAICmod
#moving into maximum likelihood. 
fit <- pml(treeNJ, phydat)
fitHKY <- optim.pml(fit, model = bestAICmod, rearrangement = "ratchet",)
#save for markdown
saveRDS(fitHKY,file = "./output/fitHKY")
fitHKY <- readRDS("./output/fitHKY")
class(bootstrap)
fitHKY[6]
bootstrap <- bootstrap.pml(fitHKY, bs=100, optNni=TRUE, multicore=TRUE, control = pml.control(trace=0))
#save for markdown
saveRDS(bootstrap,file = "./output/bootstrap_object")
bootstrap <- readRDS(file = "./output/bootstrap_object")
#plotBS(fitHKY[tree],BStrees = bootstrap,p=10,type = "p","rect")
class(bootstrap)
?pml
bootstraptr <- plotBS(tree = treeNJ,BStrees = bootstrap,p = 100)
rr.castilla <- root(bootstraptr,node = 68)
plotTree(rr.castilla,roo)
plotTree(treeNJ,node.numbers=T,branch.lengths= "none")+coord_polar()
ggtree(bootstraptr,branch.length = "none")

ggtree(rr.castilla,mapping = treeNJ)+geom_()


p1 <- ggtree(rr.castilla,branch.length = "none",nodelabels=T,)
p1 %<+% meta + geom_tippoint(aes(color = Strangler))+ 
  geom_tiplab(aes(fill =Location),
              size = 2,
              offset = .5,
              color = "black", # color for label font
              geom = "label",  # labels not text
              label.padding = unit(0.15, "lines"), # amount of padding around the labels
              label.size = 0) +# size of label border
  theme_tree() # no keys

plotTree(treeNJ,node.numbers=T,)+coord_polar()
rr.73 <- root(treeNJ,node = 73)
plotTree(rr.73)



ggtree()
p <- ggtree(bootstraptr,branch.length = "none")
p %<+% meta + 
  geom_tiplab(aes(fill = factor(Strangler)),
              color = "black", # color for label font
              geom = "label",  # labels not text
              label.padding = unit(0.15, "lines"), # amount of padding around the labels
              label.size = 0) +# size of label border
  theme(legend.position = c(0.5,0.2), 
        legend.title = element_blank(), # no title
        legend.key = element_blank()) # no keys
fit <- pml(treeNJ,data = phydat);fit

msaplot(p=p, fasta="./alignedficus.dna", window=c(150, 175))+geom_tiplab(size = 10)+
  coord_cartesian()
ggsave("./output/msatree.2.png",units = "in",height = 35,width = 40)
#test different models

beepr::beep(sound = 5)

#

