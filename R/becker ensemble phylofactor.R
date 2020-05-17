## phylofactor betacov ensemble
## danbeck@iu.edu
# 05162020

## clean environment & plots
rm(list=ls()) 
graphics.off()

## packages
library(ape)
library(phylofactor)
library(tidyverse)
library(data.table)
library(ggtree)
library(plyr)
library(caper)

## load ensemble data
setwd("~/Desktop/Fresnel/Cleaned Files")
batin=read.csv('BatModels_IS.csv',header=T)
batout=read.csv('BatModels_OS.csv',header=T)

## mammals too
mamin=read.csv('NonBatModels_IS.csv',header=T)
mamout=read.csv('NonBatModels_OS.csv',header=T)

## fix mamout
mamout=mamout[!is.na(mamout$PropRank),]

## load mammal supertree
setwd("~/Desktop/virionette/04_predictors")
tree=readRDS('Full Supertree.rds')

## extract node 6580 (Chiroptera)
btree=extract.clade(tree,6580)
length(btree$tip.label)

## make tree names
batin$treenames=gsub(' ','_',batin$Sp)
batout$treenames=gsub(' ','_',batout$Sp)
mamin$treenames=gsub(' ','_',mamin$Sp)
mamout$treenames=gsub(' ','_',mamout$Sp)

## load in taxonomy
setwd("~/Desktop/becker-betacov")
taxonomy=read.csv('mammal taxonomy.csv',header=T)
taxonomy$X=NULL
taxonomy$Sp=NULL

## trim to genus
taxonomy=taxonomy[!duplicated(taxonomy$hGenus),]

## get genus
batin$hGenus=sapply(strsplit(batin$treenames,'_'),function(x) x[1])
batout$hGenus=sapply(strsplit(batout$treenames,'_'),function(x) x[1])
mamin$hGenus=sapply(strsplit(mamin$treenames,'_'),function(x) x[1])
mamout$hGenus=sapply(strsplit(mamout$treenames,'_'),function(x) x[1])

## merge
batin=merge(batin,taxonomy,by='hGenus',all.x=T)
batout=merge(batout,taxonomy,by='hGenus',all.x=T)

## fix batout
batout$hFamily=as.character(batout$hFamily)
batout$hFamily2=revalue(batout$hGenus,
                      c('Aproteles'='Pteropodidae',
                        'Paracoelops'='Hipposideridae'))
batout$hFamily=ifelse(is.na(batout$hFamily),batout$hFamily2,batout$hFamily)
batout$hFamily2=NULL
batout$hOrder='Chiroptera'

## mammal missing
test=merge(mamin,taxonomy,by='hGenus',all.x=T); table(is.na(test$hFamily))
set=test[is.na(test$hFamily),]
set=set[!duplicated(set$hGenus),]
set=set[c('hGenus','hFamily','hOrder')]

## revalue
set$hFamily2=revalue(set$hGenus,
                     c('Balaenoptera'='Balaenopteridae',
                       'Caluromys'='Didelphidae',
                       'Camelus'='Camelidae',
                       'Chironectes'='Didelphidae',
                       'Delphinapterus'='Monodontidae',
                       'Delphinus'='Delphinidae',
                       'Didelphis'='Didelphidae',
                       'Dorcopsis'='Macropodidae',
                       'Eschrichtius'='Eschrichtiidae',
                       'Galerella'='Herpestidae',
                       'Globicephala'='Delphinidae',
                       'Isoodon'='Peramelidae',
                       'Lama'='Camelidae',
                       'Macropus'='Macropodidae',
                       'Marmosa'='Didelphidae',
                       'Metachirus'='Didelphidae',
                       'Orcinus'='Delphinidae',
                       'Perameles'='Peramelidae',
                       'Philander'='Didelphidae',
                       'Phocoena'='Phocoenidae',
                       'Physeter'='Physeteridae',
                       'Stenella'='Delphinidae',
                       'Taurotragus'='Bovidae',
                       'Trichosurus'='Phalangeridae',
                       'Tursiops'='Tursiops'))

## make order
set$hOrder2=revalue(set$hFamily2,
                    c('Balaenopteridae'='Artiodactyla',
                      'Didelphidae'='Didelphimorphia',
                      'Camelidae'='Artiodactyla',
                      'Monodontidae'='Artiodactyla',
                      'Delphinidae'='Artiodactyla',
                      'Macropodidae'='Diprotodontia',
                      'Eschrichtiidae'='Artiodactyla',
                      'Herpestidae'='Carnivora',
                      'Peramelidae'='Peramelemorphia',
                      'Phocoenidae'='Artiodactyla',
                      'Physeteridae'='Artiodactyla',
                      'Bovidae'='Artiodactyla',
                      'Phalangeridae'='Diprotodontia',
                      'Tursiops'='Artiodactyla'))

## trim
set$hFamily=NULL
set$hOrder=NULL

## merge into taxonomy
test=merge(taxonomy,set,by='hGenus',all=T)

## fix
test$hFamily=as.character(test$hFamily)
test$hOrder=as.character(test$hOrder)
test$hFamily=ifelse(is.na(test$hFamily),test$hFamily2,test$hFamily)
test$hOrder=ifelse(is.na(test$hOrder),test$hOrder2,test$hOrder)
test$hFamily2=NULL
test$hOrder2=NULL

## new taxonomy
taxonomy=test
rm(test,set)

## merge taxa into mammals
mamin=merge(mamin,taxonomy,by='hGenus',all.x=T)
mamout=merge(mamout,taxonomy,by='hGenus',all.x=T)

## make taxonomy within each file
batin$taxonomy=with(batin,paste(hOrder,hFamily,hGenus,treenames,sep='; '))
batout$taxonomy=with(batout,paste(hOrder,hFamily,hGenus,treenames,sep='; '))
mamin$taxonomy=with(mamin,paste(hOrder,hFamily,hGenus,treenames,sep='; '))
mamout$taxonomy=with(mamout,paste(hOrder,hFamily,hGenus,treenames,sep='; '))

## trim each to tree
btree_in=keep.tip(btree,batin$treenames)
btree_out=keep.tip(btree,btree$tip.label[btree$tip.label%in%batout$treenames])
tree_in=keep.tip(tree,mamin$treenames)
tree_out=keep.tip(tree,mamout$treenames)

## merge into phylogeny order
batin=batin[match(btree_in$tip.label,batin$treenames),]
batout=batout[match(btree_out$tip.label,batout$treenames),]
mamin=mamin[match(tree_in$tip.label,mamin$treenames),]
mamout=mamout[match(tree_out$tip.label,mamout$treenames),]

## caper objects
bdata_in=comparative.data(phy=btree_in,data=batin,names.col=treenames,vcv=T,na.omit=F,warn.dropped=T)
bdata_out=comparative.data(phy=btree_out,data=batout,names.col=treenames,vcv=T,na.omit=F,warn.dropped=T)
data_in=comparative.data(phy=tree_in,data=mamin,names.col=treenames,vcv=T,na.omit=F,warn.dropped=T)
data_out=comparative.data(phy=tree_out,data=mamout,names.col=treenames,vcv=T,na.omit=F,warn.dropped=T)

## clean up
rm(batin,batout,btree,mamin,mamout,tree,tree_in,tree_out,btree_in,btree_out)

## rename
bdata_in$data$treenames=rownames(bdata_in$data)
bdata_out$data$treenames=rownames(bdata_out$data)
data_in$data$treenames=rownames(data_in$data)
data_out$data$treenames=rownames(data_out$data)

## add Species
bdata_in$data$Species=rownames(bdata_in$data)
bdata_out$data$Species=rownames(bdata_out$data)
data_in$data$Species=rownames(data_in$data)
data_out$data$Species=rownames(data_out$data)

## Holm rejection procedure
HolmProcedure <- function(pf,FWER=0.05){
  
  ## get split variable
  cs=names(coef(pf$models[[1]]))[-1]
  split=ifelse(length(cs)>1,cs[3],cs[1])
  
  ## obtain p values
  if (pf$models[[1]]$family$family%in%c('gaussian',"Gamma","quasipoisson")){
    pvals <- sapply(pf$models,FUN=function(fit) summary(fit)$coefficients[split,'Pr(>|t|)'])
  } else {
    pvals <- sapply(pf$models,FUN=function(fit) summary(fit)$coefficients[split,'Pr(>|z|)'])
  }
  D <- length(pf$tree$tip.label)
  
  ## this is the line for Holm's sequentially rejective cutoff
  keepers <- pvals<=(FWER/(2*D-3 - 2*(0:(pf$nfactors-1))))
  
  
  if (!all(keepers)){
    nfactors <- min(which(!keepers))-1
  } else {
    nfactors <- pf$nfactors
  }
  return(nfactors)
}

## get species in a clade
cladeget=function(pf,factor){
  spp=pf$tree$tip.label[pf$groups[[factor]][[1]]]
  return(spp)
}

## summarize pf object 
pfsum=function(pf){
  
  ## get formula
  chars=as.character(pf$frmla.phylo)[-1]
  
  ## response
  resp=chars[1]
  
  ## holm
  hp=HolmProcedure(pf)
  
  ## save model
  model=chars[2]
  
  ## set key
  setkey(pf$Data,'Species')
  
  ## make data
  dat=data.frame(pf$Data)
  
  ## make clade columns in data
  for(i in 1:hp){
    
    dat[,paste0(resp,'_pf',i)]=ifelse(dat$treename%in%cladeget(pf,i),'factor','other')
    
  }
  
  ## make data frame to store taxa name, response, mean, and other
  results=data.frame(matrix(ncol=6, nrow = hp))
  colnames(results)=c('factor','taxa','tips','node',"clade",'other')
  
  ## set taxonomy
  taxonomy=dat[c('Species','taxonomy')]
  taxonomy$taxonomy=as.character(taxonomy$taxonomy)
  
  ## loop
  for(i in 1:hp){
    
    ## get taxa
    tx=pf.taxa(pf,taxonomy,factor=i)$group1

    ## get tail
    tx=sapply(strsplit(tx,'; '),function(x) tail(x,1))

    ## combine
    tx=paste(tx,collapse=', ')

    # save
    results[i,'factor']=i
    results[i,'taxa']=tx
    
    ## get node
    tips=cladeget(pf,i)
    node=ggtree::MRCA(pf$tree,tips)
    results[i,'tips']=length(tips)
    results[i,'node']=ifelse(is.null(node) & length(tips)==1,'species',
                             ifelse(is.null(node) & length(tips)!=1,NA,node))
    
    ## get means
    ms=(tapply(dat[,resp],dat[,paste0(resp,'_pf',i)],mean))
      
    ## add in
    results[i,'clade']=ms['factor']
    results[i,'other']=ms['other']
    
  }
  
  ## return
  return(list(set=dat,results=results))
}

## fix palette
AlberPalettes <- c("YlGnBu","Reds","BuPu", "PiYG")
AlberColours <- sapply(AlberPalettes, function(a) RColorBrewer::brewer.pal(5, a)[4])
afun=function(x){
  a=AlberColours[1:x]
  return(a)
}

## make low and high
pcols=afun(2)

## function to add segments
library(plotrix)
segfun=function(set,len){
  
  ## set x max
  plus=len
  
  ## make bat base
  base=ggtree(set$phy,size=0.1,layout="fan")
  base=base$data
  
  ## tips only
  base=base[which(base$isTip==T),]
  
  ## make data frame
  preds=data.frame(x=base$x,
                   y=base$y,
                   yend=base$y,
                   xend=rescale(set$data$PropRank,c(unique(base$x)[1]+plus,unique(base$x)[1])),
                   Betacov=factor(set$data$Betacov))
  return(preds)
}

## bat phylofactor, in sample
set.seed(1)
batin_pf=gpf(Data=bdata_in$data,tree=bdata_in$phy,
           frmla.phylo=PropRank~phylo,
           family=gaussian,algorithm='phylo',nfactors=2)

## results
batin_results=pfsum(batin_pf)

## split data from results
batin_data=batin_results$set
batin_results=batin_results$results

## revalue
batin_results$taxa=revalue(batin_results$taxa,
                           c('Natalidae, Mystacinidae, Mormoopidae, Phyllostomidae, Molossidae, Vespertilionidae'='Yangochiroptera'))

## bat phylofactor, out of sample
set.seed(1)
batout_pf=gpf(Data=bdata_out$data,tree=bdata_out$phy,
             frmla.phylo=PropRank~phylo,
             family=gaussian,algorithm='phylo',nfactors=8)

## results
batout_results=pfsum(batout_pf)

## split data from results
batout_data=batout_results$set
batout_results=batout_results$results

## mammal phylofactor, in sample
set.seed(1)
mamin_pf=gpf(Data=data_in$data,tree=data_in$phy,
             frmla.phylo=PropRank~phylo,
             family=gaussian,algorithm='phylo',nfactors=5)

## results
mamin_results=pfsum(mamin_pf)

## split data from results
mamin_data=mamin_results$set
mamin_results=mamin_results$results

## mammal phylofactor, out of sample
set.seed(1)
mamout_pf=gpf(Data=data_out$data,tree=data_out$phy,
             frmla.phylo=PropRank~phylo,
             family=gaussian,algorithm='phylo',nfactors=15)

## results
mamout_results=pfsum(mamout_pf)

## split data from results
mamout_data=mamout_results$set
mamout_results=mamout_results$results

## visualize
gg=ggtree(bdata_in$phy,
          size=0.15,
          layout='circular')

## add clades
for(i in 1:nrow(batin_results)){
  
  gg=gg+
    geom_hilight(node=batin_results$node[i],
                 alpha=0.5,
                 fill=ifelse(batin_results$clade<
                               batin_results$other,pcols[2],pcols[1])[i])+
    geom_cladelabel(node=batin_results$node[i],
                    label=batin_results$factor[i],
                    offset=25,
                    offset.text=20)
}

## visualize
p1=gg+
  
  ## title
  #ggtitle('bat rank, in-sample')+
  #theme(plot.title = element_text(hjust = 0.5))+
  
  ## add predictions
  geom_segment(data=segfun(bdata_in,25),
               aes(x=x,y=y,xend=xend,yend=yend,colour=Betacov),size=0.5)+
  scale_colour_manual(values=c('grey70','black'))#+

  # ## add clade
  # geom_cladelabel(node=batin_results$node[1],
  #                 label=batin_results$factor[1],
  #                 offset=25,
  #                 offset.text=20)

## visualize
gg=ggtree(bdata_out$phy,
          size=0.1,
          layout='circular')

## add clades
for(i in 1:nrow(batout_results)){
  
  gg=gg+
    geom_hilight(node=batout_results$node[i],
                 alpha=0.5,
                 fill=ifelse(batout_results$clade<
                               batout_results$other,pcols[2],pcols[1])[i])+
    geom_cladelabel(node=batout_results$node[i],
                    label=batout_results$factor[i],
                    offset=25,
                    offset.text=20)
}

## finish
p2=gg+
  
  ## title
  #ggtitle('bat rank, out-of-sample')+
  #theme(plot.title = element_text(hjust = 0.5))+
  
  ## add predictions
  geom_segment(data=segfun(bdata_out,25),
               aes(x=x,y=y,xend=xend,yend=yend,colour=Betacov),size=0.15)+
  scale_colour_manual(values=c('grey70','black'))#+
  
  # ## add clades
  # geom_cladelabel(node=batout_results$node[1],
  #                 label='Noctilionoidea',
  #                 offset=25,
  #                 offset.text=20,
  #                 hjust=0.5)+
  # geom_cladelabel(node=batout_results$node[2],
  #                 label='Emballonuridae',
  #                 offset=25,
  #                 offset.text=20,
  #                 hjust=0)+
  # geom_cladelabel(node=batout_results$node[5],
  #                 label='Rousettinae\nEpomophorinae\nCynopterinae',
  #                 offset=25,
  #                 offset.text=10,
  #                 hjust=0,
  #                 extend=10)+
  # geom_cladelabel(node=batout_results$node[7],
  #                 label='Rhinolophidae',
  #                 offset=25,
  #                 offset.text=20,
  #                 hjust=0.5)

## visualize
gg=ggtree(data_in$phy,
          size=0.2,
          layout='circular')

## add clades
for(i in 1:nrow(mamin_results)){
  
  gg=gg+
    geom_hilight(node=mamin_results$node[i],
                 alpha=0.5,
                 fill=ifelse(mamin_results$clade<
                               mamin_results$other,pcols[2],pcols[1])[i])+
    geom_cladelabel(node=mamin_results$node[i],
                    label=mamin_results$factor[i],
                    offset=55,
                    offset.text=50)
}

## finish
p3=gg+
  
  ## title
  #ggtitle('mammal rank, in-sample')+
  #theme(plot.title = element_text(hjust = 0.5))+
  
  ## add predictions
  geom_segment(data=segfun(data_in,55),
               aes(x=x,y=y,xend=xend,yend=yend,colour=Betacov),size=0.25)+
  scale_colour_manual(values=c('grey70','black'))

## visualize
gg=ggtree(data_out$phy,
          size=0.05,
          layout='circular')

## add clades
for(i in 1:nrow(mamout_results)){
  
  gg=gg+
    geom_hilight(node=mamout_results$node[i],
                 alpha=0.5,
                 fill=ifelse(mamout_results$clade<
                               mamout_results$other,pcols[2],pcols[1])[i])+
    geom_cladelabel(node=mamout_results$node[i],
                    label=mamout_results$factor[i],
                    offset=50,
                    offset.text=45)
}

## finish
p4=gg+
  
  ## title
  #ggtitle('mammal rank, out-of-sample')+
  #theme(plot.title = element_text(hjust = 0.5))+
  
  ## add predictions
  geom_segment(data=segfun(data_out,50),
               aes(x=x,y=y,xend=xend,yend=yend,colour=Betacov),size=0.05)+
  scale_colour_manual(values=c('grey70','black'))

## write
library(patchwork)
library(ggpubr)
setwd("~/Desktop/Fresnel/Figures")
png("Phylofactor_pred ensemble.png",width=7,height=6.5,units="in",res=600)
ggpubr::ggarrange(p1,p2,p3,p4,ncol=2,nrow=2,
                  labels=c('bat rank, in-sample',
                           'bat rank, out-of-sample',
                           'mammal rank, in-sample',
                           'mammal rank, out-of-sample'),
                  hjust=c(-0.85,-0.65,-0.5,-0.4),
                  vjust=c(1.75,1.75,1,1),
                  font.label=list(size=11,face='plain'))
#p1+p2+p3+p4+plot_layout(ncol=2)
dev.off()

## assign scale
batin_results$scale='bats'
batout_results$scale='bats'
mamin_results$scale='mammals'
mamout_results$scale='mammals'

## assign in/out
batin_results$sample='in'
batout_results$sample='out'
mamin_results$sample='in'
mamout_results$sample='out'

## combine
results=rbind.data.frame(batin_results,
                         batout_results,
                         mamin_results,
                         mamout_results)
results=results[c('scale','sample','factor','taxa','tips','clade','other')]

## export
setwd("~/Desktop/Fresnel/Output Files")
write.csv(results,'ensemble phylofactor results.csv')