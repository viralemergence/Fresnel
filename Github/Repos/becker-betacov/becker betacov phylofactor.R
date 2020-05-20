## phylofactor of betacoronavirus host status
## danbeck@iu.edu
## last updated 05/17/2020

## clean environment & plots
rm(list=ls()) 
graphics.off()

## packages
library(ape)
library(data.table)
library(ggtree)
library(plyr)
library(phylofactor)
library(tidyr)
library(plotrix)

## load master data
setwd("~/Desktop/virionette/03_interaction_data")
data=read.csv('virionette.csv',header=T)
data$tree=gsub(' ','_',data$host_species)

## betacovs only
data=data[which(data$virus_genus=='Betacoronavirus'),]
data=data.frame(data)

## betacov
data$betacov=1

## aggregate
data=data[!duplicated(data$host_species),]

## load mammal supertree
setwd("~/Desktop/virionette/04_predictors")
tree=readRDS('Full Supertree.rds')

## data
tdata=data.frame(tree=tree$tip.label)

## merge
data=merge(tdata,data,by='tree',all=T)
rm(tdata)

## fix betacov
data$betacov=replace_na(data$betacov,0)

## load bat supertree
btree=readRDS('bat-supertree_clean.rds')

## get bats
data$bats=ifelse(data$tree%in%btree$tip.label,'bats','other')

## trim
bb=data[c('tree','bats')]
setwd("~/Desktop")
write.csv(bb,'bats vs other.csv')

## clean
rm(btree,bb)

## load in cites
setwd("~/Desktop/virionette/04_predictors")
cites=read.csv('Citations.csv',header=T)
cites$X=NULL
cites$tree=gsub(' ','_',cites$name)
cites$name=NULL

## merge 
data=merge(data,cites,by='tree',all.x=T)
rm(cites)
table(is.na(data$cites))

## fix names
data$treenames=data$tree
data$tree=NULL

## trim
data=data[c('treenames','betacov','cites','bats')]

## remove humans
data=data[-which(data$treenames=='Homo_sapiens'),]
tree=drop.tip(tree,'Homo_sapiens')

## merge into phylogeny order
data=data[match(tree$tip.label,data$treenames),]

## merge with caper
library(caper)
cdata=comparative.data(phy=tree,data=data,names.col=treenames,vcv=T,na.omit=F,warn.dropped=T)

## add correct names
cdata$data$treenames=rownames(cdata$data)
cdata$data$Species=rownames(cdata$data)

## sqrt cites
cdata$data$scites=sqrt(cdata$data$cites)

## label
cdata$data$label=cdata$data$treenames
cdata$data$betafac=factor(cdata$data$betacov)

## subset to bats
bdata=cdata[which(cdata$data$bats=='bats'),]

## Holm rejection procedure
HolmProcedure <- function(pf,FWER=0.05){
  if (pf$models[[1]]$family$family%in%c('gaussian',"Gamma","quasipoisson")){
    pvals <- sapply(pf$models,FUN=function(fit) summary(fit)$coefficients['phyloS','Pr(>|t|)'])
  } else {
    pvals <- sapply(pf$models,FUN=function(fit) summary(fit)$coefficients['phyloS','Pr(>|z|)'])
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
  results=data.frame(matrix(ncol=5, nrow = hp))
  colnames(results)=c('factor','tips','node',"clade",'other')
  
  ## loop
  for(i in 1:hp){
    
    # save
    results[i,'factor']=i
    
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

## bat-only phylofactor, uncorrected
set.seed(1)
bat_pf=gpf(Data=bdata$data,tree=bdata$phy,
       frmla.phylo=betacov~phylo,
       family=binomial,algorithm='phylo',nfactors=3)

## results
bat_results=pfsum(bat_pf)

## split data from results
bat_data=bat_results$set
bat_results=bat_results$results

## refit to correct clades
set.seed(1)
bat_pf=gpf(Data=bdata$data,tree=bdata$phy,
           frmla.phylo=betacov~phylo,
           family=binomial,algorithm='phylo',nfactors=nrow(bat_results))

## bat-only phylofactor, corrected for cites
set.seed(1)
cbat_pf=gpf(Data=bdata$data,tree=bdata$phy,
           frmla.phylo=betacov~phylo,
           family=binomial,algorithm='phylo',nfactors=3,
           weights=sqrt(bdata$data$cites))

## results
cbat_results=pfsum(cbat_pf)

## split data from results
cbat_data=cbat_results$set
cbat_results=cbat_results$results

## refit to correct clades
set.seed(1)
cbat_pf=gpf(Data=bdata$data,tree=bdata$phy,
           frmla.phylo=betacov~phylo,
           family=binomial,algorithm='phylo',nfactors=nrow(cbat_results))

## make mammal besides bat
mdata=cdata[-(which(cdata$data$bats=='bats')),]

## all mammal phylofactor, uncorrected
set.seed(1)
mam_pf=gpf(Data=mdata$data,tree=mdata$phy,
           frmla.phylo=betacov~phylo,
           family=binomial,algorithm='phylo',nfactors=3)

## results
mam_results=pfsum(mam_pf)

## split data from results
mam_data=mam_results$set
mam_results=mam_results$results

## refit to correct clades
set.seed(1)
mam_pf=gpf(Data=mdata$data,tree=mdata$phy,
           frmla.phylo=betacov~phylo,
           family=binomial,algorithm='phylo',nfactors=nrow(mam_results))

## all mammal phylofactor, corrected for cites
set.seed(1)
cmam_pf=gpf(Data=mdata$data,tree=mdata$phy,
            frmla.phylo=betacov~phylo,
            family=binomial,algorithm='phylo',nfactors=3,
            weights=sqrt(mdata$data$cites))

## BeckerBatsUncorrected.csv
set=data.frame(host_species=bdata$data$treenames,
               preds=predict(bat_pf,type='response'))

## write
setwd("~/Desktop/becker-betacov")
write.csv(set,'BeckerBatsUncorrected.csv')

## BeckerBatsCitations.csv
set=data.frame(host_species=bdata$data$treenames,
               preds=ifelse(HolmProcedure(cbat_pf)==0,NA,predict(cbat_pf,type='response')))

## write
setwd("~/Desktop/becker-betacov")
write.csv(set,'BeckerBatsCitations.csv')

## BeckerMammalUncorrected.csv
set=data.frame(host_species=mdata$data$treenames,
               preds=predict(mam_pf,type='response'))

## write
setwd("~/Desktop/becker-betacov")
write.csv(set,'BeckerMammalUncorrected.csv')

## BeckerMammalCitations.csv
set=data.frame(host_species=mdata$data$treenames,
               preds=ifelse(HolmProcedure(cmam_pf)==0,NA,predict(cmam_pf,type='response')))

## write
setwd("~/Desktop/becker-betacov")
write.csv(set,'BeckerMammalCitations.csv')

## set x max
plus=5

## make bat base
bat_base=ggtree(bdata$phy,size=0.1,layout="fan")
bat_base=bat_base$data

## tips only
bat_base=bat_base[which(bat_base$isTip==T),]

## make data frame
bat_preds=data.frame(x=bat_base$x,
                 y=bat_base$y,
                 yend=bat_base$y,
                 xend=ifelse(bdata$data$betacov==0,unique(bat_base$x)[1],
                             unique(bat_base$x)[1]+plus),
                 betafac=factor(bdata$data$betacov))

## repeat for mammals
mam_base=ggtree(mdata$phy,size=0.1,layout="fan")
mam_base=mam_base$data
mam_base=mam_base[which(mam_base$isTip==T),]

## make data frame
mam_preds=data.frame(x=mam_base$x,
                     y=mam_base$y,
                     yend=mam_base$y,
                     xend=ifelse(mdata$data$betacov==0,unique(mam_base$x)[1],
                                 unique(mam_base$x)[1]+plus*2),
                     betafac=factor(mdata$data$betacov))

## make phylofactor figure
library(patchwork)

## specify lines
lwd=0.1

## bat raw
gg=ggtree(bdata$phy,
          size=0.1,
          layout='circular')

## add clades
for(i in 1:nrow(bat_results)){
  
  gg=gg+
    geom_hilight(node=bat_results$node[i],
                 alpha=0.5,
                 fill=ifelse(bat_results$clade<
                               bat_results$other,pcols[1],pcols[2])[i])+
    geom_cladelabel(node=bat_results$node[i],
                    label=bat_results$factor[i],
                    offset=plus*2,
                    offset.text=plus*2)
}

## visualize
p1=gg+
  
  ## title
  ggtitle('bats')+
  theme(plot.title = element_text(hjust = 0.5))+

  ## add predictions
  geom_segment(data=bat_preds,
               aes(x=x,y=y,xend=xend,yend=yend,
                   colour=betafac),size=0.25)+
  scale_colour_manual(values=c('white','black'))+
  guides(colour=F)

## bat citations
gg=ggtree(bdata$phy,
          size=0.1,
          layout='circular')

## add clades
for(i in 1:nrow(cbat_results)){
  
  gg=gg+
    geom_hilight(node=cbat_results$node[i],
                 alpha=0.5,
                 fill=ifelse(cbat_results$clade<
                               cbat_results$other,pcols[1],pcols[2])[i])+
    geom_cladelabel(node=cbat_results$node[i],
                    label=cbat_results$factor[i],
                    offset=plus*2,
                    offset.text=plus*2)
}

## visualize
p2=gg+
  
  ## title
  ggtitle('bats (with citations)')+
  theme(plot.title = element_text(hjust = 0.5))+
  
  ## add predictions
  geom_segment(data=bat_preds,
               aes(x=x,y=y,xend=xend,yend=yend,
                   colour=betafac),size=0.25)+
  scale_colour_manual(values=c('white','black'))+
  guides(colour=F)

## mammal raw
gg=ggtree(mdata$phy,
          size=0.1,
          layout='circular')

## add clades
for(i in 1:nrow(mam_results)){
  
  gg=gg+
    geom_hilight(node=mam_results$node[i],
                 alpha=0.5,
                 fill=ifelse(mam_results$clade<
                               mam_results$other,pcols[1],pcols[2])[i])+
    geom_cladelabel(node=mam_results$node[i],
                    label=mam_results$factor[i],
                    offset=plus*2,
                    offset.text=plus*2)
}

## visualize
p3=gg+
  
  ## title
  ggtitle('mammals')+
  theme(plot.title = element_text(hjust = 0.5))+
  
  ## add predictions
  geom_segment(data=mam_preds,
               aes(x=x,y=y,xend=xend,yend=yend,
                   colour=betafac),size=0.25)+
  scale_colour_manual(values=c('white','black'))+
  guides(colour=F)

## mammal citations
gg=ggtree(mdata$phy,
          size=0.1,
          layout='circular')
p4=gg+
  
  ## title
  ggtitle('mammals (with citations)')+
  theme(plot.title = element_text(hjust = 0.5))+
  
  ## add predictions
  geom_segment(data=mam_preds,
               aes(x=x,y=y,xend=xend,yend=yend,
                   colour=betafac),size=0.25)+
  scale_colour_manual(values=c('white','black'))+
  guides(colour=F)

## combine
setwd("~/Desktop/becker-betacov")
png("phylo betacov.png",width=7,height=7,units="in",res=300)
p1+p2+p3+p4
dev.off()