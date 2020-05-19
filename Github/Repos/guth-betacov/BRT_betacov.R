# rm(list = ls())

set.seed(05082020)

library(gbm)
library(dplyr)
#library(plyr)
library(dismo)
#library(raster)
library(verification)
library(ggplot2)
library(fastDummies)
library(tidyverse)
library(forcats)
library(PresenceAbsence)
library(reshape2)

quietly <- function(x) {
  sink(tempfile())
  on.exit(sink())
  invisible(force(x))
}  # THANKS HADLEY

read.csv(paste0(here::here(), '/Github/Repos/virionette/03_interaction_data/virionette.csv')) %>% 
  filter(host_order == 'Chiroptera') -> 
  
  batcov

read.csv(paste0(here::here(), '/Github/Repos/virionette/04_predictors/Han-BatTraits.csv')) -> 
  
  traits

# Add outcome variables 

batcov %>% mutate(betacov = as.numeric(virus_genus == 'Betacoronavirus')) -> batcov

batcov %>% dplyr::select(host_species, betacov) %>% unique -> batcov
batcov %>% group_by(host_species) %>% dplyr::summarise(betacov = max(betacov)) -> batcov

# Create binomial names in the trait data

traits %>% mutate(host_species = paste(MSW05_Genus, MSW05_Species)) %>% 
  mutate() -> traits

# Add traits and associations

right_join(batcov, traits) %>% 
  mutate(betacov = replace_na(betacov, 0)) -> batdf 

# Turn categorical variable into columns

batdf %>% dummy_cols('ForStrat.Value') %>% 
  dplyr::select(-ForStrat.Value) -> batdf

# Drop any variable with > 50% NA's
varnums <- c(7:73)
suf_val <- 6 + unname(which(c(colSums(is.na(batdf[,varnums]))/nrow(batdf)) < 0.5))

# Drop variables with insufficient variation
# mode function
mode.prop <- function(x) {
  ux <- unique(x[is.na(x)==FALSE])
  tab <- tabulate(match(na.omit(x), ux))
  max(tab)/length(x[is.na(x)==FALSE])
}
# remove if 97% or more of values == the mod
suf_var <- 7 + unname(which(c(apply(batdf[,varnums], 2, mode.prop)) < 0.97))

# only include variables with BOTH sufficient values and variation
var.names <- intersect(suf_val, suf_var)

batdf <- as.data.frame(batdf)

####### BRT W/O BOOTSTRAPPING: tuning parameters and variable set #######
# gbmGrid <- expand.grid(tree.complexity = c(2, 3, 4),
#                       learning.rate = c(0.01, 0.005, 0.003, 0.001))
#
# grid_results <- cbind(gbmGrid, matrix(NA, nrow = length(gbmGrid$tree.complexity), ncol = 5))
# colnames(grid_results) <- c("tree.complexity", "learning.rate","n.trees", "cv.dev", "train.AUC", "cv.AUC", "comp_time")
#
# for(i in 1:nrow(gbmGrid)){
#   gbm.mod <- gbm.step(data = batdf,
#                       gbm.x = var.names, # these are the predictors, can also be column #s
#                       gbm.y = "betacov", # response
#                       family = "bernoulli",
#                       n.trees = 50,
#                       tree.complexity = gbmGrid$tree.complexity[i],
#                       learning.rate = gbmGrid$learning.rate[i],
#                       bag.fraction = 0.75, # controls for stochasticity
#                       max.trees = 100000)
#
#   grid_results[i, "n.trees"] <- gbm.mod$n.trees
#   grid_results[i, "cv.dev"]  <- gbm.mod$cv.statistics$deviance.mean #holdout deviance
#   grid_results[i, "train.AUC"] <- gbm.mod$self.statistics$discrimination
#   grid_results[i, "cv.AUC"] <- gbm.mod$cv.statistics$discrimination.mean
#   grid_results[i, "comp_time"] <- gbm.mod$gbm.call$elapsed.time.minutes
#
#   #status update
#   print(paste('param combo', i, '/', nrow(gbmGrid)))
#
# }
#
# #find parameter combo that gives you the lowest deviance with trees ~1500-5000: plot n. trees vs. dev
# plot(grid_results$n.trees, grid_results$cv.dev) #tc = 2, lr = 0.005: n.trees = 2000, cv.dev = 0.3785, comp_time = 0.71

#reduce variable set--based on model with tuned parameters
gbm.mod <- gbm.step(data = batdf,
                    gbm.x = var.names, # these are the predictors, can also be column #s
                    gbm.y = "betacov", # response
                    family = "bernoulli",
                    n.trees = 50,
                    tree.complexity = 2,
                    learning.rate = 0.005,
                    bag.fraction = 0.75, # controls for stochasticity
                    max.trees = 100000)

#function that compares prediction performance of variable drops
gbm.simp <- gbm.simplify(gbm.mod, n.drops = 20)
n.simp <- min(which(gbm.simp$deviance.summary$mean == min(gbm.simp$deviance.summary$mean)))

#refit model with the optimal reduced variable set
gbm.reduced <- gbm.step(data = batdf,
                        gbm.x = gbm.simp$pred.list[[n.simp]], # these are the predictors, can also be column #s
                        gbm.y = "betacov", # response
                        family = "bernoulli",
                        tree.complexity = 2,
                        learning.rate = 0.005,
                        bag.fraction = 0.75, # controls for stochasticity
                        max.trees = 100000)

####### BOOTSTRAPPED BRT FUNCTION #######
make.map.data <- function(data, n.boots, simp.list){
  
  #### make empty vectors for outputs across bootstrap samples:
  # 1) AUC, 2) mean deviance, 3) host predictions, 4) relative variable importance coefficients
  auc.results <- vector()
  dev.results <- vector()
  preds <- matrix(NA, nrow = length(batdf$host_species), ncol = n.boots)
  var.imp <- matrix(NA, nrow = length(simp.list), ncol = n.boots)
  
  #### subset the data to have one df for presences (betacov = 1) and one for absences (background, bg, pseudoabsences)
  pres.dat <- batdf[batdf$betacov==1,]
  num.pres <- dim(pres.dat)[1]
  bg.dat <- batdf[batdf$betacov ==0,]
  
  #### for each bootstrap run, we'll subset the data to a different training and test set
  for(i in 1:n.boots){
    
    ## generate a TRAINING dataset that has equal number of presences and pseudoabs
    
    # randomly pick presences (with replacement) to match total number of unique presences in the data (num.pres = 76)
    boot.train.pres <- sample(num.pres, replace = T) #randomly samples 76 indices btw 1-76, can pick same index twice
    train.pres.data <- pres.dat[boot.train.pres,] #subsets with random set of indices
    # then randomly pick pseudabs (again with replacement) to equal the number of presence data points
    boot.train.abs <- sample(dim(bg.dat)[1], replace = T, size = num.pres) #randomly samples 76 indices btw 1-1040, can pick same index twice
    train.abs.data <- bg.dat[boot.train.abs,] #subsets with random set of indices
    # combine pres and abs data
    boot.dat <- plyr::rbind.fill(train.pres.data, train.abs.data)
    
    ## now run gbm.step on bootstrapped dataset - this is the meat of the classification tree
    
    # gbm.step() will give optimal number of boosting trees (for pred step) using k-fold cross validation (cv)
    # glm.fit warning messages due to small sample size--can ignore
    gbm.reduced <- quietly(gbm.step(data = boot.dat,
                                    gbm.x = simp.list, # these are the predictors, can also be column #s
                                    gbm.y = "betacov", # response
                                    family = "bernoulli",
                                    tree.complexity = 2,
                                    learning.rate = 0.005,
                                    bag.fraction = 0.75, # controls for stochasticity
                                    max.trees = 100000))
    
    ## TEST set: out-of-bag = presences and absences data pointsnot included in the training set
    
    # Get out of bag presences = presence data points not included in the training set (a few remaining from the original 76)
    boot.test.pres <- (1:nrow(pres.dat))[-boot.train.pres]
    test.pres.data <- pres.dat[boot.test.pres,]
    # Get out of bag absences = psuedoabs data points not included in the training set (vast majority of the original 1040 are still remaining)
    boot.test.abs <- (1:nrow(bg.dat))[-boot.train.abs]
    test.abs.data <- bg.dat[boot.test.abs,]
    # Form the test dataset by binding oob presences and absences
    test.dat = plyr::rbind.fill(test.pres.data, test.abs.data)
    
    ## Predict and assess model
    test.dat$preds <- quietly(predict.gbm(gbm.reduced,
                                          test.dat,
                                          n.trees = gbm.reduced$gbm.call$best.trees,
                                          type = "response"))
    
    test.dat <- test.dat[ ,c("host_species", "betacov", "preds")]
    merged <- merge(batdf, test.dat, by = "host_species", all.x=TRUE)
    merged_order <- merged[order(merged$host_species), ]
    preds[ ,i] <- merged_order$preds
    # assess mean deviance and area under curve
    dev.gbm.red <- calc.deviance(obs = test.dat$betacov, pred = test.dat$preds, calc.mean = T)
    dev.results[i] <- dev.gbm.red
    #library(verification)
    roc <- roc.area(test.dat$betacov, test.dat$preds)
    auc <- roc$A
    
    auc.results[i] <- auc
    
    ## Variable relative influence coefficients
    summ <- data.frame(summary(gbm.reduced)) # coefficients for the 38 variables
    summ_order <- summ[order(summ$var), ]
    var.imp[ ,i] <- summ_order$rel.inf
    
    #status update
    print(paste('boot', i, '/', n.boots))
    
  }
  
  #format names of prediction and relative influence matrices
  row.names(preds) <- merged_order$host_species
  row.names(var.imp) <- summ_order$var
  
  results.list <- list("AUC" = auc.results,
                       "Deviance" = dev.results,
                       "Prediction" = preds,
                       "Variable" = var.imp)
  return(results.list)
}

## run function ##
simp.list <- gbm.simp$pred.list[[n.simp]]
# simp.list <- c(10, 11, 37, 38, 39, 40, 41, 42, 43, 45, 46, 47, 48, 49, 50, 51, 53, 59, 66)

n.boots <- 1000
gbm.boot <- make.map.data(data = batdf, n.boots = n.boots, simp.list = simp.list)

## Plot variable relative influence coefficients
var_dat <- as.data.frame(gbm.boot$Variable)
#calculate medians across bootstrap samples
var_dat$med <- apply(var_dat, 1, median, na.rm = FALSE)
#give variable names
var_dat$coef <- row.names(var_dat)
var_dat$coef <- as.factor(var_dat$coef)

#put in descending order
var_ord <- var_dat[order(-var_dat$med), ]
# ord.names <- var_ord$coef
var_top <- var_ord[1:10, ]
#drop median column and convert bootstrap samples to long format for boxplot
var_top <- subset(var_top, select = c(-med))
var_long <- reshape2::melt(var_top, id.vars = "coef", variable.name = "bootstrap", value.name = "value")


## Build boxplot
p <- ggplot(var_long, aes(x=fct_reorder(coef, value, .desc = FALSE), y=value)) +
  geom_boxplot()  +
  labs(x = 'Predictor', y = 'Importance') + coord_flip()

print(p)

## host predictions
pred_dat <- as.data.frame(gbm.boot$Prediction)
pred_dat$pred_med <- apply(pred_dat, 1, median, na.rm = TRUE)

# Top rankings
pred_dat$host_species <- rownames(pred_dat)
pred_med_dat <- pred_dat[ ,c("host_species", "pred_med")]
pred_merge <- merge(batdf, pred_med_dat, by = "host_species", all.x = TRUE)

# A density plot in the style of Becker
pred_merge %>%
  ggplot(aes(pred_merge$pred_med,
             fill = factor(betacov),
             colour = factor(betacov))) +
  geom_density(alpha = 0.1)

# pred_merge %>%
#   as_tibble() %>%
#   filter(!(betacov == 1)) %>%
#   dplyr::select(host_species, pred_med) %>%
#   arrange(-pred_med) %>% View()

pred_merge %>%
  as_tibble() %>%
  dplyr::select(host_species, betacov, pred_med) %>%
  data.frame() -> training

write.csv(pred_merge, "GuthUncorrected.csv")

# thresh <- optimal.thresholds(data.frame(training),
#                              threshold = 10001,
#                              opt.methods = 10,
#                              req.sens = 0.9,
#                              na.rm = TRUE)[1,2]
#
# # How many new bats are above the threshold?
# pred_merge %>%
#   as_tibble() %>%
#   filter(!(betacov == 1)) %>%
#   dplyr::select(host_species, pred_med) %>%
#   arrange(-pred_med) %>%
#   filter(pred_med > thresh) -> not.df
# nrow(not.df)
#
# # How's the AUC look
# auc.roc.plot(data.frame(training))

# # File for Greg
# pred_merge %>% dplyr::select(host_species,
#                  betacov,
#                  sarbecov,
#                  pred_med) %>% mutate(pred.bin = (pred_med > thresh),
#                                    rank = rank(pred_med)) %>%
#   mutate(rank = (max(rank) - rank + 1)) %>%
#   dplyr::rename(pred = pred_med) %>% as_tibble() -> bat.report
#
# write.csv(bat.report, 'batcov-brt-2.csv')

