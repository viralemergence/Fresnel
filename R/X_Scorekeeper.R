
library(ggrepel)

BatWeb %<>% as_tibble()

#BatWeb$Network.1[which(!is.na(BatWeb$Network.2) & is.na(BatWeb$Network.1))] <- 'Unlikely'

BatWeb %>% filter(`New data`=='New data') -> BatNew

scores <- tibble(names = c('Ensemble','Trait.1','Trait.2','Trait.3','Hybrid',
                           'Network.1','Network.2','Network.3','Network.4'))

scores$pred.pos <- 0
scores$correct <- 0

predicted <- c('Suspected', 'True +')

for (i in 1:nrow(scores)) {
  t <- table(BatWeb[scores$names[i]])
  scores$pred.pos[i] <- (sum(t[names(t)[names(t) %in% predicted]]))/sum(t)
  
  t <- table(BatNew[scores$names[i]])
  scores$correct[i] <- (sum(t[names(t)[names(t) %in% predicted]]))/sum(t)
}

scores$category <- c('Ensemble','Trait','Trait','Trait','Hybrid','Network','Network','Network','Network')

scores$category <- as.factor(scores$category)

library(awtools)

scores %>% 
  #filter(category == 'Ensemble') %>%
  ggplot(aes(x = pred.pos*100, y = correct*100, fill = category, label = factor(names))) + 
  geom_point(pch = 16, size = 2) + 
  xlab("Predicted positivity rate (%)") + 
  ylab("New hosts correctly identified (%)") +
  geom_abline(slope = 1, intercept = 0, col = 'grey70') + 
  theme_bw(base_size = 15) + 
  xlim(0,100) + ylim(0,100) +
  geom_label_repel(box.padding = unit(0.35, "lines"),
                   point.padding = unit(0.35, "lines"),
                   segment.color = 'grey50',
                   fontface = 'bold',
                   #color = 'white',
                   show.legend = FALSE) +
  theme(plot.margin = unit(c(1,1,1,1),"cm"),
        axis.title.x = element_text(vjust=-5),
        axis.title.y = element_text(vjust=3),
        legend.title = element_blank(),
        legend.position = 'n')  +
  scale_fill_manual(values = c("grey80", "plum2", "indianred1", "lightskyblue")) -> g1 #+
  #scale_color_manual(values = brewer.pal(4, "Spectral"))

# write.csv(scores, 'Scores.csv')

        