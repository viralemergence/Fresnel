
varimp.pbart <- function(model, plot=TRUE) {

  names <- colnames(model$varcount)
  varimps <- colMeans(model$varcount/rowSums(model$varcount))
  
  var.df <- data.frame(names, varimps)

  var.df$names <- factor(var.df$names)
  var.df <- transform(var.df, names = reorder(names,
                                              -varimps))

  if(plot==TRUE){
  #g1 <- ggplot2::ggplot(var.df, aes(y=varimps, x=names)) +
  #  geom_bar(stat="identity", color="black") +
  #  theme(axis.text.x = element_text(angle = 45)) + 
  #  ylab("Relative importance") + theme_bluewhite()
  #print(g1)
  
  rel <- model$varcount/rowSums(model$varcount)
  colnames(rel) <- names
  
  rel %>% data.frame() %>% gather() %>%
    group_by(key) %>%
    summarise(mean = mean(value),
              sd = sd(value, na.rm = TRUE)) %>% 
    transform(Var = reorder(key, mean)) %>%
        ggplot(aes(x = Var, y = mean)) +
              geom_pointrange(aes(y = mean, x = Var, ymin = mean-sd, ymax = mean+sd),
                              color="#00AFDD") + 
              xlab(NULL) + ylab("Variable importance") + coord_flip() + 
              theme_bw() + theme(legend.position = "none",
                                 axis.title.x = element_text(size=rel(1.3), vjust = -0.8),
                                 axis.text.y = element_text(size=rel(1.4)),
                                 plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
                                 panel.grid.minor = element_blank(),
                                 panel.grid.major.x = element_line(color='grey',
                                                                   linetype='dashed')) -> p
  
  print(p)
  
  }

  return(var.df)

}

