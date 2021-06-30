admixtureplot <- function(str_out,pops,k,xaxis = T)
{
  require(tidyverse)
  #Check the number of k specified in the input matches the data
  dataK <- ncol(str_out)
  if(dataK != k) stop('The specified number of clusters does not match the data')
  
  #Sort columns by prevalence
  str_out <- str_out[,order(apply(str_out,2,sum),decreasing = T)]

  #Turn into long format
  x <- gather(str_out,key = "K",value = "Q") %>%
    mutate(ind = rep(1:nrow(str_out),k))
  
  #Population positions
  x$pop <- rep(pops$X1,k)
  str_out$pop <- pops$X1
  pop_pos <- cumsum(tapply(1:nrow(str_out),str_out$pop,length))
  labpos <- pop_pos
  
  for(i in 1:length(pop_pos))
  {
    if(i == 1)
    {
      labpos[i] <- pop_pos[i]/2
    } else
    {
      labpos[i] <- pop_pos[i-1] + (pop_pos[i]-pop_pos[i-1])/2
    }
  }
  
  labpos <- round(labpos,0)
  
  #Set colours
  mycols <- c("#a6cee3",
              "#1f78b4",
              "#b2df8a",
              "#33a02c",
              "#fb9a99",
              "#e31a1c",
              "#fdbf6f",
              "#ff7f00",
              "#cab2d6",
              "#6a3d9a")[1:k]
  
  #Make a graph
  ggplot(x,
         aes(x = factor(ind),y = Q, fill = K))+
    geom_bar(stat = "identity",width = 1)+
    theme_bw()+
    scale_x_discrete(breaks = labpos,labels = names(pop_pos))+
    scale_y_continuous(expand = c(0,0))+
    geom_vline(xintercept = pop_pos)+
    xlab("")+
    ylab("")+
    scale_fill_manual(values = mycols)+
    if(xaxis) {
      theme(axis.text.x = element_text(angle = 90,size = 8,hjust = 1),
            axis.ticks = element_blank(),
            legend.position = "none")
    } else {
      theme(axis.text.x = element_blank(),
            axis.ticks = element_blank(),
            legend.position = "none")
    }
  
}
