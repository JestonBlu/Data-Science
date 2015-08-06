
cluster.mds = function(obs) {

  ## create a data frame of some random points
  dt = data.frame(x = rnorm(obs, 1, 2),
                  y = rnorm(obs, -1, 1),
                  z = rnorm(obs, 2, 4))
  
  ## create a distance matrix
  mds.clust = as.matrix(daisy(dt, metric = "euclidean"))
  mds = data.frame(cmdscale(mds.clust, k = 2))
  colnames(mds) = c("x", "y")
  
  mds$id = 1:nrow(mds)
  
  mds.clust = data.frame(id = mds$id, mds.clust)
  
  nn = data.frame()
  
  for (i in 2:(obs + 1)) {
    mds.clust = mds.clust[order(mds.clust[, i]), ]
    id_1 = mds.clust[2, 1]
    id_2 = mds.clust[3, 1]
    id_3 = mds.clust[4, 1]
    
    nn = rbind(nn, c(id = i-1,
                     nn1 = id_1,
                     nn2 = id_2,
                     nn3 = id_3))
  }
  
  colnames(nn) = c("id", "nn1", "nn2", "nn3")
  
  nn1 = join(data.frame(id = nn$nn1), mds, type = "inner")
  colnames(nn1) = c("nn1_id", "nn1_x", "nn1_y")
  
  nn2 = join(data.frame(id = nn$nn2), mds, type = "inner")
  colnames(nn2) = c("nn2_id", "nn2_x", "nn2_y")
  
  nn3 = join(data.frame(id = nn$nn3), mds, type = "inner")
  colnames(nn3) = c("nn3_id", "nn3_x", "nn3_y")
  
  mds = cbind(mds, nn1, nn2, nn3)
  
  return(
    ggplot(mds) + 
      geom_point(aes(x = x, y = y), size= 1) +
      geom_segment(aes(x = x, xend = nn1_x, y = y, yend = nn1_y), 
                   color = "blue", alpha = .15) +
      geom_segment(aes(x = x, xend = nn2_x, y = y, yend = nn2_y), 
                   color = "blue", alpha = .15) +
      geom_segment(aes(x = x, xend = nn3_x, y = y, yend = nn3_y), 
                   color = "blue", alpha = .15) +
      scale_x_continuous("") +
      scale_y_continuous("") +
      ggtitle(
        paste("MDS with Nearest Neighbor Connections,",
              obs, "Observations")) +
      theme(plot.title = element_text(face = "bold", size = 14),
            axis.text = element_blank(),
            axis.ticks = element_blank())
  )
}
