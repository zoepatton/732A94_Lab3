#' Dijkstra algorithm
#'
#' \code{dijkstra} returns the greatest common divisor of two integers.
#'
#' The algorithm takes a graph and an initial node and calculates the shortest path from the inital node to every other node in the graph 
#'
#' @param graph a data.frame
#' 
#' @param init_node numeric
#' 
#' @return a vector representing the shortest path to every other node from the starting node
#' @export
#' @source \href{https://en.wikipedia.org/wiki/Dijkstra\%27s_algorithm}{Wikipedia}
#'   

dijkstra <- function(graph, init_node){
  if(!is.data.frame(graph)) {
    stop("error: graph has to be a data frame")
  }
  if(ncol(graph) != 3){
    stop("error: incorrect dataframe size")
  }
  if(!is.numeric(init_node) | length(init_node)!=1){
  stop("error: init_node needs to be a scaler numeric")   
  }
  if(any(colnames(graph) != c("v1", "v2", "w"))){
    stop("error: dataframe has to have v1, v2 and w column names")
  }
  if(!(init_node %in% unique(c(graph[ ,1], graph[ ,2])))) {
    stop("error: init_node is not a value in the dataframe")
  }

  Q <- c()                        #Vertex set
  w <- c()                        #Distances
  prev_v <- c()                   #Previous visited node
  n <- unique(graph[,1])          
  
  for(i in 1:length(n)){            
    
    w[i] <- Inf
    prev_v[i] <- NA
    Q[i] <-  i                  
    
  }
  
  w[init_node] <- 0
  
  while(length(Q) > 0){
    
    u <- Q[which.min(w[Q])]
    
    
    if(is.element(u, Q) == TRUE){                   #Remove u from Q
      
      Q <- Q[-which.min(w[u])]
      
    }
    
    neighbors <- graph$v1[which(graph$v2 == u)]
    
    for(i in neighbors){
      length <- graph$w[which(graph$v1 == i & graph$v2 == u)]
      alt <- w[u] + length
      
      if(alt < w[i]){ 
        w[i] <- alt 
        prev_v[i] <- u
      }
    }
    
  }
  
  return(w)
  
}
