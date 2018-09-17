#' Takes a graph and an initial node and calculates the shortest path from the initial node to every other node in the graph.
#'
#' It is an algorithm used for finding the shortest path from a single node to a single destination node 
#' by stopping the algorithm onc e the shortest path to the destination node has been determined.
#'
#' @param graph A data frame.
#' @param init_node A number.
#' @return The vector of the shortest path to every other node from the starting node.
#' @examples
#' dijkstra(wiki_graph, 1)
#' dijkstra(wiki_graph, 3)
#' 
#' @references 
#' \href{https://en.wikipedia.org/wiki/Dijkstra\%27s_algorithm}{Dijkstra}
#' 
#' @export 

dijkstra <- function(graph, init_node)
{
  if(!is.data.frame(graph))
    stop("Illegal input!")
  if (!is.numeric(init_node))
    stop("Illegal input!")
  if (!ncol(graph)==3)
    stop("Illegal input!")
  if (!all.equal(colnames(graph), c ("v1", "v2", "w")))
    stop("Illegal input!")
  if (0 %in% graph[,3])
    stop("weight can't be 0")
  if (!(init_node %in% graph[,1]))
    stop("wrong input initial node")
  
  node_list <- unique(graph[,1])
  weight <- c()
  
  for (node in node_list)
  {
    if (node == init_node)
    {
      weight[node] <- 0
    }
    else
    {
      weight[node] <- 1/0   #This is an inifinity number
    }
  }
  
  while (length(node_list)>0)
  {
    current_node <- which(weight == min(weight[node_list]))
    node_list <- node_list[! node_list %in% current_node]
    for (neighbor in graph[graph$v1 == current_node,]$v2)
    {
      dist <- graph[,3][which(graph[,1]==current_node & graph[,2]==neighbor)] + weight[current_node]
      current_dist <- weight[neighbor]
      ifelse((dist <= current_dist), weight[neighbor] <- dist, weight[neighbor] <- current_dist)
    }
  }
  return(weight)
}