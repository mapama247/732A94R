# 3.1.2 DIJKSTRA'S ALGORITHM

# function that returns the shortest path from a given initial node to every other node in a graph
# pseudocode: https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm
# video: https://www.youtube.com/watch?v=pVfj6mxhdMw

dijkstra <- function(graph,init_node){
	
	# assert that the arguments are correct
	stopifnot( is.data.frame(graph) , is.numeric(init_node) , (init_node%in%graph$v1 | init_node%in%graph$v2) )
	
	num_of_nodes <- max(graph[,1])		# number of nodes in the graph
	distances <- rep(Inf,num_of_nodes)		# vector of distances from each node to init_node
	unvisited_nodes <- 1:num_of_nodes 		# vector of nodes that have not been explored yet
	visited_nodes <- numeric(0)			# vector of nodes that have already been explored
	distances[init_node] <- 0			# distance from init_node to init_node is 0
	visiting <- init_node 				# the first node to explore is init_node
	
	while(length(unvisited_nodes)!=0){ # iterate until all nodes have been visited
		
		# find the unvisited neighbours of the node that is being explored
		neighbours <- graph$v2[ graph$v1==visiting & !(wiki_graph$v2%in%visited_nodes)]
		neighbours_pos <- which(graph$v1==visiting & graph$v2%in%neighbours)
		weight_to_neighbours <- graph$w[neighbours_pos]
		
		i <- 1 # auxiliary variable
		for(ngb in neighbours){ # loop over all unvisited neighbours
			if(distances[ngb]>graph$w[neighbours_pos[i]]+distances[visiting]){ # if shortest path is found...
				distances[ngb] <- graph$w[neighbours_pos[i]]+distances[visiting] # ...replace previously stored path
			}
			i <- i+1
		}
		
		unvisited_nodes <- unvisited_nodes[!(unvisited_nodes%in%visiting)] # remove the node from unvisited_nodes
		visited_nodes <- c(visited_nodes,visiting) # append the node to visited_nodes
		
		# if there are still unvisited nodes, select a new node to be explored in the next iteration:
		if(length(visited_nodes)<num_of_nodes){
			visiting <- which(distances==min(distances[unvisited_nodes])) # has to be the one with shortest distance to init_node
			visiting <- visiting[!(visiting%in%visited_nodes)][1] # cannot be a node that has been visited before
		}
	}
	return(distances)
}

wiki_graph<-data.frame(v1=c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6),
			     v2=c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5),
			     w=c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9))
dijkstra(wiki_graph, 1) # 0 7 9 20 20 11
dijkstra(wiki_graph, 3) # 9 10 0 11 11 2
