"lgd" <- function(d,neighborhood.size=4,weighted=TRUE) {
    require('igraph')
    d <- as.matrix(d)
    dd <- matrix(0,nrow(d), nrow(d))
    for(i in 1:nrow(dd)) dd[i,order(d[i,])[1:(neighborhood.size+1)]] <- 1
    if(weighted) dd[dd>0] <- d[dd>0] else weighted <- NULL
    g <- graph.adjacency(dd,weighted=weighted)
    return(shortest.paths(g))
}