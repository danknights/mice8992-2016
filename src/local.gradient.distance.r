"lg.dist" <- function(d,neighborhood.size=4, weighted=TRUE) {
    g <- lg.graph(d, neighborhood.size=neighborhood.size, weighted=weighted)
    lgd <- shortest.paths(g)
    return(lgd)
}

"lg.graph" <- function(d,neighborhood.size=4,weighted=TRUE) {
    require('igraph', warn.conflicts=FALSE, quietly=TRUE)
    d <- as.matrix(d)
    dd <- matrix(0,nrow(d), nrow(d))
    for(i in 1:nrow(dd)) dd[i,order(d[i,])[1:(neighborhood.size+1)]] <- 1
    if(weighted) dd[dd>0] <- d[dd>0] else weighted <- NULL

    g <- graph.adjacency(dd,weighted=weighted, mode='undirected')
    return(g)
}
