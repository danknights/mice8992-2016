
# Tibshirani and Walther's Prediction Strength (2005)
# Randomly splits data in half, clusters halves A & B separately,
# assigns B to nearest centroids in A
# Takes the worst prediction strength (relative frequency of correct B luster pairs) for an A cluster
# assigns A to nearest centroids in B
# Takes the worst prediction strength (relative frequency of correct B luster pairs) for a B cluster
# Returns the average for M repeats
# Returns vector of prediction strengths for all values of K
"prediction.strength" <- function(dmat, K=2:10, M=10){
    require('cluster')
    dmat <- as.matrix(dmat)
    N <- nrow(dmat)
    
    result <- matrix(0,nrow=M,ncol=length(K))
    for(j in 1:M){
        r.ix <- sample(N) # random ordering of indices
        A.ix <- r.ix[1:floor(N/2)]
        B.ix <- r.ix[-(1:floor(N/2))]

        for(k in K){
            cl.A <- partial.pam(dmat, A.ix, k=k)
            cl.B <- partial.pam(dmat, B.ix, k=k)
        
            clusterings <- list(cl.A,cl.B)
            group.indices <- list(A.ix, B.ix)
            orderings <- list(AB=1:2, BA=2:1)
        
            scores <- c(0,0)
            for(which.ordering in 1:2){
                cl.1 <- clusterings[[orderings[[which.ordering]][1]]]
                cl.2 <- clusterings[[orderings[[which.ordering]][2]]]
                ix.1 <- group.indices[[orderings[[which.ordering]][1]]]
                ix.2 <- group.indices[[orderings[[which.ordering]][2]]]
            
                # get worst cluster for cl.A assignments of half B
                ncorrect <- numeric(k) # holds the number of correct predictions for each cluster
                nwrong <- numeric(k)
        
                # check every cluster with more than 1 point
                for(i in 1:k){
                    cl.ix <- which(cl.1[ix.2]==i) # indices of which B points are in this cluster
                    if(length(cl.ix) > 1){
                        for(m in 1:(length(cl.ix)-1)){
                            for(n in (m+1):length(cl.ix)){
                                if(cl.2[ix.2][cl.ix[m]] == cl.2[ix.2][cl.ix[n]]) {
                                    ncorrect[i] <- ncorrect[i] + 1
                                } else {
                                    nwrong[i] <- nwrong[i] + 1
                                }
                            }
                        }
                    }
                }

                # cluster of size 0 is bad; the prediction strength remains 0
                cluster.sizes <- ncorrect + nwrong
                if(min(cluster.sizes) > 0) {
                    correct.rate <- ncorrect / cluster.sizes
                    scores[which.ordering] <- min(correct.rate)
                }
            }
        
            result[j,which(K==k)] <- mean(scores)
        }
    }
    return(colMeans(result))
}

# Tibshirani and Walther's Prediction Strength (2005)
# Randomly splits data in half, clusters halves A & B separately,
# assigns B to nearest centroids in A
# Takes the worst prediction strength (relative frequency of correct B luster pairs) for an A cluster
# assigns A to nearest centroids in B
# Takes the worst prediction strength (relative frequency of correct B luster pairs) for a B cluster
# Returns the average for M repeats
# Returns vector of prediction strengths for all values of K,
"prediction.strength.cv" <- function(dmat, nfolds=5, K=2:10){
    dmat <- as.matrix(dmat)
    N <- nrow(dmat)
    
    # set nfolds based on max K
    max.nfolds <- floor(N / (1+max(K)))
    if(max.nfolds < nfolds){
        cat(sprintf('Warning, %d samples at %d clusters can only support %d folds.\n',
                    N, max(K), max.nfolds))
        nfolds <- max.nfolds
    }
    
    result <- matrix(0,nrow=nfolds,ncol=length(K))
    for(k in K){
        folds <- jackknife.folds(n=N, nfolds=nfolds)
        for(fold in 1:nfolds){
            testix <- which(folds==fold)
            trainix <- which(folds!=fold)
            cl.A <- partial.pam(dmat, trainix, k=k)
            cl.B <- partial.pam(dmat, testix, k=k)
    
            # get worst cluster for cl.A assignments of half B
            ncorrect <- numeric(k) # holds the number of correct predictions for each cluster
            nwrong <- numeric(k)

            # check every cluster with more than 1 point
            for(i in 1:k){
                cl.ix <- which(cl.A[testix]==i) # indices of which B points are in this cluster
                if(length(cl.ix) > 1){
                    for(m in 1:(length(cl.ix)-1)){
                        for(n in (m+1):length(cl.ix)){
                            if(cl.B[testix][cl.ix[m]] == cl.B[testix][cl.ix[n]]) {
                                ncorrect[i] <- ncorrect[i] + 1
                            } else {
                                nwrong[i] <- nwrong[i] + 1
                            }
                        }
                    }
                }
            }

            # cluster of size 0 is bad; the prediction strength remains 0
            cluster.sizes <- ncorrect + nwrong
            if(min(cluster.sizes) > 0) {
                correct.rate <- ncorrect / cluster.sizes
                result[fold,which(K==k)] <- min(correct.rate)
            }
        }
    }
    return(colMeans(result))
}


# Get balanced folds where each fold has close to overall class ratio
"balanced.folds" <- function(y, nfolds=10){
    folds = rep(0, length(y))
    classes = levels(y)
    # size of each class
    Nk = table(y)
    # -1 or nfolds = len(y) means leave-one-out
    if (nfolds == -1 || nfolds == length(y)){
        invisible(1:length(y))
    }
    else{
    # Can't have more folds than there are items per class
    nfolds = min(nfolds, max(Nk))
    # Assign folds evenly within each class, then shuffle within each class
        for (k in 1:length(classes)){
            ixs <- which(y==classes[k])
            folds_k <- rep(1:nfolds, ceiling(length(ixs) / nfolds))
            folds_k <- folds_k[1:length(ixs)]
            folds_k <- sample(folds_k)
            folds[ixs] = folds_k
        }
        invisible(folds)
    }
}

# Get nfolds folds for n objects
"jackknife.folds" <- function(n, nfolds=10, random=TRUE){
    folds = rep(0, n)

    # -1 or nfolds = len(y) means leave-one-out
    if (nfolds == -1 || nfolds == n){
        return(1:n)
    }
    else{        
        folds <- rep(1:nfolds, length=n)
        if(random) folds <- sample(folds)
        return(folds)
    }
}

# performs PAM for the pamix index samples, then assigns nearest medoid
# for the others
"partial.pam" <- function(d,pamix,k=2){
    d <- as.matrix(d)
    p <- pam(as.dist(d[pamix,pamix]), k)
    medoids <- p$medoids
    # start of with the pamix clusters
    cl <- integer(nrow(d))
    cl[pamix] <- p$clustering
    
    # for(i in 1:nrow(d[-pamix,])){
    #     cl[-pamix][i] <- which.min(d[i,p$medoids])
    # }
    cl[-pamix] <- apply(d[-pamix,p$medoids],1,which.min)
    return(cl)
}

