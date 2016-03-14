require('edgeR')

# x is the independent variable, a 2-group factor
# Y is a matrix of samples x dependent variables
# returns p-values
"exact.test.edgeR" <- function(x, Y, use.fdr=TRUE,
		norm.factor.method=c('none','RLE')[1],
		include.foldchange=FALSE){
	require('edgeR')
	
	x <- as.factor(x)
	if(length(levels(x)) != 2) stop('x must be a 2-level factor')

	d <- DGEList(count=t(Y), group=x)
	d <- calcNormFactors(d, method=norm.factor.method)
 	d <- estimateCommonDisp(d)
 	d <- estimateTagwiseDisp(d)
	et <- exactTest(d)
	
	return(et)
}

# x is the independent variable
# Y is a matrix of samples x dependent variables
# returns p-values
"glm.edgeR" <- function(x, Y, covariates=NULL,
		use.fdr=TRUE, estimate.trended.disp=TRUE,
		verbose=TRUE){

	require('edgeR')

	# drop NA's
	ix <- !is.na(x)
	Y <- Y[ix,]
	x <- x[ix]
	if(!is.null(covariates)){
		if(is.null(dim(covariates))){
			covariates <- as.data.frame(covariates)
		}
		covariates <- covariates[ix,,drop=F]

		# drop constant covariates
		covariates <- covariates[,apply(covariates,2,function(xx) length(unique(xx)) > 1),drop=F]
	}

	if(verbose) cat('Making DGEList...\n')
	d <- DGEList(count=t(Y), group=x)
	if(verbose) cat('calcNormFactors...\n')
	d <- calcNormFactors(d,)
	if(!is.null(covariates)){
		covariates <- as.data.frame(covariates)
		covariates <- cbind(x, covariates)
		covariates <- droplevels(covariates)
		design <- model.matrix(~ ., data=covariates)		
	} else {
		design <- model.matrix(~x)
	}

	if(verbose) cat('estimate common dispersion...\n')
	d <- estimateGLMCommonDisp(d, design)
	if(estimate.trended.disp){
		if(verbose) cat('estimate trended dispersion...\n')
		d <- estimateGLMTrendedDisp(d, design)
	}
	if(verbose) cat('estimate tagwise dispersion...\n')
	d <- estimateGLMTagwiseDisp(d,design)
	
	if(verbose) cat('fit glm...\n')
	fit <- glmFit(d,design)
	if(verbose) cat('likelihood ratio test...\n')
	lrt <- glmLRT(fit,coef=2)
	
	return(lrt)
}


# runs set of differential "expression" tests
# x is a sample x obs count matrix, e.g. taxa, otus
#
# test.list is a named list of tests, each of the form:
#    list(ix=<logical indices of samples to test>,
#         group=<grouping vector, factor with 2 levels>,
#         covariate.names=<list of map column headers, can be omitted>)
# map must be included if any tests have covariate.names listed
#
"run.DGE.tests" <- function(x, test.list, map=NULL, verbose=FALSE){
	res <- list()

	for(j in seq_along(test.list)){
		ix <- test.list[[j]]$ix
		y <- test.list[[j]]$group
		covariate.names <- test.list[[j]]$covariate.names
		if(is.null(covariate.names)){	
			res[[names(test.list)[j]]] <- exact.test.edgeR(x[ix,], y[ix])
		} else {
			res[[names(test.list)[j]]] <- 
					exact.test.edgeR.covariates(x[ix,],y[ix],
						covariates = map[ix,covariate.names])
		}
		if(verbose){
			cat('\n\n',names(test.list)[j],'\n')
			print(topTags(res[[names(test.list)[j]]]))
		}
	}
	return(res)
}

