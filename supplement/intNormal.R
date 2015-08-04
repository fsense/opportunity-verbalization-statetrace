post.prob.order = function(order=NULL,mus,sig2)
{
  if(is.null(order))
  {
    order=1:length(mus)
  }else{
    if(length(mus)!=length(order))
    {
      stop("length of order vector must be the same as length of mus vector")
    }
  }
  mus = mus[order]
  sig2 = sig2[order]
  
  # We will determine the Pr(x1>x2>x3>...>xn)
  
  # normal approx
  
  covs = diag(as.vector(sig2))
  rotMat = matrix(c(rep(c(-1,1,rep(0,length(mus)-1)),length(mus)-2),-1,1),byrow=TRUE,ncol=length(mus))
  rotMat = rbind(c(1,rep(0,ncol(rotMat)-1)),rotMat)
  newCov = rotMat%*%covs%*%t(rotMat)
  newMu = rotMat%*%mus
  
  omxMnor(newCov,newMu,lbound=rep(0,length(mus)),ubound=rep(Inf,length(mus)))
}


# Get a list of probable orderings, using sampling
get.orders = function(mus,sig2,M=1000)
{
  z = rnorm(length(mus)*M,mus,sqrt(sig2))
  dim(z) = c(length(mus),M)
  
  w = apply(z,2,function(v) paste(order(v,decreasing=TRUE),collapse=""))
  x = strsplit(unique(w),"")
  t(matrix(as.numeric(unlist(x)),nrow=length(x[[1]])))
}

# Get the posterior probabilities of "probable" orderings
# (Will call the other functions)
get.good.probs = function(mus,sig2,M=1000)
{
  x = get.orders(mus,sig2,M)
  prs = apply(x,1,post.prob.order,mus=mus,sig2=sig2)
  z = data.frame(ord = apply(x,1,paste,collapse=","),postprob=prs)
  z[order(z[,1]),]
}

checkOrdering = function(order,labs,articRestrict=TRUE){
  ordered = labs[order]
  after <- function(a,b)
    which(ordered==a)>which(ordered==b)
  
  if(after("4.articulate","2.articulate")) return(FALSE)
  if(after("8.articulate","2.articulate")) return(FALSE)
  if(after("8.articulate","4.articulate")) return(FALSE)  
  
  if(after("4.silent","2.silent")) return(FALSE)
  if(after("8.silent","2.silent")) return(FALSE)
  if(after("8.silent","4.silent")) return(FALSE)  
  
  if (articRestrict) {
    if(after("2.articulate","2.silent")) return(FALSE)
    if(after("4.articulate","4.silent")) return(FALSE)
    if(after("8.articulate","8.silent")) return(FALSE)
  }
  
  return(TRUE)
  
}


do.sampler<-function(means,sds,M=10000)
{
  crit = pnorm(0,means,sds)
  s = qnorm(runif(M*length(crit),crit,1),means,sds)
  dim(s) = c(length(crit),M)
 t(s)
}

restrict.samples.mean<-function(means,sds,labs,M=10000,articRestrict=TRUE){
  samples = do.sampler(means,sds,M)
  ords = apply(samples,1,order)
  good = apply(ords,2,checkOrdering,labs=labs,articRestrict=articRestrict) &
    apply(samples,1,function(v) all(v>0))
  return(samples[good,])
}
