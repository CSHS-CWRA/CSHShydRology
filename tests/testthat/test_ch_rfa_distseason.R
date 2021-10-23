######################################################
## testing ch_rfa_distseason
## Martin Durocher <mduroche@uwaterloo.ca>
#######################################################

nsite <- 1000
scoord <- data.frame(angle = runif(nsite,0,2*pi),
                     radius = runif(nsite))

d1 <- ch_rfa_distseason(radius ~ angle , scoord)

radMat <- as.matrix(dist(scoord[,2], method = 'man'))

angMat <- mx <-  matrix(0, nsite,nsite)

for (ii in seq(nsite - 1)) {
  for (jj in seq(ii + 1,nsite)) {
	  mx[jj,ii] <- max(scoord[c(ii,jj),1]) - min(scoord[c(ii,jj),1])
	  angMat[jj,ii] <- min(2*pi - mx[jj,ii], mx[jj,ii])/pi
	  angMat[ii,jj] <- angMat[jj,ii]
	  mx[ii,jj] <- mx[jj,ii]
  }
}

d2 <-  sqrt(angMat^2 + radMat^2)

expect_equal(d1,d2)

