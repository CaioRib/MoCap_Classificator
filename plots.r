par(mar=c(4,6,4,4)) # ordem = bottom, left, top, right
hist(data[,"Class"], col="chocolate", breaks=0:5, probability=T, ylim=c(0, 0.3), main="MoCap classes probability distribution (post processing)", cex.main=3, cex.lab=2.5, ylab="Probability", xlab="Classes")
hist(old.data[,"Class"], col="chocolate", breaks=0:5, probability=T, ylim=c(0, 0.3), main="MoCap classes probability distribution", cex.main=3, cex.lab=2.5, ylab="Probability", xlab="Classes")
plot(r$cumprob, main="PCA analysis", ylab="Cumulative percentage of variation", xlab="Number of features", cex.main=3, cex.lab=2.5, col=1, type="b")
plot(ret$results.final, main="K values for KNN", ylab="Error rate", xlab="k values", cex.main=3, cex.lab=2.5, col=1, type="l")
plot(ret$prob.cumsum, main="Number of tree analysis", ylab="Cumulative percentage of importance", xlab="Number of trees", cex.main=3, cex.lab=2.5, col=1)

