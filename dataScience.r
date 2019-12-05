require(nnet)
require(e1071)
require(class)
require(randomForest)
require(dismo)

mocap.preprocessing <- function(data) {   
    data$User = NULL
    data = as.matrix(data[,1:16])

    newData = matrix(ncol=16)

    for (i in 1:nrow(data)){
        if(sum(data[i,] == "?") == 0){
            newData = rbind(newData, data[i,])
        }
    }

    newData = newData[2:nrow(newData),]
    write.csv(as.data.frame(newData), file = "newData.csv")
    
    return (newData)
}

pca <- function(dataset, cumulativeProb=95.0){
    matrix = as.matrix(dataset)

    norm.matrix = apply(matrix, 2, function(col){(col - mean(col))/sd(col)})
    
    cor.matrix = cor(matrix)

    eigens = eigen(cor.matrix)

    prob = eigens$values/sum(eigens$values)*100
    prob.sum = cumsum(prob)
    
    id = which(prob.sum >= cumulativeProb)
    
    if(cumulativeProb>=100) n_components = length(prob.sum)
    else n_components = length(prob.sum[-id[-1]])
    
    pca.fit = eigens$vectors[,1:n_components]

    pca.matrix  = norm.matrix%*%pca.fit

    ret = list()
    
    ret$eigens = eigens
    ret$prob = prob
    ret$cumprob = prob.sum
    ret$n_components = n_components
    ret$pca.matrix = pca.matrix
    ret$norm.matrix = norm.matrix

    return (ret)    
}

# K value estimator for KNN classification method
knn.kValue <- function(X, Y, kmax=100, k_kfold=10){
    k.values = c(1:kmax)
    results = c(rep(0,length(k.values)))

    kfolds = split.data(Y, k=k_kfold)

    results.parcial = c(rep(0,k_kfold))
    results.final = c(rep(0,kmax))

    for(k_knn in k.values){
        cat("k =", k_knn, "\n")
        for (f in 1:k_kfold){
            cat("\t", "fold =", f, "\n")
            train.ids = which(kfolds == f)
            
            X.train = X[train.ids,]
            Y.train = Y[train.ids,]

            X.test = X[-train.ids,]
            Y.test = Y[-train.ids,]
    
            knn = class.ind(knn(train=X.train, test=X.test, cl=as.factor(Y.train), k=k_knn))

            Y.test.class = class.ind(Y.test)

            results.aux = apply((Y.test.class - knn)^2, 1,
                        function(row) { sum(row) })

            acc = sum(results.aux == 0) / nrow(X.test)
            results.parcial[f] = 1-acc
            
        }

        results.final[k_knn] = mean(results.parcial)
        if(k_knn %% 10 == 0){
            for(i in (k_knn-9):k_knn){
                system(paste("echo ", i, " ", results.final[i], " >> log.txt", sep=""))
            }
        }
    }

    plot(k.values, results.final, type="l")

    ret = list()
    ret$results = results.final
    ret$k.values = k.values
    
    return (ret)
}

# Number of forests estimator for Random Forest  classification method
randomForest.ntree <- function(X, Y, train.size=0.7, plot.Eigen=FALSE, ntree=500) {
    
    ids = sample(1:nrow(X), size=floor(train.size*nrow(X)))
    
    X.train = X[ids,]
    Y.train = Y[ids,]

    X.test = X[-ids,]
    Y.test = Y[-ids,]

    model <- randomForest(X.train, as.factor(Y.train), ntree=ntree, keep.inbag=TRUE, keep.forest=TRUE)
    hat_y = predict(model, X.test, predict.all=TRUE)

    if (plot.Eigen){
        ind = hat_y$individual
        un = unique(Y)

        counter = 0
        for (i in un){
            ind[ind == i] = counter
            counter = counter + 1
        }

        ind = matrix(as.numeric(ind), nrow=dim(ind)[1], ncol=dim(ind)[2])

        eigens = eigen(cov(ind))
        
        prob = eigens$values/sum(eigens$values)*100 
        prob.cumsum = cumsum(prob)
        
        plot(prob)
        ret = list()

        ret$individual = hat_y$individual
        ret$aggregate = hat_y$aggregate
        ret$prob = prob
        ret$prob.cumsum = prob.cumsum

        return (ret)
    }

}

# Kernel Function Estimator for Support Vector Machine  classification method
svm.kernel <- function(X, Y, k_kfold=10, seed=Sys.time()){
    set.seed(seed)
    kernels = c("radial", "linear", "polynomial", "sigmoid")
    models = list()

    Err.mean = matrix(0, nrow=4, ncol=1)
    rownames(Err.mean) = kernels

    kfolds = split.data(Y, k=k_kfold)
    for(f in 1:k_kfold){
        train.ids = which(kfolds==f)

        X.train = X[train.ids,]
        Y.train = Y[train.ids,]

        X.test = X[-train.ids,]
        Y.test = Y[-train.ids,]

        Y.test.class = class.ind(Y.test)
        count = 1
        for(k in kernels){
            model <- svm(x=X.train, y=as.factor(Y.train), kernel = k)
            
            hat_Y = class.ind(predict(model, X.test))

            results = apply((Y.test.class - hat_Y)^2, 1,
		      		function(row) { sum(row) })
		    
            acc = sum(results == 0) / nrow(X.test)

            Err.mean[count,] = Err.mean[count,] + (1 - acc)   
            count = count + 1

        }
    }
    Err.mean = Err.mean/k_kfold

    return (Err.mean)
}

split.data <- function(Y, k=10){
    classes = unique(Y)
    folds = c()

    for (c in classes){
        class.ids = which(Y == c)
        fold.ids = kfold(class.ids, k=k)

        folds[class.ids] = fold.ids
    }

    return(folds)
}


kfold.cv <- function(X, Y, k_kfold=10, n_methods=5, seed=Sys.time(), hidden.length=30, mlp.maxit=1000, eta=1,
                    kernel="radial", k_knn=3, laplace=0, ntree=300){
    set.seed(seed)

    #Normalize
    X = apply(X, 2, function(col){(col - mean(col))/sd(col)})
    
    Err = matrix(0, nrow=n_methods, ncol=k_kfold)
    models = list()
    pred = list()

    kfolds = split.data(Y, k=k_kfold)

    for (f in 1:k_kfold){
        train.ids = which(kfolds == f)
        
        # Y -> must be as.matrix
        X.train = X[train.ids,]
        Y.train = Y[train.ids,]

        X.test = X[-train.ids,]
        Y.test = Y[-train.ids,]

        #Train
        
        #mlp
        models$mlp <- nnet(X.train, class.ind(Y.train), size=hidden.length, maxit=mlp.maxit, decay=eta)
        #models$mlp5 <- nnet, size=5
        #models$mlp20 <- nnet, size=20

        #svm
        models$svm <- svm(x=X.train, y=as.factor(Y.train), kernel = kernel)
        #models$svm.sigmoid <- svm(X.train, as.factor(Y.train), kernel = "sigmoid")

        #knn
        models$knn <- knn(train=X.train, test=X.test, cl=as.factor(Y.train), k=k_knn)
        #models$k10nn <- knn, k=10
        #models$k20nn <- knn, k=20
        
        #naive bayes
        models$naiveBayes <- naiveBayes(X.train, as.factor(Y.train), laplace=laplace)
        #outros valores de laplace?

        #random forest
        models$randomForest <- randomForest(x = X.train, y = as.factor(Y.train), ntree=ntree)
        #models$randomForest <- randomForest(X.train, as.factor(Y.train), ntree=ntree, keep.inbag=TRUE, keep.forest=TRUE)
        #varia ntree...

        #Test

        #mlp
        pred$mlp = predict(models$mlp, X.test)
        for(row in 1:nrow(pred$mlp)){
            id = which.max(pred$mlp[row,])
            pred$mlp[row, id] = 1
            pred$mlp[row, -id] = 0
        }
        #or usa hat_Y[which.max(hat_Y)] = 1
        
        #svm
        pred$svm = class.ind(predict(models$svm, X.test))

        #knn
        pred$knn = class.ind(models$knn)
        
        #naive bayes
        pred$naiveBayes = class.ind(predict(models$naiveBayes, X.test))
        
        #random Forest
        pred$randomForest = class.ind(predict(models$randomForest, X.test))
        #hat_Y$randomForest = predict(models$randomForest, X.test, predict.all=TRUE)

        Y.test.class = class.ind(Y.test)
        count = 1
        for (hat_Y in pred){
            #### calcula acc ####

            results = apply((Y.test.class - hat_Y)^2, 1,
		      		function(row) { sum(row) })
		    acc = sum(results == 0) / nrow(X.test)

            Err[count, f] = 1 - acc
            count = count + 1
        }
    }

    ret = list()
    
    ret$mlp = Err[1,]
    ret$svm = Err[2,]
    ret$knn = Err[3,]
    ret$naiveBayes = Err[4,]
    ret$randomForest = Err[5,]

    ret$all = Err

    return(ret)
}

plot.knn <- function(filename="log.txt"){
    kvalues = read.table(file=filename, header=F)
    plot(kvalues, main="K values for KNN", ylab="Error rate", xlab="k values", cex.main=3, cex.lab=2.5, col=1, type='l')


    return(kvalues)
}

test.mlp <- function(X, Y, k_kfold=10, seed=Sys.time(), hidden.length=c(10, 15,20,30,50), mlp.maxit=1000, eta=5e-2){
    set.seed(seed)

    #Normalize
    X = apply(X, 2, function(col){(col - mean(col))/sd(col)})
    
    Err = rep(0, length(hidden.length))

    kfolds = split.data(Y, k=k_kfold)

    for (f in 1:k_kfold){
        train.ids = which(kfolds == f)
        
        X.train = X[train.ids,]
        Y.train = Y[train.ids,]

        X.test = X[-train.ids,]
        Y.test = Y[-train.ids,]

        count = 1
        #Train
        for (size in hidden.length){
            #mlp
            model <- nnet(X.train, class.ind(Y.train), size=size, maxit=mlp.maxit, decay=eta, MaxNWts=20000)
            
            #Test
            #mlp
            hat_Y = predict(model, X.test)
            for(row in 1:nrow(hat_Y)){
                id = which.max(hat_Y[row,])
                hat_Y[row, id] = 1
                hat_Y[row, -id] = 0
            }
            
            Y.test.class = class.ind(Y.test)
            
            results = apply((Y.test.class - hat_Y)^2, 1,
                    function(row) { sum(row) })
            acc = sum(results == 0) / nrow(X.test)

            Err[count] = Err[count] + (1 - acc)
            count = count + 1
        }
    }

    Err.mean = Err/k_kfold    
    return(Err.mean)
}