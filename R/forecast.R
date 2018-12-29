if(FALSE){
forecast <- function(mod, train, test, folds, params){
	                l <- ceiling(nrow(test)/folds)
	                l2 <- l
                	mod <- nnet(y=train_targ, x=train, size=5, decay=0.001, maxit=500)
                preds[1:l] <- rep(0,l)  #as.numeric(predict(mod_svm, test[1:l,]))
                for(j in 1:(folds-1)){
                        for(k in 1:bag){
                                invisible(capture.output(mod_svm <- nnet(y=(c(train_targ2,test_targ2[1:l])), x=rbind(train,test[1:l,]),size=30, decay=0.001, maxit=1000, linout=T,  MaxNWts = 100000)))
                                preds[(l+1):(l+l2)] <- ((as.numeric(predict(mod_svm, test[(l+1):(l+l2),]))))/bag + preds[(l+1):(l+l2)]
                        }
                                cn <- c("change","range","change_prev_1","change_prev_2","change_prev_3","range_prev_1","range_prev_2","range_prev_3")
                                mod_svm <- svm(y=as.factor((c(train_targ,test_targ[1:l]))), x=rbind(train2[,cn],test2[1:l,cn]))
                                preds[(l+1):(l+l2)] <-  (preds[(l+1):(l+l2)]-1) +(as.numeric((predict(mod_svm,test2[(l+1):(l+l2),cn])))-1)/2
                        l <- l+l2
                }

}
}

svm2 <- function(train,targ,folds){
        library(cvTools)
        df <- cvFolds(nrow(train),folds)
        preds <- numeric(nrow(train))
        for(i in 1:folds){
                x <- df$subsets[df$which==i]
                mod <- svm(train[-x,],targ[-x])
                preds[x] <- predict(mod,train[x,])
        }
        return(preds)
}


lm2 <- function(train,targ,folds){
        library(cvTools)
        df <- cvFolds(nrow(train),folds)
        preds <- numeric(nrow(train))
        for(i in 1:folds){
                x <- df$subsets[df$which==i]
                mod <- lm(targ[-x]~.,train[-x,])
                preds[x] <- predict(mod,train[x,])
        }
        return(preds)
}

# x - a feature matrix ORDERED BY TIMESTAMPS INCREASING
# y - a target variable
# k - number of folds
# num_purged - number of observations to purge from the train set on each side
# exec_statement - a regression or classification function that makes a model named "mod" and uses predict
# returns - purged CV predictions on X

############################################################################################################
# EXAMPLE
############################################################################################################
# set.seed(1)
# x <- data.frame(rnorm(100),rnorm(100))
# y <- rnorm(100)
# preds <- purged_k_fold_cross_validation_wrapper(x,y,k=10,num_purged=5,exec_statement="mod <- lm(y~.,data=x)")
# library(randomForest)
# preds2 <- purged_k_fold_cross_validation_wrapper(x,y,k=10,num_purged=5,exec_statement="mod <- randomForest(y~.,data=x)")
# print(cor(preds,preds2))
###########################################################################################################

# Because this is a wrapper, I can pass in different regression/classification functions and hyperparameters to tune

purged_k_fold_cross_validation_wrapper <- function(x,y,k,num_purged,exec_statement){
        x2 <- x
    y2 <- y
        walk <- ceiling(seq(1,nrow(x),length.out=(k+1)))
        preds <- numeric(nrow(x))
            test <- x2[walk[1]:walk[2],]
            x <- x2[(walk[2]+num_purged):nrow(x2),]
                y <- y2[(walk[2]+num_purged):nrow(x2)]
                eval(parse(text = exec_statement))
                    preds[walk[1]:walk[2]] <- predict(mod,test)
                    for(i in 2:(k)){
                                test <- x2[(walk[i]+1):walk[i+1],]
                            if(i!=k){
                                            x <- rbind(x2[1:(walk[i]-num_purged),],x2[(walk[i+1]+num_purged):nrow(x2),])
                                        y <- c(y2[1:(walk[i]-num_purged)],y2[(walk[i+1]+num_purged):nrow(x2)])
                                                } else{
                                                                 x <- x2[1:(walk[i]-num_purged),]
                                                     y <- y2[1:(walk[i]-num_purged)]
                                                             }
                                    eval(parse(text = exec_statement))
                                    preds[(walk[i]+1):walk[i+1]] <- predict(mod,test)
                                        }
                        return(preds)
}

