# @returns - Calculates Sharp ratio of portfolio
#
# @ param port_mat - a 0/1 matrix of weather a trade was executed (each column represents a stock)
# @ param real_mat - a matrix of stock returns (rows are dates, coulmns stock movements)

sharp_port <- function(port_mat, real_mat,trys=1000){
	trades <- c()
	port_mat <- as.matrix(port_mat)
	real_mat <- as.matrix(real_mat)
	for(i in 1:ncol(port_mat)){
		trades <- c(trades,real_mat[port_mat[,i]==1, i])
	}
	trades <- trades -1
	print(trades)
	print(mean(trades))
	mult <- (256/(nrow(port_mat))*sum(port_mat))
	if(ncol(port_mat)==1){
		print((((mean(trades) * sqrt(mult))/sd(trades))))
		m <- cor(port_mat[,1],real_mat[,1])
		print(m)
		rows <- nrow(port_mat)
		counts <- 0
		for(i in 1:trys){
			if(cor(port_mat[sample(1:rows),1],real_mat[,1]) > m)
				counts <- counts+1
		}
		print(counts/trys)
			
	} else{
		        print(sharp_port_p(port_mat, real_mat))
	}
	return(((mean(trades) * sqrt(mult))/sd(trades)))
}

sharp_port2 <- function(port_mat, real_mat,trys=1000){
        trades <- c() 
        port_mat <- as.matrix(port_mat)
        real_mat <- as.matrix(real_mat)
        for(i in 1:ncol(port_mat)){
                trades <- c(trades,real_mat[port_mat[,i]==1, i]) 
        }   
        trades <- trades -1
        mult <- (256/(nrow(port_mat))*sum(port_mat))
        if(ncol(port_mat)==1){
                m <- cor(port_mat[,1],real_mat[,1])
                rows <- nrow(port_mat)
                counts <- 0
                for(i in 1:trys){
                        if(cor(port_mat[sample(1:rows),1],real_mat[,1]) > m)
                                counts <- counts+1
                }   
    
        }
        return(((mean(trades) * sqrt(mult))/sd(trades)))
}



# @returns - a non-parametric P-value of a sharp 
#
# @ param port_mat - a 0/1 matrix of weather a trade was executed (each column represents a stock)
# @ param real_mat - a matrix of stock returns (rows are dates, coulmns stock movements)
# @ param trials - number of trial to compute P-value

sharp_port_p <- function(port_mat, real_mat, trials=1000){
	port_mat <- as.matrix(port_mat)
	real_mat <- as.matrix(real_mat)
	shp <- sharp_port2(port_mat, real_mat)
	sums <- apply(port_mat,1,sum)
	cols <- ncol(port_mat)
	count <- 0
	for(i in 1:trials){
		for(j in 1:length(sums)){
			s <- sample(1:cols,sums[j])
			port_mat[j,s] <- 1
			port_mat[j,-s] <- 0
		}
		t_shp <- sharp_port2(port_mat, real_mat)
		if(t_shp > shp){
			count <- count + 1
		}
	}
	return((count/trials))
}











