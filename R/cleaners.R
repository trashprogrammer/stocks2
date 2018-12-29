download <- function(syms){
        system(paste0("python ~/mods/stocks_daily.py --list ", paste(syms, collapse=" ")))
}

download2 <- function(syms){
        for(s in syms){
                system(paste0("python ~/crypto.py ",s," 2013-01-01 2018-12-31 > ~/cryptos/",s,".csv"))
        }
}

download_earnings <- function(syms){
	for(s in syms){ 
		tryCatch({
			system(paste("python ~/mods/earnings/scraper.py",s))
		},error=function(e){})
	}	
}

download_updown <- function(syms){
	for(s in syms){ 
		tryCatch({	
			system(paste("python ~/mods/updown2/table_downloader.py ", s))
		},error=function(e){})
	}	
}

download_google <- function(syms){
	for(s in syms){ 
		tryCatch({	
			system(paste0("python ~/mods/google_searches/trends.py '", get_name(s),"' ",s))
		},error=function(e){})
	}	
}

clean_crypto <- function(coin){
	x <- read.csv(paste0("~/cryptos/",coin,".csv"))
	x$Date <- as.Date(as.character(x$Date), "%b %d %Y")
	x$change <- x$Close/x$Open
	x$range <- x$High/x$Low
	return(x)
}

ud <- function(stock,download=T){
	    name <- stock <- toupper(stock)
	    f <- paste("~/mods/updown2/db/",name, ".csv", sep="")
	    if(!file.exists(f)){
		download_updown(name)
	    } else if(as.POSIXlt.POSIXct(Sys.time()-46400) > file.info(f)$mtime & download==T){    
		download_updown(name)
	    }  
	x <- read.csv(paste0("~/mods/updown2/db/",stock,".csv"),stringsAsFactors=F)
	x$date <- as.character(as.Date(x$Date,"%m/%d/%y"))
	x <- x[!duplicated(x) & !is.na(x$date),]
	x$sums <- unlist(lapply(x$date,function(y){sum(y== x$date,na.rm=T)}))
	x$PT <- as.double(x$PT)
	x$means <- unlist(lapply(x$date,function(y){mean(x$PT[y== x$date],na.rm=T)}))
	return(x)
}

get_prev_td <- function(date=Sys.Date()){
	date <- as.Date(date)
	if(weekdays(date) =="Monday"){
                date <- date - 3
	} else if(weekdays(date) =="Sunday"){
                date <- date - 2
        }else{
                date <- date - 1
	}
	return(date)

}

lastline <- function(filename) {
  ## filename is of mode character
  out <- system(sprintf("wc -l %s",filename),intern=TRUE)
  n <- as.integer(sub(sprintf("[ ]*([0-9]+)[ ]%s",filename),"\\1",out))
  print(n)
  scan(filename,what="",skip=n-1,nlines=1,sep="\n",quiet=TRUE)
}

#suppressMessages(library(Rcpp))

#cppFunction('NumericVector get_variance(NumericVector x, int y) {
#      //int y = yy[0];
#      int n = x.size();
#      NumericVector arr(n);
#      double sum;
#      double mean;
#      /*double sum_weights = 0;
#      for(int i =0; i<y; i++ ){
#      sum_weights += w[i];
#      }
#      for(int i =0; i<y; i++ ){
#      w[i] = (w[i]*y)/sum_weights;
#      }*/
#      for(int i = (y+1); i < n; i++) {
#      for(int j = i-y; j < i; j++){
##      sum = sum+(x[j]);
#      }
#      mean = sum/y;
#      sum = 0;
#      for(int j = i-y; j < i; j++){
#      sum = sum+((mean-x[j])*(mean-x[j]));
#      }   
#      arr[i] = pow(sum/(y-1), 0.5);
#      sum =0; 
#      }   
#      return arr;
#      }') 

download_morning <- function(sym){
	system(paste("python ~/mods/stocks_morning.py --list", paste(sym,collapse=" ")))
}

get_morning <- function(sym){
	sym <- read.csv(paste0("~/mods/morning/",sym,".csv"))
	colnames(sym) <- c("time", "high", "low", "open", "close", "volume")	
	sym$date <- substr(as.character(sym$time),1,10)
	sym$time <- substr(as.character(sym$time),12,19)
	return(sym)
}

get_earnings <- function(df,f){
        f <- tolower(f)
        system(paste0("table2csv --nth=1 https://www.nasdaq.com/earnings/report/",f," > dump.txt"))
        df2 <- read.csv("dump.txt",sep="|")
        df2$date <- as.Date(as.character(df2$datereported),"%m/%d/%Y")
        df$earnings <- df$date %in% as.character(df2$date)
        return(df)
}

clean_standard_daily_rand <- function(name){
    tryCatch({ stock <- read.csv(paste("~/mods/stocks/",name, ".csv", sep=""), header = FALSE, nrows=1)}, error = function(e){ 
		(system(paste0("python ~/mods/stocks_daily.py --list ", name)))
		})
    stock <- read.csv(paste("~/mods/stocks/",name, ".csv", sep=""), header=FALSE)
    stock$V8 <- NULL
    stock$V7 <- NULL
    colnames(stock) <- c("time", "high", "low", "open", "close", "volume")
    stock$open <- rnorm(n=nrow(stock), mean=20, sd=0.1)
    stock$close <- stock$open+rnorm(n=nrow(stock), mean=0, sd=0.1)
    stock$high <- rnorm(n=nrow(stock), mean=20, sd=0.01)
    stock$low <- rnorm(n=nrow(stock), mean=20, sd=0.1)
    stock$change <- stock$close/stock$open
    stock$range <- stock$high/stock$low
    stock <- split_time(stock)
    stock$time <- NULL
    #stock <- get_earnings(stock,name)
    return(stock)
    
}

clean_standard_daily <- function(name){
    name <- toupper(name)
    f <- paste("~/mods/stocks/",name, ".csv", sep="")
    if(!file.exists(f)){
	download(name)
    } else if(as.POSIXlt.POSIXct(Sys.time()-86400) > file.info(f)$mtime){	
	download(name)
    }
    stock <- read.csv(f, header=FALSE)
    stock$V8 <- NULL
    stock$V7 <- NULL
    colnames(stock) <- c("time", "high", "low", "open", "close", "volume")
    stock$change <- stock$close/stock$open
    stock$range <- stock$high/stock$low
    stock <- split_time(stock)
    stock$time <- NULL
    return(stock)
}

earnings_helper <- function(x){
	tryCatch({
	 y <- x[2]
	 x <- x[1]
	 x <- as.numeric(x)
	if(y=="million"){ 
		x <- x * 10^6
	} 
	if(y=="billion"){
		x <- x * 10^9
	}
	if(y=="trillion"){
		x <- x * 10^12
	}
	return(x)
	},error=function(e) {return(0)})
}

clean_earnings <- function(sym){
	tryCatch({
		    name <- toupper(sym)
		    f <- paste("~/mods/earnings/db/",name, ".csv", sep="")
		  #  if(!file.exists(f)){
			#download_earnings(name)
		  #  } else if(as.POSIXlt.POSIXct(Sys.time()-86400) > file.info(f)$mtime){	
			#download_earnings(name)
		  #  }
		  #if("Acutal.EPS" %in% colnames(df2))
		  #  v <- df2$Actual.EPS
		  #else
		  #v <- df2$Reported.EPS
		df2 <- read.csv(f,stringsAsFactors=F)
		df2 <- df2[2:nrow(df2),]
		#df2$Reported.EPS <- gsub("\\)","",gsub("\\(\\$","\\$-",v))
		#df2$Consensus.Estimate <- gsub("\\)","",gsub("\\(\\$","\\$-",df2$Consensus.Estimate))
		#df2$EPS <- as.numeric(substr(df2$Reported.EPS,2,nchar(df2$Reported.EPS)))
		#df2$Con_EPS <- as.numeric(substr(df2$Consensus.Estimate,2,nchar(df2$Consensus.Estimate)))
		#df2$Rev <- unlist(lapply(strsplit((substr(df2$Actual.Revenue,2,nchar(df2$Actual.Revenue)))," "),earnings_helper))
		#df2$Con_Rev <- unlist(lapply(strsplit((substr(df2$Revenue.Estimate,2,nchar(df2$Revenue.Estimate)))," "),earnings_helper))
		df2$date <- as.character(as.Date(df2$Date,"%m/%d/%Y"))
		return(df2[,c("date")])												
	},error=function(e){return(data.frame())})
}

clean_free_cash_flow <- function(sym){
	df <- read.csv(paste0("~/mods/fcf/db/",sym,".csv"))
	if(suppressWarnings(anyNA(as.numeric(as.character(df[,4])))))
		suppressWarnings(df <- df[1:(which(is.na(as.numeric(as.character(df[,4]))))-1),])
	date <- as.character(df[,1])
	price <- as.numeric(as.character(df[,2]))
	year <- substr(date,1,4)
	fcf <- as.numeric(as.character(df[,4]))
	return(data.frame(date,fcf,price,year))
}

META_DATA <- read.csv("~/mods/meta/meta.csv")

get_industry <- function(sym){
        return(META_DATA$industry[META_DATA$Symbol==toupper(sym)])
}

get_sector <- function(sym){
        return(META_DATA$Sector[META_DATA$Symbol==toupper(sym)])
}

get_name <- function(sym){
        return(gsub("holdings","",gsub("company","",gsub("the","",gsub("\\s$","",gsub("limited","",gsub("ltd","",gsub("corp","",gsub("corporation","",gsub("inc","",tolower(gsub("[^A-Za-z ]*","",META_DATA$Name[META_DATA$Symbol==toupper(sym)]))))))))))))
}

clean_googles <- function(sym){
 	tryCatch({
	    name <- toupper(sym)
	    f <- paste("~/mods/google_searches/db/",name, ".csv", sep="")
	    if(!file.exists(f)){
		download_google(name)
	    } else if(as.POSIXlt.POSIXct(Sys.time()-86400) > file.info(f)$mtime){	
		download_google(name)
	    }
	df2 <- read.csv(f,stringsAsFactors=F)
	return(df2)
	},error=function(e){return(data.frame())})
}

get_day_vec <- function(x,y){
	z <- unlist(lapply(x, function(a){ max(y[(which(a > y))])}))
	return(z)
}

bind_googles <- function(stock,googles){
	stock$date2 <- get_day_vec(stock$date,googles$date)
	googles$isPartial <- NULL
	colnames(googles) <- c("date2","googles")
	df <- merge(stock,googles,by="date2")
	df$date2 <- NULL
	return(df)
}

bind_earnings <- function(stock,earnings){
	stock <- stock[order(stock$date),]
	earnings <- earnings[order(earnings$date),]
	stock$earnings_date <- as.numeric(stock$date %in% earnings$date)
	#stock$eps <- 0
	#stock$con_eps <- 0
	#stock$con_rev <- 0
	#stock$rev <- 0
  #stock$eps[stock$earnings_date==1] <- earnings$EPS
	#stock$con_eps[stock$earnings_date==1] <- earnings$Con_EPS
	#stock$con_rev[stock$earnings_date==1] <- earnings$Con_Rev
	#stock$rev[stock$earnings_date==1] <- earnings$Rev
	return(stock)
}

bind_ud <- function(stock,ud_df){
    ud_df <- ud_df[ud_df$means!=0 & !is.na(ud_df$means),]
	stock$date <- as.character(stock$date)
	ud_df$date <- as.character(ud_df$date)
	stock <- stock[order(stock$date),]
	ud_df <- ud_df[order(ud_df$date),]
    ud_df$v2 <- ud_df$means/cummean(ud_df$means)
	stock$ud_date <- as.numeric(stock$date %in% ud_df$date)
	stock$pt_mean <- 0
    ud_df$fm <- ud_df$Research.Firm
    stock$fm <- ""
    stock$fm[stock$ud_date==1] <- ud_df$fm[ud_df$sums==1 | (c(F,ud_df$sums[1:(nrow(ud_df)-1)]!=1 & ud_df$sums[2:(nrow(ud_df))]!= 1 & ud_df$sums[1:(nrow(ud_df)-1)]==ud_df$sums[2:(nrow(ud_df))]))]
	ud_df <- ud_df[!duplicated(ud_df$date),]
	stock$pt_mean[stock$ud_date==1] <- ud_df$means
	stock$v2[stock$ud_date==1] <- ud_df$v2
	return(stock)		
}

#library(dplyr)

get_avg_googles <- function(df){
	df$googles_avg <- cummean(df$googles)
	return(df)
}

rm_na <- function(df){
	j <- ncol(df)
	i <- 1
	while(i <= j){
		if(anyNA(df[,i])){
			df[,i] <- NULL
			i <- i - 1
			j <- j -1
		}
		i <- i+1
	}
	return(df)
}

m1 <- function(df){
	df <- df[1:(nrow(df)-1),]
	return(df)
}

p1 <- function(df){
	df <- df[2:(nrow(df)),]
	return(df)
}

set_null <- function(stock, date=T){
	stock$time <- NULL
	stock$high <- NULL
	stock$low <- NULL
	stock$open <- NULL
	stock$close <- NULL
	stock$volume <- NULL
	if(date){
	if("date" %in% colnames(stock))
		stock$date <- NULL
	}
	return(stock)
}

split_time <- function(stock){
	ls <- strsplit(as.character(stock$time)," ")
	ls <- as.data.frame(t(as.data.frame(ls)))
	row.names(ls) <- NULL
	stock$date <- as.character(ls[,1])
	stock$time <- as.character(ls[,2])
	return(stock)
}

time_int <- function(time){
	time <- paste0(strsplit(time,":")[[1]], collapse="")
	return(time)
}	

part_on_time <- function(stock, time){
	u <- unique(stock$date)
	times <- as.numeric(sapply(stock$time, time_int))
	stock$time <- times
	time <- as.numeric(time_int(time))
	e <- numeric(length(u))
	df2 <- data.frame(u,e,e,e,e,e,e,e,e)
	colnames(df2) <- c("date","high_m","low_m","open_m","close_m","high_a","low_a","open_a","close_a")
	for(d in u){
		if((T %in% ((stock$date==d) & (stock$time <= time))) & (T %in% ((stock$date==d) & (stock$time > time)))){
		df <- stock[(stock$date==d) & (stock$time <= time),]
		high_m <- max(df$high)
		low_m <- min(df$low)
		open_m <- df$open[which.min(df$time)]
		close_m <- df$close[which.max(df$time)]
		
		df <- stock[(stock$date==d) & (stock$time > time),]
		high_a <- max(df$high)
		low_a <- min(df$low)
		open_a <- df$open[which.min(df$time)]
		close_a <- df$close[which.max(df$time)]
		df2[df2$date==d,2:9] <- c(high_m,low_m,open_m,close_m,high_a,close_a,open_a,close_a) 	}
	}
	df2$change_m <- df2$close_m/df2$open_m
	df2$change_a <- df2$close_a/df2$open_a
	df2$range_a <- df2$high_a/df2$low_a
	df2$range_m <- df2$high_m/df2$low_m
	return(df2)
}

set_null2 <- function(stock){
	stock$open_m <- NULL
	stock$close_m <- NULL
	stock$low_m <- NULL
	stock$high_m <- NULL
	stock$open_a <- NULL
	stock$close_a <- NULL
	stock$low_a <- NULL
	stock$high_a <- NULL
	stock$date <- NULL
	return(stock)
}

download_min <- function(sym,time){
		st <- paste("python ~/mods/stocks_min.py --list", as.character(paste(sym,collapse=" ")), "--minutes",as.character(time))
		system(st)
}

get_min_today <- function(sym, time, fetch=TRUE){
	if(fetch){
		st <- paste("python ~/mods/stocks_min_today.py --list", as.character(sym), "--minutes",as.character(time))
		system(st)
	}
	stock <- read.csv(paste0("~/mods/stocks_min_today/",sym,".csv"), header=FALSE)
	colnames(stock) <- c("time","high","low","open","close","vol1","vol2","nas")
	stock$nas <- NULL
	stock <- split_time(stock)
	return(stock)
}

get_min <- function(sym, time, fetch=TRUE){
	if(fetch){
		st <- paste("python ~/mods/stocks_min.py --list", as.character(sym), "--minutes",as.character(time))
		system(st)
	}
	stock <- read.csv(paste0("~/mods/stocks_min/",sym,".csv"), header=FALSE)
	colnames(stock) <- c("time","high","low","open","close","vol1","vol2","nas")
	stock$nas <- NULL
	stock <- split_time(stock)
	return(stock)
}

clean_data <- function(name, var=90){
    stock <- clean_standard_daily(name)
    stock$olow <- stock$open/stock$low
    stock$ohigh <- stock$open/stock$high
    stock$chigh <- stock$close/stock$high
    stock$clow <- stock$close/stock$low
    stock$change_rar <- (stock$change-1)/stock$range
    stock$range_var <- get_variance(stock$range, var)
    stock$change_var <- get_variance(stock$change, var)
    stock$volume_var <- get_variance(stock$volume, var)
    stock$cr_var <- get_variance(stock$change_rar, var)
    stock <- stock[101:nrow(stock),]
    stock$norm_range <- (stock$range-mean(stock$range))/stock$range_var
    stock$norm_change <- (stock$change-1)/stock$change_var
    stock$norm_cr <- (stock$change_rar)/stock$cr_var
    stock$norm_vol <- (stock$volume-mean(stock$volume))/stock$volume_var
    change <- stock$change[2:nrow(stock)]
    prev_norm_cge <- stock$norm_change[1:(nrow(stock)-1)]
    prev_norm_rge <- stock$norm_range[1:(nrow(stock)-1)]
    prev_norm_vol <- stock$norm_vol[1:(nrow(stock)-1)]

    return(stock)
}


# @returns - dataframe with the ratio of the coulmn from num1 rows to num2 rows
# @param stock - dataframe
# @param column - column of dataframe
# @param num1 - number of rows to go back for top of ratio
# @param num2 - number of rows to go back
# ex - get_data_prev2(stock, "close", 5, 1) gets the close from 5 days ago divided by close from 1 day ago

get_data_prev2 <- function(stock, column, num1, num2){
    stock[, paste(column,num1,sep="x")] <- c(rep(0,num1),stock[1:((nrow(stock)-num1)),column])
    stock[, paste(column,num2,sep="x")] <- c(rep(0,num2),stock[1:((nrow(stock)-num2)),column])
    stock[, paste(column,num2,sep="_prev_")] <- stock[, paste(column,num1,sep="x")]/stock[, paste(column,num2,sep="x")]
    stock[, paste(column,num1,sep="x")] <- NULL
    stock[, paste(column,num2,sep="x")] <- NULL
    return(stock)
}

# @returns - dataframe with the ratio of the coulmn from num1 rows to num2 rows
# @param stock - dataframe
# @param column - column of dataframe
# @param nums - vector of rows to go back for ratios
# ex - get_data_prev_bars(stock, "close", c(1,2,3,4,5)) gets the close from 1 days ago divided by close from 2 day ago, 2 divided by 3 .. etc

get_data_prev_bars <- function(stock, column, nums){
    for(i in 1:(length(nums)-1)){
      j <- nums[i]
      k <- nums[i+1]
      stock <- get_data_prev2(stock, column, j, k)
    }
    return(stock)
}

row_avg <- function(mat, rmat){
	trades <- c()
	for(i in 1:nrow(mat)){
		t <- c()
		for(j in 1:ncol(mat)){
			if(mat[i,j] ==1)
				t <- c(t, rmat[i,j])	
		}
		if(length(t) > 5)
			trades <- c(trades, mean(t))
	}
	return(trades)
}

# @returns - dataframe with previous "num" columns
# @param stock - dataframe
# @param column - column of dataframe
# @param num - number of rows to go back
# @ex - get_data_prev(stock, "close", 5) gets the close from 1, 2, 3, 4, and 5 days ago and makes these columns in the df

get_data_prev <- function(stock, column, num){
    for(i in 1:num){
      stock[, paste(column,i,sep="_prev_")] <- c(rep(0,i),stock[1:((nrow(stock)-i)),column])
    }
    return(stock)
}

# @returns - dataframe with previous "num" columns greater or less than center
# @param stock - dataframe
# @param column - column of dataframe
# @param num - number of rows to go back
# @param center - thershold to compare numbers
# @ex - get_data_prev_bool(stock, "close", 5, 1) bools of wether the close from 1, 2, 3, 4, and 5 days ago is greater than 1

get_data_prev_bool <- function(stock, column, num, center=1){
    for(i in 1:num){
      stock[, paste(column,i,sep="_prev_")] <- ifelse(c(rep(0,i),stock[1:((nrow(stock)-i)),column])>center,1,-1)
    }
    return(stock)
}

# @returns - dataframe with sum of previous "num" columns greater or less than center
# @param stock - dataframe
# @param column - column of dataframe
# @param num - number of rows to go back
# @param center - thershold to compare numbers
# @ex - get_data_prev_bool(stock, "close", 5, 1) sum of bools of wether the close from 1, 2, 3, 4, and 5 days ago is greater than 1

get_data_prev_bool_sum <- function(stock, column, num, center){
    new_mat <- matrix(nrow=nrow(stock),ncol=num)
    for(i in 1:num){
      new_mat[,i] <- ifelse(c(rep(0,i),stock[1:((nrow(stock)-i)),column])>1,1,0)
    }
    sums <- apply(new_mat, 1, sum)
    stock[,paste("sums",i,sep="_")] <- sums
    return(stock)
}


# @returns - dataframe with 0/1 clusters
# @ param data - dataframe/matrix
# @ param centers - number of clusters

get_clusters <- function(data, centers){
    vect <- kmeans(x=data[,2:ncol(data)], centers=centers)
    for(i in 2:centers){
      data[,paste("cluster_",i, sep="")] <- ifelse(vect$cluster==i,1,0)
    }
    return(data)
}

# @returns - normalized dataframe
# @ param data - dataframe/matrix

normalize <- function(data){
    for(i in 1:ncol(data)){
      data[,i] <- (data[,i] - mean(data[,i]))/sd(data[,i])
    }
    return(data)
}

# @returns - normalized dataframe
# @ param data - dataframe/matrix

normalize2 <- function(data, weight){
    for(i in 1:ncol(data)){
	n <- colnames(data)[i]
	num <- as.numeric(gsub("\\D", "\\1", n))
	if(is.na(num)){
      		data[,i] <- (data[,i] - mean(data[,i]))/sd(data[,i])
	} else { 
		data[,i] <- ((data[,i] - mean(data[,i]))/(sd(data[,i])*(num * weight)))
	}
    }   
    return(data)
}

dummy <- function(df, col){
	u <- unique(df[,col])
	for(i in 2:length(u)){
		df[,paste0(col,u[i])] <- ifelse(df[,col]==u[i],1,0)
	}
	df[,col] <- NULL
	return(df)
}

