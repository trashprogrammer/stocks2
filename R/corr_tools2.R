#library(ggplot2)
#library(ggcorrplot)


#function to parse arguements
get_args <- function(args, sub){
        for(i in args){
                if(grepl(sub, i)){
                        out <- strsplit((strsplit(i, sub)[[1]])[2]," ")[[1]]
                        return(out)
                }
        }
        return (NULL)
}

# -sym: A list of stock symbols separated by spaces
# -days: Number of days - if not specified does the max length of dataset
# -dir_in: Directory to read data from - if not specified choses current directory, if specified please end with a "/" character
# -dir_out: Directory to output pngs - if not specified choses current directory, if specified please end with a "/" character
# -name: a shared column between datasets default name is "change" to plot
# -line, an option flag to display a line through the plots
# -heat, Outputs heatmap of correlation matrix instead

# @returns - png of correlations of shared column or a heatmap


error_msg <- 'Rscript corr_tool.R "-sym stock1 stock2 ..." "-days NUMBER_OF_DAYS" "-dir ~/pictures/" "name target"'
get_corr_tools_plot <- function(args=""){
mx <- FALSE
stock_names <- get_args(args,"-sym ")
days <- get_args(args, "-days ")
name <- get_args(args, "-name ")
dir <- get_args(args, "-dir_out ")
diri <- get_args(args, "-dir_in ")
b_line <- "-line" %in% args
b_heat <- "-heat" %in% args

if(is.null(name)){
        name <- "change"
}
if(is.null(stock_names) | (length(stock_names) < 2)){
        print("Please use as follows")
        print(error_msg)
}
if(is.null(days)){
        mx <- TRUE
} else{
        days <- as.numeric(days) -1
}
if(is.null(dir)){
        dir <- ""
}
if(is.null(diri)){
  diri <- ""
}

get_heat <- function(l, mx, days=100, syms){
  mn_len <- 1000000000000
  if(mx){
    for(i in 1:length(l)){
      if(length(l[[i]]) < mn_len)
        mn_len <- length(l[[i]])-1
    }
  } else{
    mn_len <- days
    for(i in 1:length(l)){
      if(length(l[[i]]) < days)
        mn_len <- length(l[[i]])
    }
  }
  mat <- matrix(nrow=(mn_len+1), ncol=length(syms))
  for(i in 1:length(l)){
    mat[,i] <- (l[[i]])[(length(l[[i]]) - mn_len):length(l[[i]])]
  }
  colnames(mat) <- syms
  mat <- cor(mat)
  ggcorrplot(mat, hc.order = TRUE, lab = TRUE)
  ggsave(paste0(dir,"Heat.png"))
}

if(!b_heat){
temp_days <- FALSE
#loops through list of symbols and makes plots for each pair
for(i in 1:(length(stock_names)-1)){
        tryCatch( {
        sym1 <- stock_names[i]
        data1 <- read.csv(paste0(diri, sym1,".csv"))
        for(j in (i+1):length(stock_names)){
                sym2 <- stock_names[j]
                data2 <- read.csv(paste0(diri, sym2,".csv"))
                #condition to check if the max days was specified
                if(mx){
                        d <- min(c((nrow(data1)-1), (nrow(data2)-1)))
                        targ1 <- data1[(nrow(data1)-d):nrow(data1),name]
                        targ2 <- data2[(nrow(data2)-d):nrow(data2),name]
                        if(b_line)
                          ggplot() + xlab(paste(sym2,name)) + ylab(paste(sym1,name)) + geom_point(aes(targ2, targ1)) + geom_smooth(aes(targ2, targ1))  + ggtitle(paste(sym1, sym2,"   CORR: ",cor(targ2,targ1)))
      else
        ggplot() + xlab(paste(sym2,name)) + ylab(paste(sym1,name)) + geom_point(aes(targ2, targ1)) + ggtitle(paste(sym1, sym2,"   CORR: ",cor(targ2,targ1)))

                        ggsave(paste0(dir,sym1,"_",sym2,".png"))
                } else{
                        if((days > nrow(data1)) |  (days > (nrow(data2)))){
                                temp <- days
                                days <- min(c((nrow(data1)-1), (nrow(data2)-1)))
                                temp_days <- T
                        }
                        targ1 <- data1[(nrow(data1)-days):nrow(data1), name]
                        targ2 <- data2[(nrow(data2)-days):nrow(data2), name]
                        if(b_line)
                          ggplot() + xlab(paste(sym2,name)) + ylab(paste(sym1,name)) + geom_point(aes(targ2, targ1)) + geom_smooth(aes(targ2, targ1)) + ggtitle(paste(sym1, sym2,"   CORR: ",cor(targ2,targ1)))
                        else
                          ggplot() + xlab(paste(sym2,name)) + ylab(paste(sym1,name)) + geom_point(aes(targ2, targ1)) + ggtitle(paste(sym1, sym2,"   CORR: ",cor(targ2,targ1)))

                        ggsave(paste0(dir,sym1,"_",sym2,".png"))
                        if(temp_days){
                           #condition if days specified was larger than a dataset given
                                days <- temp
                                temp_days <- FALSE
                        }

                }
        }
        }, error = function(){print("Error occured please run as follows")
                                print(error_msg)})
}

} else{
  streams <- list()
  for(i in 1:(length(stock_names))){
      sym1 <- stock_names[i]
      data1 <- read.csv(paste0(diri, sym1,".csv"))
      streams[[i]] <- data1[,name]
  }
  if(mx){
    get_heat(streams, mx, days=100, stock_names)
  } else{
    get_heat(streams, FALSE, days, stock_names)
  }
}
}
