library(stringr)
clean_options <- function(x){
    files <- list.files(paste0("~/mods/options/",x,"/"))
    for(f in files){
        df <- read.csv(paste0("~/mods/options/",x,"/",f),header=F)
        month <- str_extract(str_extract(f,"[0-9]+[A-Z][0-9]+"),"[A-Z]")
        price <- str_extract(str_extract(f,"[A-Z][0-9]+\\.csv"),"[0-9]+")
        if(month > "M"){
            cp <- "_put_"
        } else{
            cp <- "_call_"
        }
        n <- c("time","open","low","high","close","volume","open_int","na")
        n <- paste0(n,paste0(cp,price))
        colnames(df) <- n
        df[,ncol(df)] <- NULL
        colnames(df)[1] <- "time"
        df <- split_time(df)
        df$time <- NULL
        if(exists("fin_df")){
            fin_df <- merge(fin_df,df,by="date",all=T)
        } else{
            fin_df <- df
        }
    }
    return(fin_df)
}
