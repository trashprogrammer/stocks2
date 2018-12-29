download <- function(syms){
	system(paste0("python ~/mods/stocks_daily.py --list ", paste(syms, collapse=" ")))
}

download2 <- function(syms){
	for(s in syms){
		system(paste0("python ~/crypto.py ",s," 2013-01-01 2018-12-31 > ~/cryptos/",s,".csv"))
	}
}
