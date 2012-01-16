###############################################################################
###############################################################################

#------------------------------------------------------------------------------
#This function is used to decide which folder to use.
ALEX_HOME = function(fileName) {
	kore.address = "C:/Users/azhang/Dropbox/Working"
	if (file.exists(kore.address)) {
		return ( file.path(kore.address, fileName) )
	} else {
		return ( file.path("C:/Users/Alex/Dropbox/Working", fileName) )
	}
}

#Using ALEX_HOME to avoid the trouble of having to change addresses.
ALEX_SOURCE <- function(fileName) {
	source(ALEX_HOME(fileName))
	return (sprintf("ALEX_SOURCE call ended at %s", Sys.time()))
}

#------------------------------------------------------------------------------
YYYYMMDD <- function(d) {
#d is assumed to be a Date
	p = as.POSIXlt(d)
	return (toString(as.numeric(1900 + p$year)*10000 + (p$mon+1)*100 + p$mday))
}

ExcelDate <- function(d) {
#d is assumed to be a Date	
#stupid Excel believe 2/29/1900 existed...
if (d > as.Date("1900-02-28")) {
	return (as.numeric(d - as.Date("1900-01-01"))+2)
} else
{
	return (as.numeric(d - as.Date("1900-01-01"))+1)
}

}

IfNAThen <- function(x, thenVal) {
#If x is NA then replace with a given value
	if (is.na(x)) {
		return (thenVal)
	} else {
		return (x)
	}
}

YYYY <- function(d) {
	return (substr(YYYYMMDD(d), 1, 4))
}

MM <- function(d) {
	return (substr(YYYYMMDD(d), 5, 6))
}

DD <- function(d) {
	return (substr(YYYYMMDD(d), 7, 8))
}

#------------------------------------------------------------------------------

AddSQ <- function(input) {
#convert stuff to string and add single quote to two sides.
	return (paste("'", toString(input), "'", sep = ""))
}

GenEmptyDataFrameWNames <- function(col.names, size=0) {

	#create matrix with NA. Note we need to treate size = 0 differently.
	result = data.frame(matrix(NA, nrow = max(1, size), ncol = length(col.names)))
	names(result) = col.names

	if (size == 0) {
		#We need to get rid of the first row if the wanted nrow = 0.
		result = result[-1,]
	}
	
	return (result)
}

#------------------------------------------------------------------------------

GenEmptyDataFrameWNames <- function(col.names, size=0) {

	#create matrix with NA. Note we need to treate size = 0 differently.
	result = data.frame(matrix(NA, nrow = max(1, size), ncol = length(col.names)))
	names(result) = col.names

	if (size == 0) {
		#We need to get rid of the first row if the wanted nrow = 0.
		result = result[-1,]
	}
	
	return (result)
}

#------------------------------------------------------------------------------


Append2Table <- function(new.data, tableName, conn) {
#new data is supposed to be a data frame whose column names responds to the fields that
#you want to insert into table. All fields are assumed to be strings.

	for (i in (1:nrow(new.data))) {
	
		upload.str =  paste("insert into ", tableName, "(",  sep = "")
		
		for (j in (1:ncol(new.data))) {
			upload.str = sprintf("%s %s, ", upload.str, colnames(new.data)[j])
		}
		
		upload.str = substr(upload.str, 1, nchar(upload.str)-2)
		upload.str = paste(upload.str, ") values (", sep = "")
		
		for (j in (1:ncol(new.data))) {
			upload.str = sprintf("%s '%s', ", upload.str, download.data[i, j])
		}
		
		upload.str = substr(upload.str, 1, nchar(upload.str)-2)
		upload.str = paste(upload.str, ")", sep = "")
		
		print(upload.str)
		sqlQuery(channel = conn, query = upload.str)
		
	}
	
	return (nrow(new.data))

}

#------------------------------------------------------------------------------

#This function is just for processing Pi Trading's hour and minute which are combined together.
#For example, if 7 will mean hour 0, minute 7. 107 will mean hour 1, minute 7 while 117 will mean
#hour 1 minute 17. 1117 will mean hour 11 minute 17.

GetPiTradingHourNum <- function(hm) {
	hm = as.numeric(hm)
	
	if ((hm %% 100) >= 60) { #if this happens, there is something seriously wrong with the input
		return (NA)
	} else {
		return (floor(hm/100))
	}
}

#------------------------------------------------------------------------------

GetPiTradingMinuteNum <- function(hm) {
	hm = as.numeric(hm)
	
	if ((hm %% 100) >= 60) { #if this happens, there is something seriously wrong with the input
		return (NA)
	} else {
		return (hm %% 100)
	}
}

#------------------------------------------------------------------------------

#This function calculates the PnL and the cum PnL.
Calc.PnL <- function(p.p, price.col.name, position.col.name) {
#p.p is assumed to be a data.frame that contains price and position info.
#price.col.name tells you the column name of the price column where position.col.name tells
#you about the position column.
	p.p$PnL = c(0, diff(p.p[, price.col.name], lag = 1)) * c(0, p.p[(2:nrow(p.p)), position.col.name])
	p.p$Cum.PnL = cumsum(p.p$PnL)
	
	return (p.p)
}

#------------------------------------------------------------------------------

#This function calculates the PnL w carry.
Calc.FX.PnL <- function(p.p, price.col.name, position.col.name, ir.base.name, ir.quote.name) {
#p.p is assumed to be a data.frame that contains price and position info.
#price.col.name tells you the column name of the price column where position.col.name tells
#you about the position column.

	#browser()
	
	#this is only the pricinpal part.
	price = p.p[, price.col.name]
	position = p.p[, position.col.name]
	ir.base = p.p[, ir.base.name]
	ir.quote = p.p[, ir.quote.name]
	
	p.p$PnL = c(0, diff(price, lag = 1)) * c(0, position[(2:nrow(p.p))])
	#Carry Part
	p.p$PnL = p.p$PnL + (c(0, ir.base[2:nrow(p.p)])* price /36500 - c(0, ir.quote[2:nrow(p.p)]* price[2:nrow(p.p)])/36500) * c(0, position[(2:nrow(p.p))])
	p.p$Cum.PnL = cumsum(p.p$PnL)
	
	return (p.p)
}

#------------------------------------------------------------------------------

InfoRatio <- function(pnl) {
	return (mean(pnl)/sd(pnl))
}


#------------------------------------------------------------------------------

ChangeDataFrameNameAccording2Hash <- function(d.f, hash.old2new) {

	#Here d.f is a data.frame
	#hash.old2new is a hash object that maps old names 2 new names
	for (thisOldName in keys(hash.old2new)) {
		names(d.f)[names(d.f) == thisOldName] = hash.old2new[[thisOldName]]
	}
	
	return(d.f)
}