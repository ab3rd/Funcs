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