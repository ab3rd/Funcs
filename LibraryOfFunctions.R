###############################################################################
###############################################################################

#------------------------------------------------------------------------------
ALEX_HOME = function(fileName) {

  #This function helps to solve the problem of having different dropbox and google drive
  #addresses at home and at work by dynamically testing if the corresponding folders exist.
  
  work.gfolder = "C:/Users/azhang/Google Drive/New Work"
  home.gfolder = "C:/Users/Alex/Dropbox/Working"
  work.dropbox = "C:/Users/azhang/Dropbox/Working"
  home.dropbox = "C:/Users/Alex/Dropbox/Working"
  
  if (file.exists(work.gfolder)) {
    #Now, we know that we are at work.
    if (file.exists(file.path(work.gfolder, fileName))) {
      return (file.path(work.gfolder, fileName))
    } else {
      return (file.path(work.dropbox, fileName))
    }
    
  } else {
    #Now, we know that we are at home.
    if (file.exists(file.path(home.gfolder, fileName))) {
      return (file.path(home.gfolder, fileName))
    } else {
      return (file.path(home.dropbox, fileName))
    }
  }

}

#Using ALEX_HOME to avoid the trouble of having to change addresses.
ALEX_SOURCE <- function(fileName) {
	source(ALEX_HOME(fileName))
	message(sprintf("ALEX_SOURCE call ended at %s", Sys.time()))
}

#------------------------------------------------------------------------------
YYYYMMDD <- function(d) {
#d is assumed to be a Date
	p = as.POSIXlt(d)
	return (toString(as.numeric(1900 + p$year)*10000 + (p$mon+1)*100 + p$mday))
}

#------------------------------------------------------------------------------

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

#------------------------------------------------------------------------------
POSIXlt.To.ExcelDateTime <- function(p.time) {

	#Here we convert a POSIXlt date time into an Excel style number
	date.part = ExcelDate(as.Date(p.time))
	time.part = p.time$hour/24 + (p.time$min/60)/24 + ((p.time$sec/60)/60)/24

	return (date.part + time.part)
}

#------------------------------------------------------------------------------

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
Append2Table <- function(new.data, tableName, conn, errors = TRUE) {
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
			upload.str = sprintf("%s '%s', ", upload.str, new.data[i, j])
		}

		upload.str = substr(upload.str, 1, nchar(upload.str)-2)
		upload.str = paste(upload.str, ")", sep = "")
		
		#Try to upload. If an error, print out the SQL and die. Die loudly.
		err <- try(sqlQuery(channel = conn, query = upload.str, errors = errors))
		
		#browser()
		if (ContainsStr(err, "ERROR")) {
			print(upload.str)
			stop("Upload is unsuccessful...")
		} 
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

#------------------------------------------------------------------------------

Alex.uniroot <- function(f, start_interval, growthRate, tol, maxiter) {
#We wrap up the R base function uniroot to provide a little bit more flexibility.
#Here, if the function f gives values of the same sign at both ends of start_interval,
#We will enlarge the start_interval by the growthRate.

#However, we are assuming that the solution and all the intervals are positive. Otherwise,
#our way of growing the interval won't work.

#start_interval is assumed to be of the form c(init.left, init.right)

	currentTry = 0
	
	while (currentTry < maxiter) {
	
		err <- try( this.result <- uniroot(f, start_interval, tol= tol, maxiter = maxiter - currentTry), silent = TRUE )

		if (class(err) == "try-error") { 
                start_interval = c(start_interval[1]/growthRate, start_interval[2]*growthRate)
        } else { 
                return(this.result)	
        }
		
		currentTry = currentTry + 1 #We are counting a failed try as an iteration as well.

	}
	#if you ever run into this line, you are doomed!
	return (NA)

}

#------------------------------------------------------------------------------

ContainsStr <- function(fullStr, lookfor, ignore.case = TRUE) {
	return (!length(grep(paste(".*", lookfor, ".*", sep=""), x = fullStr, ignore.case = ignore.case))==0)
}

#------------------------------------------------------------------------------

cls <- function() {
       require(rcom)
       wsh <- comCreateObject("Wscript.Shell")
       comInvoke(wsh, "SendKeys", "\014")
       invisible(wsh)
}


#------------------------------------------------------------------------------

Z.trim <- function(input) {
  #We aim to replicate the VBA function trim which removes spaces in the front
  #and at the end. ALSO: if there are consecutive spaces in between, we are supposed
  #to let just 1 remain.
  return (gsub(pattern = " {2,}", replacement =" ", x = gsub(pattern = "^ +| +$", replacement = "", x = input)))
  
}

#-------------------------------------------------------------------------------

Z.t2d <- function(tq) {
  
  tq = Z.trim(tq)

  #We need do deal with the case "14-20 1/8". We need to firstly identify.
  #grepl(" [0-9]+/[0-9]+$", x = tq)
  #browser()
  last.digit.pattern = " [0-9]"
  if (grepl(last.digit.pattern, x = tq)) {

  #Now, we are doing a recursion. We evaluate the first part by calling the function itself.
  #The last part is a little bit tricky. If it is just 2, like in "10-13 2", then we are assuming
  #it being equivalent to "10-13 2/8" which is indeed 10 + 13/32 + 2/8*1/32. 

    match.pos = regexpr(pattern = last.digit.pattern, text = tq)
	val.part1 = Z.t2d(substr(x = tq, start = 1, stop = match.pos))
	part2 = substr(x = tq, start = match.pos, stop = nchar(tq))
	val.part2 = ifelse(grepl(pattern ="/[0-9]", x = part2), 1/32, (1/32)*(1/8))*eval(parse(text = part2))
    return ( val.part1 + val.part2 )
  }

  
  #This function converts a tick quote into a decimal quote. e.g. 9-16 -> 9.5
  if(!is.na(suppressWarnings(as.numeric(tq)))) {
    return (as.numeric(tq))
  }
  
  tq.v = strsplit(x= tq, split= "-|:")[[1]]
  #browser()
  
  #the split result cannot be more than 2.
  if (length(tq.v) > 2) {
    stop("When split, %s becomes more than 2 parts!", tq)
  }
  
  #after we split, we must make sure that the first part is numeric and the second part
  #also needs to follow some things.
  if(is.na(suppressWarnings(as.numeric(tq.v[1])))) {
    stop(sprintf("The first part of %s i.e. %s is not numeric", tq, tq.v[1]))
  }
  
  p1 = as.numeric(tq.v[1])

  #Deal with the 2nd part
  p2 = tq.v[2]
  
  if(suppressWarnings(is.na(as.numeric(p2)))) {

    #Deal with the 2nd part being like 12+.
    #browser()
    if (substr(p2, start=nchar(p2), stop=nchar(p2)) == "+") {
      return (p1 + as.numeric(substr(p2, start=1, stop=nchar(p2)-1))/32 + 1/64)
    } else {
      stop(sprintf("Cannot interpret the 2nd part of %s, i.e. %s", tq, p2))
    }
  } else {
    if(nchar(p2) <= 2) {
      return (p1+as.numeric(p2)/32)
    } else {
      #browser()
      temp = as.numeric(substr(x = p2, start = 3, stop = nchar(p2)))
      if (temp >= 8) {
        stop("Having trouble to interpret!")
      } else {
        return (p1+as.numeric(substr(p2, start = 1, stop = 2))/32+temp/256)
      }
    }
  }

}
  
Z.delchars <- function(str, n, lead = TRUE){
#delete certain number of chars.
  dots = paste(rep('.',n),collapse='') 
  pat = ifelse(lead, paste('^',dots,sep=''), paste(dots,'$',sep='')) 
  sub(pat,'',str) 
}

Z.left <- function(s, n) {
#mimicking the VBA left function.
#yeah, I know, it is a shame to mimick VBA but whatever...
  stopifnot(n >= 0)
  ifelse(n == 0, "", substr(s, 1, min(nchar(s), n)))
  
}

Z.right <- function(s, n) {
  #mimicking the VBA right function.
  #yeah, I know, it is a shame to mimick VBA but whatever...
  stopifnot(n >= 0)
  ifelse(n == 0, "", substr(s, max(1, nchar(s) - n + 1), nchar(s)))
  
}