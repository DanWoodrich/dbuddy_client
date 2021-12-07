digital_root<-function(n){
  n = strsplit(n, "")[[1]]

  f = sum(sapply(n,as.integer))
  if(f < 10){
    return(f)
  }else{
    return(digital_root(as.character(f)))
  }
  
}

#simple hash function that is tolerant of letters and numbers (removes special characters). Probably good enough to prevent any collisions with our
#internal usage
str2hash <- function(x,hashlen) {
  x1= gsub("[^[:alnum:]]", "", x) #just throw these out, too hard otherwise
  x2 <- strsplit(x1, "")[[1]]
  x2l<-match(x2, letters) +314#add a 333 to this so that letters to encourage triggering of window walkdown and prevent errors with single replacements
  x2l<-x2l[!is.na(x2l)]
  x2num <-suppressWarnings(as.integer(x2))
  x2num<-x2num[!is.na(x2num)]
  
  x3 = c(x2num,x2l)
  
  x3 = paste(x3,collapse="")
  x3 <- strsplit(x3, "") 
  x3 <- unlist(x3) 
  x3 <- as.vector(as.numeric(x3)) 
  
  if(length(x3)<hashlen){
    while(length(x3)<hashlen){
      x3 = rep(x3,2)
    }
    
    x3 = x3[1:hashlen]
  
  }else if(length(x3)>hashlen){
    while(length(x3)>hashlen){
      newx3 = vector(length=(length(x3)-1))
      for(i in 1:(length(x3)-1)){
        newx3[i]<-(x3[i]-x3[i+1])/2
      }
      x3 = newx3
    }
    
    x3 = as.character(x3)
    x3= gsub("[^[:alnum:]]", "", x3)
    x3 = as.numeric(sapply(x3,digital_root))
    
  }else{
    #do nothing 
  }
  
  #depending on the length of string, pad by repetition or 
  return(paste(x3,collapse=""))
}

args<-commandArgs(trailingOnly = TRUE)
hashlen = 8 

ip = gsub(".*? ([[:digit:]])", "\\1", system("ipconfig", intern=T)[grep("IPv4", system("ipconfig", intern = T))])

#get an ID for the request (hash the args and IP)
ipID = str2hash(paste(ip,collapse=""),hashlen)
argID = str2hash(paste(args,collapse=""),hashlen)

#write a file with this request id as the title, and with the request in the body. 

request_file = paste(Sys.getenv("COMPATH"),"/requests/request_argID_",argID,"_IPID_",ipID,".txt",sep="")

if(args[1]=="insert"|args[1]=="modify"|args[1]=="delete"){
  
  print(args[3])
  
  #means DML. So, copy the local file onto the NAS in /transfer by default
  NASpath = paste(Sys.getenv("COMPATH"),"/transfer/",basename(args[3]),sep="")
  
  print(NASpath)
  file.copy(args[3],NASpath)
  
  args[3]= NASpath
  
  didDML = TRUE
  
}else{
  didDML=FALSE
}

print(paste("request ID",argID,"written to",Sys.getenv("COMPATH")))
writeLines(paste(args,collapse=" "), request_file)

#now, wait for request file and inform user of waiting:

#standardwait(request_file,TRUE)



if(args[1]=='pull'){
  
  outfile = args[3]
  
  comtype = "pull"
  
}else{

  #this means it's dml, so output will be specified by --log if desired: 
  if(any(grepl('--log',args))){
    outfile = args[which(args=="--log")+1]
  }else if(any(grepl('--out',args))){
    outfile = args[which(args=="--out")+1]
  }else{
    outfile = "PRINT_TO_CONSOLE"
  }
  
  comtype = "DML/other"
  
}




outdir = paste(Sys.getenv("COMPATH"),"/output",sep="")

nas_outfile = paste("request_argID_",argID,"_IPID_",ipID,sep="")#don't specify extension- could be txt if pull fails
nas_outfile_fp = paste(Sys.getenv("COMPATH"),"/output/",nas_outfile,sep="") 

nas_outfile_fp1 = paste(nas_outfile_fp,".txt",sep="")
nas_outfile_fp2 = paste(nas_outfile_fp,".csv",sep="")

print('response submitted!')
s=0.5
exists = c(FALSE,FALSE)
NOTFOUND=TRUE
while(!any(exists)){
  
  if(s%%2==0 & s<20){
    print(paste("waiting for response: ",s,"s",sep=""))
  }else if(s%%5==0 & s<100){
    print(paste("waiting for response: ",s,"s",sep=""))
  }else if(s%%25==0){
    print(paste("waiting for response: ",s,"s",sep=""))
  }
  
  if(s%%5==0&NOTFOUND){
    if(file.exists(request_file)){
      print("request not been recieved by server")
    }else{
      print("request has been recieved by server")
      NOTFOUND = FALSE
    }
  }
  
  s = s+0.25
  Sys.sleep(0.25)
  
  exists[1]<-file.exists(nas_outfile_fp1)
  exists[2]<-file.exists(nas_outfile_fp2)
  
}

print("output returned!")

nas_outfile_fp = c(nas_outfile_fp1,nas_outfile_fp2)[exists]
#print(nas_outfile_fp)

#only copy file to destination if not an error. Want to 

if(grepl(".txt",nas_outfile_fp)){
  #this mean that the pull was in error, and a message was spit out- don't save the error message as a correct output, but instead print out
  
  
  outputtext = readLines(nas_outfile_fp)
  
  if(outfile!="PRINT_TO_CONSOLE"){
    
    outputtextstr = paste(outputtext,collapse=" ")
    
    if(grepl("Error",outputtextstr) & grepl("Execution halted",outputtextstr)){ #resonable to assume that the output represents an error
      
      outfile = "PRINT_TO_CONSOLE" #don't copy error, so INSTINCT won't take it to mean completion. 
      
    }else{
      #assume not an error, so save as 'receipt' 
      invisible(file.copy(nas_outfile_fp,outfile, overwrite = TRUE))
    }
    
  }
  
  if(comtype=="pull" | outfile=="PRINT_TO_CONSOLE"){
    print("---begin server message---")
    print(outputtext)
    print("---end server message---")
  }
  
}else{
  invisible(file.copy(nas_outfile_fp,outfile, overwrite = TRUE))
}

invisible(file.remove(nas_outfile_fp))

if(didDML){
  
  invisible(file.remove(NASpath))
  
}

