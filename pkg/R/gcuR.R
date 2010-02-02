#Alternate titles: friendlyR

cat("friendlyR enhancements loaded -- beta v1-2010 01/27/10\n");

defsep="\t"

myvar=function(x,na.rm)
{ 
   ss=sum(x^2,na.rm=T);
   n=length(x[!is.na(x)]);
   ss=ss-n*mean(x)^2;
   return(ss/(n-1));
}

mysd=function(x,na.rm)
{
   return(sqrt(myvar(x)));
}

xtable=function (..., exclude = c(NA, NaN), dnn = list.names(...), deparse.level = 1) 
{
    list.names <- function(...) {
        l <- as.list(substitute(list(...)))[-1]
        nm <- names(l)
        fixup <- if (is.null(nm)) 
            seq_along(l)
        else nm == ""
        dep <- sapply(l[fixup], function(x) switch(deparse.level + 
            1, "", if (is.symbol(x)) 
            as.character(x)
        else "", deparse(x, nlines = 1)[1]))
        if (is.null(nm)) 
            dep
        else {
            nm[fixup] <- dep
            nm
        }
    }
    args <- list(...)
    if (length(args) == 0) 
        stop("nothing to tabulate")
    if (length(args) == 1 && is.list(args[[1]])) {
        args <- args[[1]]
        if (length(dnn) != length(args)) 
            dnn <- if (!is.null(argn <- names(args))) 
                argn
            else paste(dnn[1], 1:length(args), sep = ".")
    }
    bin <- 0L
    lens <- NULL
    dims <- integer(0)
    pd <- 1L
    dn <- NULL
    for (a in args) {
        if (is.null(lens)) 
            lens <- length(a)
        else if (length(a) != lens) 
            stop("all arguments must have the same length")
        cat <- if (is.factor(a)) {
            if (!missing(exclude)) {
                ll <- levels(a)
                factor(a, levels = ll[!(ll %in% exclude)], exclude = if (is.null(exclude)) 
                  NULL
                else NA)
            }
            else a
        }
        else factor(a, exclude = exclude)
        nl <- length(ll <- levels(cat))
        dims <- c(dims, nl)
        dn <- c(dn, list(ll))
        bin <- bin + pd * (as.integer(cat) - 1L)
        pd <- pd * nl
    }
    names(dn) <- dnn
    bin <- bin[!is.na(bin)]
    if (length(bin)) 
        bin <- bin + 1L
    y <- array(tabulate(bin, pd), dims, dimnames = dn)
    class(y) <- "table"
    y
}


isItHere=function(x){
   if(missing(x)){cat("nothing here\n");return(invisible(0))}
   y=as.character(substitute(x));
   z=sprintf("exists('%s')",y);
   isthere=eval(parse(text=z));
   if(!isthere){cat(sprintf("%s not in environment\n",y));return(invisible(0))}
   eval(parse(text=y));
}

getParameterText=function(invoke,pname,ppos)
{
   callList=as.list(invoke[[1]]);
   callText=as.character(invoke[[1]]);
   pnames=names(callList);
   pos=((1:length(pnames))[pnames==pname])[1];
   if(!is.na(pos))
      return(callText[pos])
   else
      return(callText[ppos+1])
}

reAss=function(x){
# This function reassembles certain character strings
#
  if(length(x)<2){return(x)}
  if(x[1]=="$"){return(sprintf("%s%s%s",x[2],x[1],x[3]))}
  if(x[1]=="["){
     if(length(x)==3){return(sprintf("%s[%s]",x[2],x[3]))}
     if(length(x)==4){return(sprintf("%s[%s,%s]",x[2],x[3],x[4]))}
  }
  cat("Error: Cannot properly reassemble ");
  cat(x);
  cat(" please report.\n");
  return(0)
}

#functions to interactively create and save dataset
# should have a similar function for tables
# maybe should call the function InputDS
# and the get function ImportDS
# I'm going to include several different versions and
# give my testers an opportunity to decide which
# they like best
# my first versions will be quite general purpose
# the first an importData()
# the second an inputData() 
# the only difference is that import will use
# file.choose() to select a set, while
# the second will use keyboards in various ways
# both will have a parameter that can select alternate
# types

# note -- it appears by default
# the current working directory at startup is
# the default user directory
# on the other hand the file dialogue popup
# box for file.choose() will open for the first
# time in the home of the R-executable

ImportData=function(file=file.choose(),header=T,sep=defsep,form=set,...)
{
# a wrapper for several different functions that
   if(missing(form)) return(read.table(file,header,sep,...));
   type=as.character(substitute(form));
   if(type=="ask"){
   }
   if(missing(sep)&((type=="vec")|(type=="vech"))) sep=" " else sep=defsep;
   if(type=="vec")return(scan(file));
   if(type=="vech") return(scan(file,skip=1));
   if(type=="set")return(read.table(file,header,sep,...));
   if((type=="tabl1")|(type=="t1")) return(getTable1(file,header,sep));
   if((type=="tabl2")|(type=="t2")) return(getTable2(file,header,sep));
   cat(paste("Error: ",type," is not a valid type\n"));
}

InputT1=function(guided=F,help=F,sep=defsep)
{
   if(!guided & help)
   {
      # help for table input here
   }
   tabl=c();
   rn=c();
   vname="";
   linesRead=0;
   k=1;   
   xxx="xxx";
   while(xxx != "")
   {
       if(!linesRead)
       {
          prompt="Enter table variable name ";
          xxx=readline(prompt);
          vname=xxx;
          xxx="xxx";
          linesRead=linesRead+1;
       }
       else
       {
          if(guided)
             prompt="Enter value(text), count "
          else
             prompt="";   
          xxx=readline(prompt);
          if(xxx=="")
          {
             if(guided)
             {
                prompt="Are you sure you want to terminate data entry? (type n to continue) ";
                xxx=readline(prompt);
                if((xxx=="n")|(xxx=="N")) xxx="xxx" else xxx="";
             }
          }
          else
          {    
             yyy=unlist(strsplit(xxx,sep));
             if(length(yyy)!=2)
             {
                cat("Data line must contain value text and count separated by commas, please reenter\n");
             }
             else
             {
                if(suppressWarnings(is.na(as.numeric(yyy[2]))))
                {
                   cat("Second entry of data line must be numeric, reenter\n");
                }
                else
                {
                  rn[k]=yyy[1];
                  tabl[k]=as.numeric(yyy[2]);
                  k=k+1;            
                  linesRead=linesRead+1;
                }
             }
         }
          
     }
   }
   if(linesRead==1)
   {
      cat("No table data entered\n");
      invisble(return());
   }
   tabl=as.table(tabl);
   if(vname!="")
   {
     cmd=sprintf("dimnames(tabl)=list(%s=rn)",vname);
     eval(parse(text=cmd));
   }
   else
     dimnames(tabl)=list(rn);
   cat(sprintf("%s data lines read\n",linesRead));  
   tabl;
}

InputT2=function(guided=F,help=F,sep=defsep)
{
   if(!guided & help)
   {
      # help for table input here
   }
   tabl=c();
   rn=c();
   vnames="";
   linesRead=0;
   k=1;      # tracks current row of the table
   kk=1;     # tracks current numeric entry in table
   xxx="xxx";
   while(xxx != "")
   {
       if(!linesRead)
       {
          prompt="Enter table variable names ";
          xxx=readline(prompt);
          vnames=unlist(strsplit(xxx,sep));
          if(!length(vnames))
          {
               vnames="";
               linesRead=linesRead+1;
               xxx="xxx";
          }
          else
          if(length(vnames)!=2) {
             cat("Error: Must be two or no category names\n"); 
             
          }
          else
          {
             vnames=noquote(noquote(vnames));
             linesRead=linesRead+1;
           }
       }
       else
       if(linesRead==1)
       {
          prompt=sprintf("Enter column names separated by %s ",sep);
          xxx=readline(prompt);
          if(xxx!="")
          {
             cn=unlist(strsplit(xxx,sep));
             cols=length(cn);
             cols1=cols+1;
             linesRead=linesRead+1;
          }
          else
          {
             xxx="xxx";
          }
       }
       else
       {
          if(guided)
          {
            if(length(vnames))
              startprompt=sprintf("Enter %s category",vnames[1])
            else
              startprompt="Enter row category ";
             prompt=sprintf("%s & %d counts ",startprompt,cols)
          }   
          else
             prompt="";   
          xxx=readline(prompt);
          if(xxx=="")
          {
             if(guided)
             {
                prompt="Are you sure you want to terminate data entry? (type n to continue) ";
                xxx=readline(prompt);
                if((xxx=="n")|(xxx=="N")) xxx="xxx" else xxx="";
             }
          }
          else
          {    
             yyy=unlist(strsplit(xxx,","));
             numsOk=T;
             if(length(yyy)!=cols1)
             {
                cat(sprintf("Data line must include category and %d counts separated by commas, please reenter\n",cols1));
             }
             else
             {
                rn[k]=noquote(noquote(yyy[1]));
                if(sum(suppressWarnings(is.na(as.numeric(yyy[2:cols1])))))
                {
                    cat(sprintf("Non numeric entries found in row %s.\n",j-1));
                }
                else
                {
                     tabl[kk:(kk+cols-1)]=as.numeric(yyy[2:cols1]);
                     k=k+1;
                     kk=kk+cols;
                     linesRead=linesRead+1;
                }
              }
             }
         }
          
   }
   if(linesRead==2)
   {
      cat("No table data entered\n");
      invisble(return());
   }
   tabl=matrix(tabl,ncol=cols,byrow=T);
   tabl=as.table(tabl);
   if(length(vnames)==2)
   {
     cmd=sprintf("dimnames(tabl)=list(%s=rn,%s=cn)",vnames[1],vnames[2]);
     eval(parse(text=cmd));
   }
   else
     dimnames(tabl)=list(rn,cn);
   tabl;
}


InputData=function(form=ask,guided=F,help=F,sep)
{
# allows direct input of data to be placed in
# objects of a variety of classes
# vector
   if(missing(form))
   {
   
   }
   type=as.character(substitute(form));
   if(type=="ask"){
     cat("Choices are\n");
     cat("   1) vector ---- inputData(vec)");
     cat("   2) data set -- inputData(set)");
     cat("   3) 1-d table - inputData(t1)");
     cat("   4) 2-d table - inputData(t2)");
     prompt="enter 1-4 to run, return to exit";
     choice=readline(prompt);
     type="";
     if (choice=="1") type="vec";
     if (choice=="2") type="set";
     if (choice=="3") type="t1";
     if (choice=="4") type="t2";
     if (type=="") invisible(return());
   }
   if(missing(sep)&(type=="vec"))sep=" " else sep=defsep;
   if(type=="vec")return(scan(sep=sep));
   if(type=="set")return(createDataSet());
   if((type=="tabl1")|(type=="t1"))
   {   
       return(inputT1(guided,help,sep));
   }
   if((type=="tabl2")|(type=="t2"))
   {
       return(inputT2(guided,help,sep));
   }
   cat(paste("Error: ",type," is not a valid type\n"));
}

CreateDataSet=function(setName){
name=NULL;
if(!missing(setName)){
   name=as.character(substitute(setName));
   check=sprintf("exists('%s')",name);
   isthere=eval(parse(text=check));
   if (isthere){
      cat(sprintf("%s exists in environment, must remove first\n",name));
      return(invisible(NULL));
   }
}
x=data.frame();
if(is.null(name)) return(edit(x));
action=sprintf("%s <<- edit(x)",name);
eval(parse(text=action));
}


GetDataSet=function(file=file.choose(),header=T,sep="\t",...)
{ read.table(file,header=header,sep=sep,...)}

# Need functions to get tables
# To find out if a particular string is numeric we need
# suppressWarnings(is.na(as.numeric(string)))
# if the string is numeric this will return a false
# if not it will return true
# getTable reads 1 dimensional table

GetTable1=function(file=file.choose(),header=T,sep=defsep)
{ 
  x=readLines(file);if(!length(x)){print("no table data");return()}
  if(header){vname=x[1];j=2} else {vname="";j=1};
  if(!(length(x)-j+1)){cat("no table data");return()}
  k=1; tabl=c();rn=c();
  for(i in j:length(x))
  {
     l=unlist(strsplit(x[i],sep));
     if(!(length(l)==2)|suppressWarnings(is.na(as.numeric(l[2]))))
     { 
         out=paste("invalid data at line ",i);
         cat(out);
         return();
     }
    rn[k]=l[1];
    tabl[k]=as.numeric(l[2]);
    k=k+1;
   }
   tabl=as.table(tabl);
   if(vname!="")
   {
     cmd=sprintf("dimnames(tabl)=list(%s=rn)",vname);
     eval(parse(text=cmd));
   }
   else
     dimnames(tabl)=list(rn);
   tabl;
}

GetTable2=function(file=file.choose(),header=T,sep=defsep)
{ 
  x=readLines(file);if(!length(x)){print("no table data");return()}
  if(header)
  {
      vnames=unlist(strsplit(x[1],sep));
      if(length(vnames)!=2) {cat("Error: Must be two category names"); return()}
      vnames=noquote(noquote(vnames));
      j=2
  } else {vnames=c("");j=1};
  if(!((length(x)-j)>1)){cat("Error: no table data");return()}
  cn=unlist(strsplit(x[2],sep));
  cols=length(cn);
  cols1=cols+1;
  j=j+1;
  k=1;kk=1; tabl=c();rn=c();
  for(i in j:length(x))
  {
     l=unlist(strsplit(x[i],sep));
     if(!(length(l)==cols1)|sum(suppressWarnings(is.na(as.numeric(l[2:cols1])))))
     { 
         out=paste("Error: invalid data at line ",i);
         cat(out);
         invisible(return());
     }
     rn[k]=noquote(noquote(l[1]));
     tabl[kk:(kk+cols-1)]=as.numeric(l[2:cols1]);
     k=k+1;
     kk=kk+cols;
   }
   tabl=matrix(tabl,ncol=cols,byrow=T);
   tabl=as.table(tabl);
   if(length(vnames)==2)
   {
     cmd=sprintf("dimnames(tabl)=list(%s=rn,%s=cn)",vnames[1],vnames[2]);
     eval(parse(text=cmd));
   }
   else
     dimnames(tabl)=list(rn,cn);
   tabl;
}

# getResultTable is used as a routine within
# Mean, Max, Min, Range
# to handle the looping needed to create
# table of output values when input
# is a matrix or dataframe with multiple
# mumeric columns


getResultTable=function(x,fn,opts1=NULL,opts2=NULL)
{
   j=0;
   results=c();
   rn=c();
   for(i in 1:dim(x)[2])
   {
      if(is.numeric(x[,i]))
      {  
         j=j+1;
         y=colnames(x)[i];
         if (is.null(y)) y=sprintf("col-%d",i);
         rn[j]=y;
         ll=sum(is.na(x[,i]))
         if(ll)
         {
             cat(sprintf("Warning: %s has %d missing values.\n",y,ll));
         }
         cmd=sprintf("%s(%s[,i],",as.character(substitute(fn)),as.character(substitute(x)));
         if (!is.null(opts1))
            cmd=sprintf("%s %s,",cmd,opts1);
         cmd=sprintf("%s na.rm=T",cmd)
         if(!is.null(opts2))
         {
            cmd=sprintf("%s,%s)",cmd,opts2)
         }
         else
         {
            cmd=sprintf("%s)",cmd);
         }

#         cat(j);cat(" ");cat(cmd);cat("\n");
         results[j]=eval(parse(text=cmd));      
        }
    }
    if(j){
       results=as.table(results);
       cmd=sprintf("dimnames(results)=list(%s=rn)",as.character(substitute(fn)));
       eval(parse(text=cmd));
       return(results);
    }
    else
    {
        cat(sprintf("%s has no numeric columns\n",as.character(substitute(x))));
        return(0);
    }
} 

Mean=function(x,trim=0,na.rm,...)
{ 
  l=0;
  if(missing(na.rm)) {l=sum(is.na(x));na.rm=F};
  if(l)
  {
     if(is.data.frame(x)|is.matrix(x))
     {
         invoke=sys.calls();
         x1=getParameterText(invoke,"x",1)
#         x1=as.character(substitute(x));
#         x2=as.character(substitute(trim));
         x2=getParameterText(invoke,"trim",2)
         if(is.na(x2))
            cmd=sprintf("getResultTable(%s,mean)",x1)
         else if(missing(...))
         {
            cmd=sprintf("getResultTable(%s,mean,%s)",x1,x2);
            print(cmd);
         }
         else
         {
            x3=as.character(substitute(...));
            cmd=sprintf("getResultTable(%s,mean,%s,%s)",x1,x2,x3);
            print(cmd);
         }
         result=eval(parse(text=cmd));
         return(result);
      }     
     else
     {
        invoke=sys.calls();
        z=getParameterText(invoke,"x",1);
#        z=as.character(substitute(x));
#        z=reAss(z);
#        if(length(z)==3) z=sprintf("%s%s%s",z[2],z[1],z[3]);
        cat(sprintf("Warning: %s has %d missing values.\n",z,l));
        return(base::mean(x,trim,na.rm=T,...));
     }     
  }
  mean(x,trim,na.rm,...);
}


Max=function(x,na.rm)
{ 
  l=0;
  if(missing(na.rm)) {l=sum(is.na(x));na.rm=F};
  if(l)
  {
     if(is.data.frame(x)|is.matrix(x))
     {
         invoke=sys.calls();
         x1=getParameterText(invoke,"x",1)
         cmd=sprintf("getResultTable(%s,max)",x1)
         result=eval(parse(text=cmd));
         return(result);
     }     
     else
     {
         invoke=sys.calls();
         z=getParameterText(invoke,"x",1)
        cat(sprintf("Warning: %s has %d missing values.\n",z,l));
        return(max(x,na.rm=T));
     }     
  }
  max(x,na.rm=na.rm);
}

Min=function(x,na.rm)
{ 
  l=0;
  if(missing(na.rm)) {l=sum(is.na(x));na.rm=F};
  if(l)
  {
     if(is.data.frame(x)|is.matrix(x))
     {
         x1=as.character(substitute(x));
         cmd=sprintf("getResultTable(%s,min)",x1)
         result=eval(parse(text=cmd));
         return(result);
     }     
     else
     {
        z=as.character(substitute(x));
        z=reAss(z);
        cat(sprintf("Warning: %s has %d missing values.\n",z,l));
        return(min(x,na.rm=T));
     }     
  }
  min(x,na.rm=na.rm);
}

Range=function(x,na.rm)
{ 
  l=0;
  if(missing(na.rm)) {l=sum(is.na(x));na.rm=F};
  if(l)
  {
     if(is.data.frame(x)|is.matrix(x))
     {
        j=0;
        results=c();
        rn=c();
        for(i in 1:dim(x)[2])
        {
           if(is.numeric(x[,i]))
           {  
              j=j+1;
              y=colnames(x)[i];
              if (is.null(y)) y=sprintf("col-%d",i);
              rn[j]=y;
              ll=sum(is.na(x[,i]))
              if(ll)
              {
                  cat(sprintf("Warning: %s has %d missing values.\n",y,ll));
              }
           results[j]=max(x[,i],na.rm=T)-min(x[,i],na.rm=T);
           }
        }
        if(j){
           results=as.table(results);
           dimnames(results)=list(rn);
           return(results);
        }
        else
        {
             cat(sprintf("%s has no numeric columns\n",as.character(substitute(x))));
             return(0);
        }
     }     
     z=as.character(substitute(x));
        z=reAss(z);
#        if(length(z)==3) z=sprintf("%s%s%s",z[2],z[1],z[3]);
     cat(sprintf("Warning: %s has %d missing values.\n",z,l));
     return(max(x,na.rm=T)-min(x,na.rm=T));
  }
  max(x,na.rm)-min(x,na.rm);
}


Quantile=function(x,probs=seq(0,1,.25),na.rm,...)
{ 
  l=0;
  if(missing(na.rm)) {l=sum(is.na(x));na.rm=T};
  if(l)
  {
     if(is.data.frame(x)|is.matrix(x))
     {
        # Output for data frame or matrix is a table of quantiles
        # where rows correspond to eligible columns
        # of the data frame or matrix
        # The columns of the matrix correspond
        # to the different quantiles we generate
        n=probs*100;
        qtls=c();       # we'll assemble all quantile values here
        rn=c();         # all row names here
        rows=0;
        for(i in 1:dim(x)[2])
        {
           if(is.numeric(x[,i]))
           {
              y=colnames(x)[i];    # y is the name of the current column
              rows=rows+1;
              ll=sum(is.na(x[,i]))
              if(ll)
              {
                 if(is.null(y))
                 {
                    y=sprintf("column %d of %s",i,as.character(substitute(x)));
                    cat(sprintf("Warning: %s has %d missing values.\n",y,ll));
                 }
                 else
                  cat(sprintf("Warning: column %s has %d missing values.\n",y,ll));
              }
              if(is.null(y))
                rn=c(rn,sprintf("Col %d:",i))
              else
                rn=c(rn,y);
              q=quantile(x[,i],probs,na.rm,...);
              qtls=c(qtls,q);
           }
        }
        qtls=matrix(qtls,nrow=rows,byrow=T);
        cn=c();
        for(i in 1:length(n)){cn=c(cn,sprintf("%d%%",n[i]))}
        qtls=as.table(qtls)
        dimnames(qtls)=list(rn,cn);
        return(qtls);
     }     
     else
     {
        z=as.character(substitute(x));
        z=reAss(z);
#        if(length(z)==3) z=sprintf("%s%s%s",z[2],z[1],z[3]);
        cat(sprintf("Warning: %s has %d missing values.\n",z,l));
     }     
     return(quantile(x,probs,na.rm,...));
  }
  quantile(x,probs,na.rm,...);
}

Var=function(x,na.rm,...)
{ 
  l=0;
  if(missing(na.rm)) {l=sum(is.na(x));na.rm=F};
  if(l)
  {
     if(is.data.frame(x)|is.matrix(x))
     {
         x1=as.character(substitute(x));
         cmd=sprintf("getResultTable(%s,myvar)",x1)
         result=eval(parse(text=cmd));
         return(result);
     }
     else
     {
        z=as.character(substitute(x));
        z=reAss(z);
        cat(sprintf("Warning: %s has %d missing values.\n",z,l));
        return(myvar(x,na.rm=T,...));     
     }
  }
  var(x,na.rm=na.rm,...);
}

Sd=function(x,na.rm,...)
{
  l=0;
  if(missing(na.rm)) {l=sum(is.na(x));na.rm=F};
  if(l)
  {
     if(is.data.frame(x)|is.matrix(x))
     {
         x1=as.character(substitute(x));
         cmd=sprintf("getResultTable(%s,mysd)",x1)
         result=eval(parse(text=cmd));
         return(result);
     }
     else
     {
        z=as.character(substitute(x));
        z=reAss(z);
        cat(sprintf("Warning: %s has %d missing values.\n",z,l));
        return(mysd(x,na.rm=T,...));     
     }
  }
  sd(x,na.rm,...);
}

# From a student usability standpoint the defaults in 
# barplot are all wrong
# issues: 1) for graphs of two way tables 


Barplot=function(height,freq=T,order=NULL,beside=T,...)
{
    if(is.data.frame(height))
    {
         if(dim(height)[2]>2)
         {
             cat(height);cat(" has too many columns\n");
             return();
         }
         if(((dim(height)[2]==2) & (class(height[,2])!="numeric"))|
            ((dim(height)[2]==1) & (class(height[,1])!="numeric")))
         {
             cat(height);cat(" does not have numeric data\n");
             return()
         }
         if(dim(height)[2]==2)
         {
              height=tapply(height[,2],height[,1],sum);
         }
         else
              height=height[,1];
         
    }

# if a relative frequency barplot calculate percentages
    if(!freq) 
      if (!is.null(dim(height)))             # is this a matrix?
      {
         if(!is.na(dim(height)[2]))          # if it's two dimensional find row relative freq
            height=prop.table(height,1)*100
         else 
            height=prop.table(height)*100
      }
      else
         height=prop.table(height)*100;
         
# now before the actual plotting
# determine if we're working with a two dimensional table
     dim2=0;
     if(!is.null(dim(height))) if (!is.na(dim(height)[2])) dim2=1;

# do we have to reorder the table -- only for one dimensional tables
     if(!dim2 & !is.null(order))
     {
        if(order=="a")
           height=height[sort(names(height))]
        if(order=="d")
           height=height[sort(names(height),decreasing=T)]
        if(order=="A")
           height=height[order(height,height)]
        if(order=="D")
           height=height[order(height,height,decreasing=T)]
      }     

      # if it's a two dimensional matrix need to work on transpose

      if (dim2)
          barplot(t(height),beside=beside,...)
      else
          barplot(height,...)
          
}  

Dotplot=function(x,y=NULL,xlabs=NULL,xlim=NULL,col=NULL,xlab=NULL,...){
if(missing(x)){cat("Error: no first variable specifed\n");return(invisible(0))}; 
if(!is.numeric(x)){cat("Error: first variable must be numeric\n");return(invisible(0));};
if(is.null(y)){p1=xlim;p2=col;
if(is.null(p1)&is.null(p2))stripchart(x,at=0,pch=19,method="stack",frame.plot=F,xlab,...);
if(is.null(p1)&!is.null(p2))stripchart(x,at=0,pch=19,method="stack",frame.plot=F,col=p2,xlab,...);
if(!is.null(p1)&is.null(p2))stripchart(x,at=0,pch=19,method="stack",frame.plot=F,xlim=p1,xlab,...);
if(!is.null(p1)&!is.null(p2))stripchart(x,at=0,pch=19,method="stack",frame.plot=F,xlim=p1,col=p2,xlab,...);
};
if(!is.null(y)) {
if(!is.numeric(y)){cat("Error: second variable must be numeric\n");return(invisble(0));};
if(is.null(xlim)){xlim=c(min(c(x,y),na.rm=T),max(c(x,y),na.rm=T))};p1=xlim;
if(is.null(col)){col=c("Green","Blue")};p2=col;
p3=xlab;if(!is.null(p3)){if(is.na(p3[2])) p3[2]=""};
oldpar=par(no.readonly=T);
par(mfrow=c(2,1));
stripchart(x,method="stack",xlim=p1,col=p2[1],at=0,pch=19,frame.plot=F,xlab=p3[1],...);
stripchart(y,method="stack",xlim=p1,col=p2[2],at=0,pch=19,frame.plot=F,xlab=p3[2],...);
par(oldpar);
}
}

Hist=function(x,type,xlab,main,breaks="Sturges",...)   
{
  if(missing(xlab)){z=as.character(substitute(x)); 
  xl= reAss(z)} else xl=xlab;
#  cat(sprintf("xlab %s \n",xlab));
  if(missing(main))mn=paste("Histogram of ",xl) else mn=main;
#  cat("main ");cat(main);cat("\n");
    y=as.character(substitute(type));
  if(!is.null(y)&length(y)){
     if(y=="perc"){
        y=suppressWarnings(hist(x,breaks,plot=F,...));
        y$counts=100*y$counts/sum(y$count);
        y$density=y$counts;
        suppressWarnings(plot(y,ylab="Percent",xlab=xl,main=mn,...));
        return(invisible(y))}
     if(y=="freq")
        return(hist(x,breaks,freq=T,xlab=xl,main=mn,...));
     if(y=="dens")
        return(hist(x,breaks,freq=F,xlab=xl,main=mn,...));   
 }
 hist(x,breaks,xlab=xl,main=mn,...)
}

Table=function(x,y=NULL,count=NULL,type="freq",dnn,...)
{
   if(missing(dnn))
   {
      if(is.null(y))
      {
        z=as.character(substitute(x));
        z=reAss(z);
        dnn=c(z);
      }
      else
      {
        z1=as.character(substitute(x));
        z1=reAss(z1);
        z2=as.character(substitute(y));
        z2=reAss(z2);
        dnn=c(z1,z2);
      }
   }
   if(!is.null(count))
   {
       if(is.null(y))
       {
           temp=xtabs(count~x);
#          temp=tapply(count,x,sum);
        }
        else
        {
           temp=xtabs(count~x+y);       
#          temp=tapply(count,list(x,y),sum);
        }
    
   }
   else
   {
       if(is.null(y))
       {
          temp=xtable(x,dnn=dnn,...)
       }
       else
       {
       
          temp=xtable(x,y,dnn=dnn,...);
       }
          
   }
   z=as.character(substitute(type));
   if(z=="rel")
   {
       if(is.null(y))
          temp=prop.table(temp)
       else
          temp=prop.table(temp,1)
   }
   if(z=="perc")
   {
       if(is.null(y))
          temp=prop.table(temp)*100
       else
          temp=prop.table(temp,1)*100
   }
   temp;
}

## Needed comprehensive table information
## How about ftables?
    