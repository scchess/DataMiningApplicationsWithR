library(RODBC)
library(RSQLite)

################ step 1
m <- dbDriver("SQLite")
con <- dbConnect(m, dbname = "parsian3.dbms")
customertarget3=dbGetQuery(con, "select * from customertarget3")

m <- dbDriver("SQLite")
con <- dbConnect(m, dbname = "parsian5.dbms")
dbWriteTable(con, "customertarget3f", customertarget3 , overwrite = T)
customertarget3fdata=dbGetQuery(con, "select * from customertarget3f")
customertarget3fdataorder<- dbGetQuery(con, "select * from customertarget3f order by target")

customertarget3fdatanormal=scale(customertarget3fdataorder)
fs= matrix(0,ncol(customertarget3fdatanormal),ncol(customertarget3fdatanormal))

####If Normal data ther is no need to as.real
v=0
for (i in 2:ncol(fs)){
    for (j in 2:(i-1)){
v=0
        v=customertarget3fdatanormal[,i]*customertarget3fdatanormal[,j]

        meanpositive=mean(v[20716:22427])
        meannegative=mean(v[1:20715])
        meantotal=mean(v[1:22427])

        varpositive=var(v[20716:22427])
        varnegative=var(v[1:20715])

        fs[i,j]=(((((meanpositive-meantotal)^2)+((meannegative-meantotal)^2)))/(varpositive+varnegative))
    }
}


#####Finding High F-score
f=matrix(0,7569,3)
p=1
    for (i in 1:ncol(fs)){
        for (j in 1:nrow(fs)){
            f[p,2]=i
            f[p,3]=j
            f[p,1]=fs[i,j]
            p=p+1
        }
    }

f2=as.data.frame(f[order(f[,1]), ])
f2

#####################
names(customertarget3fdata[55])



for (t in 7532:nrow(f2)){
    i=f2[t,2]
    j=f2[t,3]

    dbSendQuery(con, paste("alter table customertarget3f add column inter", as.character(i),"_", as.character(j) ," integer", sep = ""))
    dbSendQuery(con, paste("update customertarget3f set inter", as.character(i),"_", as.character(j) ,"=", as.character(names(customertarget3fdata[i]))," * ", as.character(names(customertarget3fdata[j])), sep=""))
}


################# step 2

m <- dbDriver("SQLite")
con <- dbConnect(m, dbname = "parsian5.dbms")
customertarget3fdata=dbGetQuery(con, "select * from customertarget3f")


customertarget3finter=dbGetQuery(con, "select * from customertarget3f order by target")
customertarget3finternormal=scale(customertarget3finter)

fs4= matrix(0,ncol(customertarget3finternormal),2)
#fs=vector()
####If Normal data ther is no need to as.real
v=0
for (i in 1:nrow(fs4)){
   
        v=0
        v=customertarget3finternormal[,i]

        meanpositive=mean(v[20716:22427])
        meannegative=mean(v[1:20715])
        meantotal=mean(v[1:22427])

        varpositive=var(v[20716:22427])
        varnegative=var(v[1:20715])

        fs4[i,1]= i
        fs4[i,2]=(((((meanpositive-meantotal)^2)+((meannegative-meantotal)^2)))/(varpositive+varnegative))
    
}

fsorder4=as.data.frame(fs4[order(fs4[,2]), ])


customerselectedf=data.frame(ID=cbind(customertarget3fdata[,1]),
                        avgtranshortlast=cbind(customertarget3fdata[,64]),
                        totalservice=cbind(customertarget3fdata[,12]),
				totalservicelastyear=cbind(customertarget3fdata[,26]),
				totaldepositshort=cbind(customertarget3fdata[,80]),
				totaldepositshortlast=cbind(customertarget3fdata[,85]),
    				totaltranlast=cbind(customertarget3fdata[,33]),
   				totaltran=cbind(customertarget3fdata[,70]),
     				totaltranshortlast=cbind(customertarget3fdata[,32]),
  				totaltranshort=cbind(customertarget3fdata[,41]),
                        inter17_3=cbind(customertarget3fdata[,89]),
    				recencymaxtrandate=cbind(customertarget3fdata[,87]),
    				age=cbind(customertarget3fdata[,7]),
                        inter28_14=cbind(customertarget3fdata[,91]),
    				mintranlong=cbind(customertarget3fdata[,36]),
    				mintranlonglast=cbind(customertarget3fdata[,44]),
    maxtranlong=cbind(customertarget3fdata[,48]),
    maxtranlonglast=cbind(customertarget3fdata[,67]),
    avgtranlong=cbind(customertarget3fdata[,53]),
    avgtranlonglast=cbind(customertarget3fdata[,63]),
    totaltranlonglast=cbind(customertarget3fdata[,31]),
    totaltranlong=cbind(customertarget3fdata[,40]),
    inter55_28=cbind(customertarget3fdata[,92]),
    inter19_14=cbind(customertarget3fdata[,94]),
    shortlastyear=cbind(customertarget3fdata[,18]),
    notrantotal=cbind(customertarget3fdata[,60]),
    notrantotallast=cbind(customertarget3fdata[,76]),
    inter55_19=cbind(customertarget3fdata[,102]),
    gharzlastyear=cbind(customertarget3fdata[,16]),
    inter16_14=cbind(customertarget3fdata[,104]),
    inter55_27=cbind(customertarget3fdata[,93]),
    notranshort=cbind(customertarget3fdata[,59]),
    notranshortlast=cbind(customertarget3fdata[,75]),
    recencylastaccount=cbind(customertarget3fdata[,14]),
    inter55_6=cbind(customertarget3fdata[,101]),
    inter58_14=cbind(customertarget3fdata[,105]),
    inter74_14=cbind(customertarget3fdata[,106]),
    inter14_4=cbind(customertarget3fdata[,100]),
    inter87_55=cbind(customertarget3fdata[,90]),
    recencylasttran=cbind(customertarget3fdata[,55]),
    short=cbind(customertarget3fdata[,5]),
    gharz=cbind(customertarget3fdata[,3]),
    inter17_14=cbind(customertarget3fdata[,107]),
totalaccserv=cbind(customertarget3fdata[,27]),
totalaccservlastyear=cbind(customertarget3fdata[,28]),
totalacc=cbind(customertarget3fdata[,6]),
notranlong=cbind(customertarget3fdata[,58]),
notranlonglast=cbind(customertarget3fdata[,74]),
totalacclastyear=cbind(customertarget3fdata[,19]),
long=cbind(customertarget3fdata[,4]),
longlastyear=cbind(customertarget3fdata[,17]),
target=cbind(customertarget3fdata[,71]))



m <- dbDriver("SQLite")
con <- dbConnect(m, dbname = "parsian5.dbms")


dbWriteTable(con, "customertargetselectedf", customerselectedf , overwrite = T)
 
customerselectedfscale=subset(customerselectedf, select=-target)
customerselectedfscale=subset(customerselectedfscale, select=-ID)
customerselectedfscale=scale(customerselectedfscale)

customerselectedfscale=cbind(customerselectedfscale,data.frame(target=customertarget3fdata[,71]))
customerselectedfscale=cbind(customerselectedfscale,data.frame(ID=customertarget3fdata[,1]))

dbWriteTable(con, "customerselectedfscale", customerselectedfscale, overwrite = T)
