### R code from vignette source 'Chapter10.Rnw'
### Encoding: ISO8859-1

###################################################
### code chunk number 1: CargaPaquetes
###################################################
set.seed(1)
options(warn=-1,width=65,continue=" ")
options(SweaveHooks=list(fig=function()
	  par(mar=c(5.1, 4.1, 1.1, 2.1))))
require(xtable)


###################################################
### code chunk number 2: Anon0
###################################################
Bilbao    <- read.csv(file="Bilbao.csv")
Bilbao$Date <- as.Date(as.character(Bilbao$Date))


###################################################
### code chunk number 3: ReadData1
###################################################
Bilbao      <- read.csv(file="Bilbao.csv")
BilbaoUTM   <- read.csv(file="BilbaoUTM.csv")


###################################################
### code chunk number 4: ReadData2
###################################################
Bilbao$Date <- as.Date(Bilbao$Date)


###################################################
### code chunk number 5: Chapter10.Rnw:583-590
###################################################
aaa <- factor(Bilbao$Age,
	      levels=c("no consta","< 5 años","5-10 años","10-20 años","20-30 años","+ 30 años"))
aaa[is.na(aaa)] <- "no consta"
Bilbao$Age <- aaa
aaa <- factor(Bilbao$Heating,
	      levels=c("desc/no tiene","individual","central"))
Bilbao$Heating <- aaa


###################################################
### code chunk number 6: Chapter10.Rnw:673-674 (eval = FALSE)
###################################################
## download.file(URL,File,quiet=0,method="wget")


###################################################
### code chunk number 7: XML (eval = FALSE)
###################################################
## doc <- htmlTreeParse(file=File)$children$html[["body"]]


###################################################
### code chunk number 8: TransformData1
###################################################
Bilbao$Street <- gsub("(^ +)|( +$)", "",Bilbao$Street)
Bilbao$Street <- toupper(Bilbao$Street)
Bilbao$Street <- chartr("ÁÉÍÓÚ", "AEIUO",Bilbao$Street)


###################################################
### code chunk number 9: TransformData1b
###################################################
Addr2Search <- with(Bilbao,paste(Type,Street,Num,CP,sep="-"))
Searchable  <- with(BilbaoUTM,paste(Type,Direccion,Num,CP,sep="-"))


###################################################
### code chunk number 10: TransformData2
###################################################
Key <- Addr2Search[611]
Key


###################################################
### code chunk number 11: Match1
###################################################
match(Key,Searchable)


###################################################
### code chunk number 12: TransformData3
###################################################
Key <- gsub("ZUMALA[C|K]AR(R*)EG(U*)I",
	    "ZUMALACARREGUI", Key, perl=TRUE)
Key


###################################################
### code chunk number 13: Match2
###################################################
i <- match(Key,Searchable)
BilbaoUTM[i,c("Type","Direccion","Num","CP","UTMX","UTMY")]


###################################################
### code chunk number 14: TransformData4
###################################################
found <- match(Addr2Search,Searchable)
Bilbao <- cbind(Bilbao, BilbaoUTM[found, c("UTMX","UTMY")] )


###################################################
### code chunk number 15: RefPlot1
###################################################
require(maptools)
Bilbao.utm <- readShapePoly("BilbaoDistricts.shp",
    proj4string=CRS("+proj=utm +zone=30 +ellps=intl"))


###################################################
### code chunk number 16: RefPlot2
###################################################
require(rgdal)
Bilbao.wgs84 <- spTransform(Bilbao.utm,
		CRS("+proj=longlat +datum=WGS84 +ellps=WGS84"))


###################################################
### code chunk number 17: RefPlot3
###################################################
Bilbao.PS <-  SpatialPolygons2PolySet(Bilbao.wgs84)


###################################################
### code chunk number 18: Anon2
###################################################
Bilbao.PS$X <- Bilbao.PS$X - 0.0014
Bilbao.PS$Y <- Bilbao.PS$Y - 0.0012


###################################################
### code chunk number 19: RefPlot4
###################################################
require(RgoogleMaps)
lat <- c(43.222,43.2822)
lon <- c(-2.880,-2.985)
bb <- qbbox(lat, lon, margin=list(m=c(1,1,1,1), TYPE="perc"))
Bilbo.raster <- GetMap.bbox(bb$lonR,bb$latR,maptype="mobile",
			    destfile="Bilbao.png")


###################################################
### code chunk number 20: Anon3
###################################################
pdf(file="RefPlot.pdf")


###################################################
### code chunk number 21: RefPlot5
###################################################
PlotPolysOnStaticMap(Bilbo.raster,Bilbao.PS,lwd=1.5,
		     col=NULL,add=FALSE)


###################################################
### code chunk number 22: Anon4
###################################################
aaa <- dev.off()


###################################################
### code chunk number 23: DistrictMap
###################################################
fich <- "/home/etptupaf/ft/datos/Cartografia/Bilbao/BilbaoDistritos.shp"
xx <- readShapePoly(fich)
plot(xx,axes=TRUE)
text(503300,4792000,"San Ignacio",cex=0.9)
text(504200,4791300,"Deusto",cex=0.9)
text(505100,4789900,"Indautxu",cex=0.9)
text(505400,4790400,"Abando",cex=0.9)
text(506300,4789600,"Casco\nViejo",cex=0.9)
text(506050,4787900,"La Peña",cex=0.9)
text(502900,4790300,"Basurto\nZorroza",cex=0.9)
text(507400,4789268,"Begoña",cex=0.9)
text(507150,4789000,"Santutxu",cex=0.9)
text(507300,4788560,"San Adrián",cex=0.9)
text(506650,4790720,"Uribarri",cex=0.9)
text(508128,4789867,"Otxarcoaga\nTxurdinaga",cex=0.9)
text(504700,4788563,"Recalde",cex=0.9)
par(cex=1.0)


###################################################
### code chunk number 24: Anon5
###################################################
pdf(file="boxplotpm2.pdf")


###################################################
### code chunk number 25: Pricem2
###################################################
require(zoo)
boxplot(Price/M2built ~ as.yearmon(Date), data=Bilbao,
	ylab="EUR per square meter", xlab="Date",
	varwidth=TRUE, cex=0.5)


###################################################
### code chunk number 26: Anonx
###################################################
aaa <- dev.off()


###################################################
### code chunk number 27: CompoVentas1
###################################################
attach(Bilbao)
work1 <- table(Date,District)
work2 <- zoo(x=work1, order.by=as.Date(rownames(work1)))
work3 <- aggregate(x=work2, by=as.yearmon, FUN=sum)
coredata(work3) <- 100*coredata(work3) / rowSums(work3)


###################################################
### code chunk number 28: Chapter10.Rnw:963-974
###################################################
work3 <- work3[-(1:24),]
perm <- rev(order((apply(tail(work3,3),2,sum)-apply(head(work3,3),2,sum))))
work4 <- work3 <- work3[,perm]
work4[,1] <- apply(work3[,c("Abando - Albia","Indautxu")],1,sum)
work4[,3] <- apply(work3[,c("Ibaiondo","Otxarkoaga - Txurdinaga","San Adrián - La Peña")],1,sum)
work4[,2] <- apply(work3[,c("Basurto - Zorroza","Begoña - Santutxu","Casco Viejo","Deusto","Rekalde",
			    "San Ignacio","Uribarri")],1,sum)
work4 <- work4[,1:3]
colnames(work4) <- c("High price (Abando-Albia, Indautxu)",
		     "Rest of districts",
		     "Low price (Ibaiondo, Otxarkoaga-Txurdinaga, San Adrián-La Peña)")


###################################################
### code chunk number 29: CompoVentas2
###################################################
require(RColorBrewer)
myPalette <- colorRampPalette(brewer.pal(3,"Spectral"))
oldpar <- par()
par(xpd=T, mar=par()$mar+c(9,0,0,0))
barplot(t(coredata(work4)),names.arg=index(work4),
	ylab="House offers (% total)",col=myPalette(3))
legend(7,-23,colnames(work4)[c(1,3,2)],cex=0.8,ncol=1,
       fill=myPalette(3)[c(1,3,2)])
detach(Bilbao)
par(oldpar)


###################################################
### code chunk number 30: mod.1-1
###################################################
require(gam)
x  <- as.numeric( Bilbao$Date - min(Bilbao$Date) + 1 )
mod.1 <- gam(log(Price/M2built) ~ Bedrooms + Bathrooms
	       + Elevator + log(M2built) + Community
	       + ParkingPlaces + Heating + HouseType +
	       + District + Age + s(x,12), data=Bilbao)


###################################################
### code chunk number 31: mod.1-2
###################################################
aaa <- print(xtable(lm(mod.1)),file="/dev/null")
aaa <- sub("hline","toprule",aaa)
aaa <- sub("hline","midrule",aaa)
aaa <- sub("hline","bottomrule",aaa)
aaa <- sub("HouseType","\\\\textbf{Type of house:} & \\\\multicolumn{4}{c}{(ref. level: penthouse)} \\\\\\\\",aaa)
aaa <- gsub("HouseType","",aaa)
aaa <- sub("District","\\\\textbf{District:} & \\\\multicolumn{4}{c}{(ref. level: Abando-Albia)} \\\\\\\\",aaa)
aaa <- gsub("District([a-zA-Z]+)","\\1",aaa)
aaa <- sub("Heating","\\\\textbf{Type of heating:} & \\\\multicolumn{4}{c}{(ref. level: none)} \\\\\\\\",aaa)
aaa <- gsub("Heating","",aaa)
aaa <- sub("Age","\\\\textbf{Age of building:} & \\\\multicolumn{4}{c}{(ref. level: unknown)} \\\\\\\\",aaa)
aaa <- gsub("Age([a-zA-Z<0-9]+)","\\1",aaa)
aaa <- gsub("s\\(x, ([0-9]+)\\)","\\\\textbf{Time trend:\\\\hfill}\\\\\\\\ s(x, \\1)",aaa)
aaa <- gsub("\\\\begin\\{center\\}",
	    "\\\\begin\\{center\\}\\\\caption\\{Model~1: \\$\\\\log(\\\\textrm{Price}/\\\\textrm{m}^2) = \\\\sum_{i=1}^p\\\\beta_ix_i + s(t) + \\\\epsilon. $ \\}\\\\label\\{betas\\}",
	    aaa)
# aaa <- gsub("ElevatorTRUE","Elevator",aaa)
aaa <- gsub("Dormitorios","Bedrooms",aaa)
aaa <- gsub("Baños","Bathrooms",aaa)
aaa <- gsub("años","years",aaa)
aaa <- gsub("PlazasGaraje","Parking places",aaa)
aaa <- gsub("M2built","Square meters",aaa)
aaa <- gsub("estudio","studio",aaa)
aaa <- gsub("piso","flat",aaa)
aaa <- gsub("dúplex","duplex",aaa)
aaa <- gsub("ático","penthouse",aaa)
aaa <- gsub("casa o chalet independiente","single house",aaa)
aaa <- gsub("chalet adosado","house in a block",aaa)
aaa <- gsub("chalet pareado","paired house",aaa)
aaa <- gsub("central","colective",aaa)
aaa <- gsub("individual","individual",aaa)
aaa <- gsub("desc/no tiene","none/unstated",aaa)
aaa <- gsub("indeterminada","unstated",aaa)
aaa <- gsub("no consta","unknown",aaa)
aaa <- gsub("no disponible","unavailable",aaa)
aaa <- gsub("Comunidad","Community fees",aaa)
aaa <- gsub("Ascensor","Elevator",aaa)
cat("\n")
cat(aaa)


###################################################
### code chunk number 32: ProcesoDatos
###################################################
#
#   Proceso datos
#
#
CompFechas <- function(indice,fechas) {
  scratch <- zoo(0,order.by=seq.Date(from=min(fechas),to=max(fechas),by="day"))
  indice  <- merge(scratch,indice)[,-1]
  return(na.approx(indice,rule=2))
}
#
ConsInd <- function(modelo=NULL,base="2005-01-01",comp="s(x, 12)",rug=FALSE,
		    fechas=NULL,ylabel=NULL,confint=TRUE,plot=TRUE) {
  oldpar <- par() ; par(cex=1.2)
  if (length(modelo$na.action) > 0)
      to.drop <- modelo$na.action
  #
  #  En mod.1$na.action están las observaciones omitidas.
  #
  fechas <- fechas[-to.drop]
  sel  <- match(unique(fechas),fechas)
  fechas <- fechas[sel]
  mod  <- preplot(modelo)[[comp]]
  x    <- unlist((mod$x),use.names=FALSE)
  y    <- unlist((mod$y),use.names=FALSE)
  se.y <- unlist((mod$se.y),use.names=FALSE)
  ICV  <- (cbind(y,y+1.96*se.y,y-1.96*se.y))[sel,]
  difs <- as.numeric(abs(fechas-as.Date(base)))
  orig <- head((1:length(fechas))[difs==min(difs)],1)    # posición de la fecha más próxima a la base deseada
  ICV  <- zoo(100*exp(ICV - as.numeric(ICV[orig,1])),order.by=fechas)
  if(plot==TRUE) {
    color <- ifelse(confint,"blue","white")
    plot(window(ICV[,2:3],start=min(fechas)),ylab=ylabel,sub=paste("Base: ",base,sep=" "),
       lty=2,col=color,plot.type="single",xlab="Date",ylim=c(79,125))
    lines(window(ICV[,1],start=min(fechas)),lty=1,lwd=2)
    if (rug) rug(x)
  }
  par(oldpar)
  #
  #  Antes de devolver el índice, que sólo está calculado para fechas que aparecían en
  #  el vector "fechas", vamos a completarlo para TODAS las fechas posibles entre la
  #  primera y la última. Eso garantiza que nos permitirá deflactar cualquier magnitud
  #  definida en el mismo intervalo de fechas, pero no necesariamente sobre las mismas
  #  abscisas temporales.
  #
  return( CompFechas(indice=ICV[,1:3], fechas=index(ICV)) )
}
if (!exists("bw")) bw <- 400


###################################################
### code chunk number 33: Indice1
###################################################
baseday <- "2005-01-01"
indice0 <- ConsInd(mod.1,base=baseday,comp="s(x, 12)",rug=FALSE,plot=FALSE,
		   fechas=Bilbao$Date,ylabel="Asking prices index")
#
indice0 <- indice0[,1]
plot(indice0,ylim=c(90,140),xlab="Date",ylab="Index")
mm <- zoo(Bilbao$Price/Bilbao$M2built,order.by=Bilbao$Date)
mmm <- aggregate(mm,by=function(x) as.Date(as.yearmon(x)), median)
lines(100*mmm/as.numeric(mmm[3]), lty=2, col="red")
legend("topright",c("Price index (Model 1)","Monthly median price"),
       lty=c(1,2),col=c(1,2),inset=0.02,ncol=1)


###################################################
### code chunk number 34: WithCoord
###################################################
Bilbao.g <- subset(Bilbao, !is.na(UTMX + UTMY))
scratch  <- with(Bilbao.g, log(Price/M2built))
Bilbao.g <- cbind(Bilbao.g, logPM2=scratch)


###################################################
### code chunk number 35: Anon7
###################################################
sel <- !is.na(Bilbao.g$Community)
Bilbao.g <- Bilbao.g[sel,]
scratch <- coredata(log(indice0/100))
s0 <- scratch - mean(scratch)


###################################################
### code chunk number 36: StartingValue
###################################################
r     <- range(Bilbao.g$Date)
dates <-  match(Bilbao.g$Date, seq(from=r[1],to=r[2],by="day"))
smooth.old <- s0[dates]       # smooth term from Model 1


###################################################
### code chunk number 37: BackFitting (eval = FALSE)
###################################################
## require(spgwr) ; gpclibPermit()
## tol <- 0.0035 ; iter <- 0 ; bw <- 400
## xy  <- as.matrix(Bilbao.g[,c("UTMX","UTMY")])
## repeat {
##   defl  <- Bilbao.g$logPM2 - smooth.old
##   mod2   <- gwr(defl ~ Bedrooms + Bathrooms + Elevator
## 	       + log(M2built) + Community + ParkingPlaces
## 	       + Heating + HouseType + Age,
## 	       coords=xy, bandwidth=bw, data=Bilbao.g)
##   resids     <- Bilbao.g$logPM2 - mod2$SDF@data$pred
##   smooth.new <- gam(resids ~ s(dates,12))$fitted.values
##   if  ( max(abs(smooth.new-smooth.old)) < tol )
##     break
##   smooth.old <- smooth.new ; iter <- iter + 1
## }


###################################################
### code chunk number 38: IndexBackfitting
###################################################
sel  <- match(unique(dates),dates)
ibf  <- zoo(smooth.new[sel], order.by=Bilbao.g$Date[sel])
base <- match(as.Date("2005-01-05"),index(ibf))
ibf  <- 100 * exp(ibf) / exp(coredata(ibf)[base])


###################################################
### code chunk number 39: ComparisonPlot
###################################################
plot(indice0, ylim=c(85,124))
lines(ibf,col="green",lwd=2,lty=2)
INE <- NA * ibf
INE[as.Date("2007-01-01")] <- 97.80
INE[as.Date("2007-04-01")] <- 100.498
INE[as.Date("2007-07-01")] <- 101.377
INE[as.Date("2007-10-01")] <- 100.324
INE[as.Date("2008-01-01")] <- 99.278
INE[as.Date("2008-04-01")] <- 99.255
INE[as.Date("2008-07-01")] <- 94.953
INE[as.Date("2008-10-01")] <- 92.260
INE[as.Date("2009-01-01")] <- 88.552
INE[as.Date("2009-04-01")] <- 86.746
INE[as.Date("2009-07-01")] <- 85.043
INE[as.Date("2009-10-01")] <- 85.751
INE[as.Date("2010-01-01")] <- 85.071
INE[as.Date("2010-04-01")] <- 86.29
INE[as.Date("2010-07-01")] <- 84.519
INE[as.Date("2010-10-01")] <- 84.573
INE[as.Date("2011-01-01")] <- 82.353
INE[as.Date("2011-04-01")] <- 80.480
INE[as.Date("2011-07-01")] <- 77.182
INE[as.Date("2011-10-01")] <- 74.272
INE[as.Date("2012-01-01")] <- 69.044
INE[as.Date("2012-04-01")] <- 69.044
INErebased <- INE  <- na.locf(INE)
adjust <- ( ibf[as.Date("2007-02-15")] + indice0[as.Date("2007-02-15")] ) /
	  ( 2 * INE[as.Date("2007-02-15")] )
coredata(INErebased) <- as.vector(coredata(INE) * adjust)
lines(INErebased,col="red",lwd=2)
legend(x="bottomleft",
       c("Model 1 (simple GAM)","Model 2 (GWR + backfitting)","INE Housing Price Index (IPV)"),
       lty=c(1,2,1),
       col=c(1,3,2),
       cex=1,
       inset=0.03,
       ncol=1)


###################################################
### code chunk number 40: Chapter10.Rnw:1489-1501
###################################################
proj4string(mod2$SDF) <- CRS("+init=epsg:23030")
mod2$SDF <- spTransform(mod2$SDF,CRS("+init=epsg:23030"))
#   CRS("+proj=utm +zone=30 +ellps=intl +units=m +no_defs")
#   writeOGR(mod2$SDF,dsn="mod1","pred",driver="ESRI Shapefile",verbose=TRUE)
#   mod2$SDF <- spTransform(mod2$SDF,CRS("+init=epsg:4326"))
#   CRS("+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")
#   writeOGR(mod2$SDF,dsn="mod1/mod1.kml","pred",driver="KML",verbose=TRUE)
fich <- "/home/etptupaf/ft/datos/Cartografia/Bilbao/BilbaoDistritos.shp"
xx <- readShapePoly(fich)
#   plot(xx,border="blue",axes=TRUE,las=1)
aaa <- mod2$SDF[!is.na(mod2$SDF$pred),]
aaa@data$pred <- exp(aaa@data$pred)


###################################################
### code chunk number 41: Anon9
###################################################
mi.tema <- trellis.par.get()
mi.tema$plot.symbol$cex <- 0.3
trellis.par.set(mi.tema)


###################################################
### code chunk number 42: SpPlot1
###################################################
val <- mod2$SDF[!is.na(mod2$SDF$pred),]
val@data$pred <- exp(val@data$pred)


###################################################
### code chunk number 43: SpPlot1c
###################################################
require(colorspace)
cuts   <- c(0,3000,3250,3750,4500,Inf)
colors <- diverge_hcl(length(cuts)-1)
fig    <- spplot(val,c("pred"), col.regions=colors,
		 cuts=cuts,cex=0.6, key.space="bottom",
		 scales=list(draw=TRUE),xlim=c(500000,510000),
		 ylim=c(4784500,4793500),
		 sp.layout=list("sp.polygons",Bilbao.utm))
print(fig)


###################################################
### code chunk number 44: SpPlot11
###################################################
cuts <- c(0.00,0.50,0.55,0.60,0.65,0.70,0.75,0.80,1.00)
colors <-  rev(diverge_hcl(length(cuts)-1))
map  <- spplot(mod2$SDF,c("localR2"), col.regions=colors, cuts=cuts,
	      key.space="bottom",cex=0.6, scales=list(draw=TRUE),
	      xlim=c(500000,510000),ylim=c(4784500,4793500),
	      sp.layout=list("sp.polygons", xx, col="black"))
print(map)


###################################################
### code chunk number 45: Limpieza
###################################################
# rm(list=ls())
# rm(list=setdiff(ls(),"Bilbao.g"))
dev.off()


