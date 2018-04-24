#Plot: Elevations for map

base_dir <- "u:/RhineFlow/Elevation/"

stationMeta <- read.table(paste0(base_dir,"rawData/IDAweb/stationMeta.csv"), sep=",", header=T)

pdf(paste0(base_dir,"mapElev.pdf"), width=3 , height=1.8)

eles <- stationMeta[length(stationMeta$alt):1, c(3,6)]

par(mar=c(1, 2,0.2,0.2))
plot(eles$alt, type="n", axes=F, ylab="", xlab="", yaxs="i", xaxs="i", ylim=c(0,3750), xlim=c(0,nrow(eles)+1))
lines(which(eles$category =="low"),    eles$alt[which(eles$category =="low")],    type="h", col="red3",  lwd=2,  lend=1)
lines(which(eles$category =="middle"), eles$alt[which(eles$category =="middle")], type="h", col="black", lwd=2,  lend=1)
lines(which(eles$category =="high"),   eles$alt[which(eles$category =="high")],   type="h", col="blue3", lwd=2,  lend=1)
abline(v=length(which(eles$category =="low"))+0.5, lwd=1)
abline(v=length(which(eles$category =="low"))+length(which(eles$category =="middle"))+0.5, lwd=1)

text(23, 1550, "low",    col="red3",  cex=0.7)
text(23, 1250, paste0("(", length(which(eles$category =="low")),")"),    col="red3",  cex=0.7)
text(65, 2350, "middle", col="black", cex=0.7)
text(65, 2050, paste0("(", length(which(eles$category =="middle")),")"), col="black", cex=0.7)
text(88, 3450, "high",   col="blue3", cex=0.7)
text(88, 3150, paste0("(", length(which(eles$category =="high")),")"),   col="blue3", cex=0.7)

axis(1, mgp=c(3, 0.0, 0), tck=-0.02, cex.axis=0.7)
axis(2, mgp=c(3, 0.1, 0), tck=-0.02, cex.axis=0.7)
mtext("Altitude [m]", side=2, line=1.0, adj=0.5, cex=0.7)
box()

par(xpd=TRUE)
lines(c(0,0),c(min_na(eles$alt[which(eles$category =="high")]),   max_na(eles$alt[which(eles$category =="high")])),   type="l", col="blue3", lwd=2,  lend=1)
lines(c(0,0),c(min_na(eles$alt[which(eles$category =="middle")]), max_na(eles$alt[which(eles$category =="middle")])), type="l", col="black", lwd=2,  lend=1)
lines(c(0,0),c(min_na(eles$alt[which(eles$category =="low")]),    max_na(eles$alt[which(eles$category =="low")])),    type="l", col="red3",  lwd=2,  lend=1)
par(xpd=FALSE)

dev.off()
