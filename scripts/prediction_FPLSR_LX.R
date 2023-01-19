
#
#
#

library(fda)
library(ftsa)
library(fpcb)
library(glue)
library(dplyr)
library(data.table)
library(MLmetrics)

library(partitionedsurv)


df <- fread(file = glue({path},'/Data/data.txt'))
df[which(df$Age=='110+'), 2] <- 110


############################################################
####################### MODEL FOR lx #######################
############################################################

lx <- dcast(df, factor(Age, levels = unique(df$Age)) ~ factor(Year, levels = unique(df$Year)), 
            value.var = 'lx')

lx <- lx[,-1]
lx <- t(lx)
Age <- unique(df$Age)

colnames(lx) <- as.character(0:110)
rownames(lx) <- as.character(1922:2020)

##### Model for mx ######
data <- lx
par(mfrow=c(1,1))
matplot(t(data),type='l', ylab = '1+log(mx)',xlab ='Age', cex.lab = 1.5, col='gray')

N <- nrow(data)
p <- ncol(data)
t <- seq(0, 1, length.out = p) 
npc <- 2:30

########### training - validation and testing sets ###########
train <- 0.8
valid <- 0.2

########################### MACRO ESPERIMENT ###########################

# TRAINING DATA SETS
data.train  <- data[1:round(N*(train+valid)), ] # datos para entrenar y validar
htrain      <- round(N*valid)                   # horizonte de prediccion para validar
n           <- dim(data.train)[1]               # cantidad de curvas para entrenar y validar
ntrain      <- n-htrain                         # cantidad de curvas para entrenar
RMSE.fplsr  <- matrix(0,htrain,length(npc))

for (k in (ntrain+1):n) {
  for (j in 1:length(npc)) {
    
    #### modelo y prediccion fplsr
    dataset <- rainbow::fts(y=t(data[1:(k-1), ]),x=t)
    modelo.FPLSR  <- ftsa::fplsr(data = dataset,order = npc[j], interval = FALSE)
    predict_fplsr <- modelo.FPLSR$Ypred$y
    RMSE.fplsr[k-ntrain,j] <- MLmetrics::RMSE(predict_fplsr,data[k, ])
  }
  print(paste0('~~~~~~~~~~~N+',k-ntrain,' validation step~~~~~~~~~~~'))
}

print('~~~~~~ENTRENAMIENTO COMPLETADO~~~~~~')

# hiperparameters FPLSR
av.RMSE.fplsr <- colMeans(RMSE.fplsr)
kn.fplsr <- npc[which.min(av.RMSE.fplsr)]
kn.fplsr <- 4

par(mar=c(5,5,3,3))
plot(npc,av.RMSE.fplsr, xlab='Components', ylab='RMSEP', pch = 19, col= 'slategray',
     bty='n', xaxt = 'n', cex.lab = 1.5, cex.axis = 1.25)
axis(1, at = npc, labels = npc)
abline(v=npc[which.min(av.RMSE.fplsr)],col='gray',lty=2,lwd=1.5)
points(npc[which.min(av.RMSE.fplsr)],min(av.RMSE.fplsr),col='darkred',pch=19,cex=1.25)
points(npc[which.min(av.RMSE.fplsr)],min(av.RMSE.fplsr),col='blue',pch=1,cex=2, lwd=2)
mtext(text = paste0('NC = ',kn.fplsr), side=3,line=-2, at =which.min(av.RMSE.fplsr)+4, font=2)

############ OUT OF THE SAMPLE FORECAST ##############

h <- 5
os_fplsr <- matrix(0,h,length(t))
fplsr.data <- data

for (k in (N+1):(N+h)) {
  #### modelo y prediccion FPLSR
  dataset<- rainbow::fts(y=t(fplsr.data),x=t)
  modelo.FPLSR   <- ftsa::fplsr(data = dataset,order =kn.fplsr, interval = FALSE)
  os_fplsr.aux   <- modelo.FPLSR$Ypred$y
  os_fplsr[k-N,] <- os_fplsr.aux
  fplsr.data     <- rbind(fplsr.data,t(as.matrix(os_fplsr.aux)))
}
  
h.years <- (as.numeric(rownames(data)[length(rownames(data))])+1):(as.numeric(rownames(data)[length(rownames(data))])+h)
colnames(fplsr.data)[(N+1):(N+h)] <- h.years
dataset <- rainbow::fts(y=t(fplsr.data[(N+1):(N+h),]),x=t)

rownames(fplsr.data) <- NULL
write.table(fplsr.data, file = paste0(path,'Data/lx_',h.years[1],'-',h.years[length(h.years)],'.txt'))

############ The End ! #################

pdf(file = paste0(path,"FTS-pred.pdf"),width=8,height=6,paper='special')
par(mfrow=c(1,1))
matplot(t,t(fplsr.data),type='l',col='gray',lty=1, ylab='1+log(mx)',main='FPLSR model', cex.axis=1.25,
        cex.lab=1.5)
matlines(t,t(fplsr.data[c((N+1):(N+h)),]),type='l',col=c('red','green','blue','yellow','black'),lty=1)
legend('bottomleft', legend=c('1922-2020',h.years),
       lty=c(1), col=c('gray','red','green','blue','yellow','black'),bty='n', cex=0.6)
dev.off()
