
library(fda); library(ftsa); library(fpcb); library(glue)
library(dplyr); library(data.table); library(MLmetrics)

path = '/Users/nico/OneDrive - University College London/FDA-partitioned-surv/'
source(glue({path},'/scripts/aux_functions.R'))

df <- fread(file = glue({path},'/Data/data.txt'))
df[which(df$Age=='110+'),2]=110

mx <- dcast(df, factor(Age, levels = unique(df$Age)) ~ factor(Year, levels = unique(df$Year)), 
            value.var = 'mx')

mx <- mx[,-1]
mx = t(mx)
Age = unique(df$Age)

colnames(mx) <- as.character(0:110)
rownames(mx) <- as.character(1922:2020)

##### Model for mx ######
data = 1+log(mx)
par(mfrow=c(1,1))
matplot(t(data),type='l', ylab = '1+log(mx)',xlab ='Age', cex.lab = 1.5, col='gray')

N = nrow(data); p = ncol(data)
t = seq(0, 1,length.out = p) 
sigmas <- c(0.1^2,0.1,1,5,7.5,10,50,100)
basis.func <- 2:30

Theta = expand.grid(sigmas,basis.func)
colnames(Theta)<-c('Sigmas', 'Basis')

########### training - validation and testing sets ###########
train<-0.6; valid<-0.2; test<-0.2 

########################### MACRO ESPERIMENT ###########################
# TRAINING DATA SETS
data.train  <- data[1:round(N*(train+valid)),] # datos para entrenar y validar
htrain      <- round(N*valid) # horizonte de prediccion para validar
n           <- dim(data.train)[1] # cantidad de curvas para entrenar y validar
ntrain      <- n-htrain # cantidad de curvas para entrenar

# TRAINING EMPTY MATRICES
RMSEP_rkhs <- array(0,dim=c(htrain,length(basis.func),length(sigmas)))

for (k in (ntrain+1):n) {
  
  for (j in 1:nrow(Theta)){
    
    #### modelo y prediccion rkhs
    kernel <- fpcb::rk(grid = t, r=Theta$Basis[j], sigma = Theta$Sigmas[j])
    curves <- fpcb::fdata_rkhs(curves = data[1:(k-1), ], rk = kernel)
    model <- fpcb::arh_rkhs(curves)
    predict.rkhs <- fpcb::predict_rkhs(model, bands = FALSE)
    
    id.basis <- which(Theta$Basis[j]==basis.func)
    id.sigma <- which(Theta$Sigmas[j]==sigmas)
    
    RMSEP_rkhs[k-ntrain,id.basis,id.sigma]  <- MLmetrics::RMSE(predict.rkhs$forecast,data.train[k,])
    
  }
  print(paste0('~~~~~~~~~~~N+',k-ntrain,' validation step~~~~~~~~~~~'))
}

print('~~~~~~ENTRENAMIENTO COMPLETADO~~~~~~')

print('~~~~~~COMIENZA EL TESTEO~~~~~~')

# av.cov.train.95<-colMeans(cov.train.95)
# colnames(av.cov.train.95)<-sigmas
# rownames(av.cov.train.95)<-basis.func
# 
# av.pl.train.95<-colMeans(pl.train.95)
# colnames(av.pl.train.95)<-sigmas
# rownames(av.pl.train.95)<-basis.func

av.RMSEP_rkhs = colMeans(RMSEP_rkhs)
colnames(av.RMSEP_rkhs)<-sigmas
rownames(av.RMSEP_rkhs)<-basis.func

sigma.opt = sigmas[which(av.RMSEP_rkhs==min(av.RMSEP_rkhs),arr.ind=T)[2]] 
basis.opt = basis.func[which(av.RMSEP_rkhs==min(av.RMSEP_rkhs),arr.ind=T)[1]] 

#################### TEST ######################


# TESTING DATA SET
htest <- round(N*valid)
ntest <- N-htest

# TESTING METRICS
RMSEP_rkhs_test <- matrix(0,htest,1)

for (k in (ntest+1):N) {

data.test <- data[1:(k-1), ]
test.curve <- t(as.matrix(data[k, ]))

kernel       <- fpcb::rk(grid = t, r=basis.opt, sigma = sigma.opt)
curves       <- fpcb::fdata_rkhs(curves = data, rk = kernel)
model        <- fpcb::arh_rkhs(curves)
predict.rkhs <- fpcb::predict_rkhs(model, bands = FALSE)

RMSEP_rkhs_test[k-ntest,1] <- MLmetrics::RMSE(predict.rkhs$forecast,test.curve)

print(paste0('~~~~~~~~~~~N+',k-ntest,' testing step~~~~~~~~~~~'))
}

#################### MODEL AND PREDICTION WITH FAR AND FPLRS ####################
  
RMSEP.fplsr = RMSEP.far = matrix(0,htrain,length(basis.func))

for (k in (ntrain+1):n) {
  
  for (j in 1:length(basis.func)){
    
  #### modelo y prediccion FPLSR
  dataset<- rainbow::fts(y=t(data.train[1:(k-1),]),x=t)
  modelo.FPLSR  <- ftsa::fplsr(data = dataset,order = basis.func[j], interval = FALSE)
  predict_fplsr <- modelo.FPLSR$Ypred$y
  RMSEP.fplsr[k-ntrain,j]  <- MLmetrics::RMSE(predict_fplsr,as.matrix(data.train)[k,])

  #### modelo y prediccion FAR 
  far.data         <- far::as.fdata(t(data.train[1:(k-1),]),p=length(t))
  fit              <- far::far(data=far.data, y="var",kn=basis.func[j], na.rm=FALSE)
  new.far.data     <- far::as.fdata(t(data.train[1:(k-1),]),p=length(t))
  predict_far      <- far::predict.far(fit, newdata = far.data,na.rm=FALSE)$var[,k-1]
  RMSEP.far[k-ntrain,j] <- MLmetrics::RMSE(predict_far,as.matrix(data.train)[k,])

  }
  
  print(paste0('~~~~~~~~~~~N+',k-ntrain,' validation step~~~~~~~~~~~'))
}

# hiperparameters FPLSR
av.RMSEP.fplsr<-colMeans(RMSEP.fplsr)
kn.fplsr<-basis.func[which.min(av.RMSEP.fplsr)]

# hiperparameters FAR
av.RMSEP.far<-colMeans(RMSEP.far)
kn.far<-basis.func[which.min(av.RMSEP.far)]

RMSEP.test <- matrix(0,htest,2)

for (k in (N-htest+1):N){
  
  #### modelo y prediccion FPLSR
  dataset<- rainbow::fts(y=t(data[1:(k-1),]),x=t)
  modelo.FPLSR  <- ftsa::fplsr(data = dataset,order =kn.fplsr, interval = FALSE)
  predict_fplsr <- modelo.FPLSR$Ypred$y
  RMSEP.test[k-(N-htest),1]  <- MLmetrics::RMSE(predict_fplsr,as.matrix(data)[k,])
  
  #### modelo y prediccion FAR 
  far.data         <- far::as.fdata(t(data[1:(k-1),]),p=length(t))
  fit              <- far::far(data=far.data, y="var",kn=kn.far, na.rm=FALSE)
  new.far.data     <- far::as.fdata(t(data[1:(k-1),]),p=length(t))
  predict_far      <- far::predict.far(fit, newdata = far.data,na.rm=FALSE)$var[,k-1]
  RMSEP.test[k-(N-htest),2] <- MLmetrics::RMSE(predict_far,as.matrix(data)[k,])
}

RMSEP.mx <- cbind(RMSEP_rkhs_test,RMSEP.test)
colnames(RMSEP.mx) <-c('Kernel','FPLSR','FPCA')

##################################################################
###################### MODEL FOR lx ##############################
##################################################################

df <- fread(file = glue({path},'/Data/data.txt'))
df[which(df$Age=='110+'),2]=110

lx <- dcast(df, factor(Age, levels = unique(df$Age)) ~ factor(Year, levels = unique(df$Year)), 
            value.var = 'lx')

lx <- lx[,-1]
lx = t(lx)
Age = unique(df$Age)

colnames(lx) <- as.character(0:110)
rownames(lx) <- as.character(1922:2020)

##### Model for mx ######
data = lx
par(mfrow=c(1,1))
matplot(t(data),type='l', ylab = '1+log(mx)',xlab ='Age', cex.lab = 1.5, col='gray')

N = nrow(data); p = ncol(data)
t = seq(0, 1,length.out = p) 
sigmas <- c(0.1^2,0.1,1,5,7.5,10,50,100)
basis.func <- 2:30

Theta = expand.grid(sigmas,basis.func)
colnames(Theta)<-c('Sigmas', 'Basis')

########### training - validation and testing sets ###########
train<-0.6; valid<-0.2; test<-0.2 

########################### MACRO ESPERIMENT ###########################
# TRAINING DATA SETS
data.train  <- data[1:round(N*(train+valid)),] # datos para entrenar y validar
htrain      <- round(N*valid) # horizonte de prediccion para validar
n           <- dim(data.train)[1] # cantidad de curvas para entrenar y validar
ntrain      <- n-htrain # cantidad de curvas para entrenar

# TRAINING EMPTY MATRICES
RMSEP_rkhs <- array(0,dim=c(htrain,length(basis.func),length(sigmas)))

for (k in (ntrain+1):n) {
  
  for (j in 1:nrow(Theta)){
    
    #### modelo y prediccion rkhs
    kernel <- fpcb::rk(grid = t, r=Theta$Basis[j], sigma = Theta$Sigmas[j])
    curves <- fpcb::fdata_rkhs(curves = data[1:(k-1), ], rk = kernel)
    model <- fpcb::arh_rkhs(curves)
    predict.rkhs <- fpcb::predict_rkhs(model, bands = FALSE)
    
    id.basis <- which(Theta$Basis[j]==basis.func)
    id.sigma <- which(Theta$Sigmas[j]==sigmas)
    
    RMSEP_rkhs[k-ntrain,id.basis,id.sigma]  <- MLmetrics::RMSE(predict.rkhs$forecast,data.train[k,])
    
  }
  print(paste0('~~~~~~~~~~~N+',k-ntrain,' validation step~~~~~~~~~~~'))
}

print('~~~~~~ENTRENAMIENTO COMPLETADO~~~~~~')

print('~~~~~~COMIENZA EL TESTEO~~~~~~')

av.RMSEP_rkhs = colMeans(RMSEP_rkhs)
colnames(av.RMSEP_rkhs)<-sigmas
rownames(av.RMSEP_rkhs)<-basis.func

sigma.opt = sigmas[which(av.RMSEP_rkhs==min(av.RMSEP_rkhs),arr.ind=T)[2]] 
basis.opt = basis.func[which(av.RMSEP_rkhs==min(av.RMSEP_rkhs),arr.ind=T)[1]] 

#################### TEST ######################

# TESTING DATA SET
htest <- round(N*valid)
ntest <- N-htest

# TESTING METRICS
RMSEP_rkhs_test <- matrix(0,htest,1)

for (k in (ntest+1):N) {
  
  data.test <- data[1:(k-1), ]
  test.curve <- t(as.matrix(data[k, ]))
  
  kernel       <- fpcb::rk(grid = t, r=basis.opt, sigma = sigma.opt)
  curves       <- fpcb::fdata_rkhs(curves = data, rk = kernel)
  model        <- fpcb::arh_rkhs(curves)
  predict.rkhs <- fpcb::predict_rkhs(model, bands = FALSE)
  
  RMSEP_rkhs_test[k-ntest,1] <- MLmetrics::RMSE(predict.rkhs$forecast,test.curve)
  
  print(paste0('~~~~~~~~~~~N+',k-ntest,' testing step~~~~~~~~~~~'))
}

#################### MODEL AND PREDICTION WITH FAR AND FPLRS ####################

RMSEP.fplsr = RMSEP.far = matrix(0,htrain,length(basis.func))

for (k in (ntrain+1):n) {
  
  for (j in 1:length(basis.func)){
    
    #### modelo y prediccion FPLSR
    dataset<- rainbow::fts(y=t(data.train[1:(k-1),]),x=t)
    modelo.FPLSR  <- ftsa::fplsr(data = dataset,order = basis.func[j], interval = FALSE)
    predict_fplsr <- modelo.FPLSR$Ypred$y
    RMSEP.fplsr[k-ntrain,j]  <- MLmetrics::RMSE(predict_fplsr,as.matrix(data.train)[k,])
    
    #### modelo y prediccion FAR 
    far.data         <- far::as.fdata(t(data.train[1:(k-1),]),p=length(t))
    fit              <- far::far(data=far.data, y="var",kn=basis.func[j], na.rm=FALSE)
    new.far.data     <- far::as.fdata(t(data.train[1:(k-1),]),p=length(t))
    predict_far      <- far::predict.far(fit, newdata = far.data,na.rm=FALSE)$var[,k-1]
    RMSEP.far[k-ntrain,j] <- MLmetrics::RMSE(predict_far,as.matrix(data.train)[k,])
    
  }
  
  print(paste0('~~~~~~~~~~~N+',k-ntrain,' validation step~~~~~~~~~~~'))
}

# hiperparameters FPLSR
av.RMSEP.fplsr<-colMeans(RMSEP.fplsr)
kn.fplsr<-basis.func[which.min(av.RMSEP.fplsr)]

# hiperparameters FAR
av.RMSEP.far<-colMeans(RMSEP.far)
kn.far<-basis.func[which.min(av.RMSEP.far)]

RMSEP.test <- matrix(0,htest,2)

for (k in (N-htest+1):N){
  
  #### modelo y prediccion FPLSR
  dataset<- rainbow::fts(y=t(data[1:(k-1),]),x=t)
  modelo.FPLSR  <- ftsa::fplsr(data = dataset,order =kn.fplsr, interval = FALSE)
  predict_fplsr <- modelo.FPLSR$Ypred$y
  RMSEP.test[k-(N-htest),1]  <- MLmetrics::RMSE(predict_fplsr,as.matrix(data)[k,])
  
  #### modelo y prediccion FAR 
  far.data         <- far::as.fdata(t(data[1:(k-1),]),p=length(t))
  fit              <- far::far(data=far.data, y="var",kn=kn.far, na.rm=FALSE)
  new.far.data     <- far::as.fdata(t(data[1:(k-1),]),p=length(t))
  predict_far      <- far::predict.far(fit, newdata = far.data,na.rm=FALSE)$var[,k-1]
  RMSEP.test[k-(N-htest),2] <- MLmetrics::RMSE(predict_far,as.matrix(data)[k,])
}

RMSEP.lx <- cbind(RMSEP_rkhs_test,RMSEP.test)
colnames(RMSEP.lx) <-c('Kernel','FPLSR','FPCA')

RMSEP.mx = as.data.table(RMSEP.mx)
RMSEP.lx = as.data.table(RMSEP.lx)

RMSEP.mx[, type := rep('mx',nrow(RMSEP.mx))]
RMSEP.lx[, type := rep('lx',nrow(RMSEP.lx))]

RMSEP <- bind_rows(RMSEP.mx,RMSEP.lx) 

datos <- RMSEP %>% as.data.table %>% 
  .[, id := 1:nrow(.)] %>% 
  melt(id = c('id','type'), variable.name = 'Models')


col <- c(scales::hue_pal()(5))
pdf(file = paste0(path,'Figures/RMSEP.pdf'),width=10,height=6,paper='special')
print({  
  datos %>% ggplot(aes(Models, y= value, fill=Models)) + 
    #geom_boxplot(outlier.shape = NA) +
    geom_boxplot() +
    theme_bw() +
    facet_wrap(~type, scales = "free_y", 
             labeller = as_labeller(c(lx = "Persons (in thousands) surviving to age-cohort", 
                                      mx = "Log-Mortality rate")))+
    ylab('RMSEP') +
    xlab(NULL) +
    scale_color_manual(values = col)+
    theme(strip.text = element_text(face='bold')) +
    theme(legend.position="bottom") +
    theme(legend.title=element_blank())+
    theme(strip.placement = "outside") +
    theme(axis.text.y = element_text(size=12, color="black"),
          axis.text.x = element_text(size=12, color="black")) +
    theme(axis.title.y = element_text(size=15, color="black")) +
    theme(axis.title.x = element_text(size=15, color="black")) +
    theme(legend.text = element_text(face='bold'))+
    guides(color=guide_legend(nrow=1, byrow=TRUE))
})
dev.off()
