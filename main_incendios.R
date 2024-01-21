library(spatstat)
library(rpart)
library(sp)
library(maptools)
library(ggplot2)


S <- readShapePoints("./data/Puntos_Final.shp")
S$Class <-  as.factor(S$Class)
X<- as.ppp.SpatialPointsDataFrame(S[,"Class"])

incendios <- split(X)$'1'
plot(incendios)

inc <- rescale(incendios,10000)
Window(X)
plot(Window(incendios))
#---- tesselation #----
H <- hextess(inc,5)
hQ <- quadratcount(inc,tess = H)
plot(inc)
plot(hQ, add = T)
ThQ <- intensity(hQ,image = T)
plot(ThQ)

#----- cuadrant count #-----
C1 <- quadratcount(inc, 8,8)
intensity(C1)
plot(inc)
plot(C1, add = T)
plot(C1)
ThQ1 <- intensity(C1,image = T)
plot(ThQ1)

#----- cuadrant test #----
test1 <- quadrat.test(incendios, alternative = 'clustered')
#for example recorded spatial pattern of tress in three separate plots in the same forest.
# uniform intensity is performed by applying pool.quadranttest
test1 <- quadrat.test(inc, 3,alternative = 'clustered')#not X1,X2,X3
test2 <- quadrat.test(inc, 4,alternative = 'clustered')
test3 <- quadrat.test(inc, 5,alternative = 'clustered')
pool(test1,test2,test3)

#----- kernel estimations #-----
#by default uniform, diggle = T (Diggle corrected), set edge = F (un corrected)
den <- density(inc, sigma = 1, edge = F) #smoothing bandwidth is specified argument sigma
persp(den)
contour(den)
plot(den)

#----- bandwidth selecction #-----
#if not specified inte density.ppp, then sigma 1-8 (unsatisfactory)
#bw.diggle for Diggle and Berman mean square error assume a COx process
#         which is more clustered (correlation positive) than a Poisson Process  
#          cross-validation method
#bw.ppl for the likelihood cross-validation method assume an inhomogeneous Poisson Process
#data Swedish Pines data. more regular (correlation negative) than a Poisson process.

b <- bw.ppl(inc)
b
bw.diggle(inc)
bw.scott(inc) #multidimesional smoothing
plot(b)
plot(b, xlim=c(0,5))

D <- density(inc, sigma = bw.diggle)
plot(D)

#---- estimation of intensity at the data points #-----
#sometimes required to estimate values lambda(xi) at the data points xi themselves.
dX <- density(inc, sigma = bw.diggle, at = 'points')
plot(dX)
dX[100:105]

#---- spatially adaptive smoothing #-----
#adaptive estimators of intensity can be based on Dirichlet-Voronot tessellations
#argument f=1
vden <- adaptive.density(inc, f=1)
plot(vden)

#fraction f of the points in the point pattern are selected at random, and used to construct
#a Dirichket tessellation. A quadrant counting of the intensity is based on this tessellation
#this process is repeated 'nrep' times and the results are averaged.
aden <- adaptive.density(inc, f=0.1, nrep=30)
plot(aden)
plot(inc,add = T)

#another strategy is to measure the distance R=d(u,x), u fixed point to the nearest xi.
#and calculate the area A=piR^2.
#maximun likelihoood estimade of lambda based or R is lambda_k = k/(piR^2_k) set.
nden <- nndensity(inc, k=10)
plot(nden)
plot(inc, add = T)


#--------correlation #------
inc <- rescale(inc,1000)
#fryplot(inc)
#plot(frypoints(inc))

Ks <- Kest(inc)

Ki <- Kest(inc, correction = 'isotropic')
plot(Ki)
Kb <- Kest(inc, correction = 'border')
plot(Kb)
Kt <- Kest(inc, correction = 'translation')
plot(Kt)

L <- Lest(inc, correction = 'border')
plot(L)

#---- lack correlation does not prove independence
Xcell <- rcell(nx=15)
plot(Xcell)

plot(Ki, iso~r)

plot(Ks,./theo ~r)

plot(Ks,. ~ theo)

lambda <- intensity(inc)
plot(Ks, lambda * . ~ r)

#Ko <- subset(Ks, r<0.5, select=-border)
#plot(Ko)

#---- convert to a true function #----
Ks <- Kest(inc, correction = 'isotropic')
K <- as.function(Ks,value=".")
K(9)

y <- with(Ks,iso-theo)
x <- with(Ks, r)

plot(x,y)

with(Ks, max(abs(iso-theo)))

#---- estimating the pair correlation function #-----
g <- pcf(inc)
plot(g)

#---- standard errors and confidence intervalas #-----
#---- block bootstrap #----
Kvb <- varblock(inc, Kest, correction='border', nx=3, ny=3)
plot(Kvb)

#--- loh's bootstrap #-----
kloh <- lohboot(inc, Kest)
plot(kloh)

#---- L-function#----
#global 95% confidence interval for the tru L-function
Lg <- lohboot(inc, Lest, global = T)
plot(Lg)

#back-transformed  confiedence interval for K-function
#using Loh´s bootstrap
Kg <- eval.fv(pi*Lg^2)
plot(Kg)

#---- non-graphical test #----
mad.test(inc, Lest, nsim = 99, rmax=2, use.theo=T)

dclf.test(inc, Lest, nsim =99, rmax=2,use.theo=T)$p.value
#------ INVESTIGATING DEPENDENCE OF INTENSITY ON A COVARIATE #-----


#con todos las variables
X1<- as.ppp.SpatialPointsDataFrame(S)
names(marks(X1))

# Split solo con incendios
incendios <- split(X1, "Class", un = T)
incendios <- incendios$`1`
plot(incendios)

# split con marca y todas las variables
data_incendios <- split(X1, "Class", un = F)
plot(data_incendios)

data_incendios <- data_incendios$`1`

#----- agrupacion espacio temporal #----
names(marks(data_incendios))

table(marks(data_incendios)$ANIO)
hist(marks(data_incendios)$ANIO, freq = T)

table(marks(data_incendios)$MES)
plot(table(marks(data_incendios)$MES))

data_mes_incendios <- split(data_incendios,"MES", un = TRUE)
plot(data_mes_incendios)

plot(density(data_mes_incendios))

data_nivel_incendios <- split(data_incendios,"NIVEL1", un = TRUE)
plot(data_nivel_incendios)
