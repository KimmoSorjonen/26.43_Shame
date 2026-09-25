

############################# BIENVENUE #############################

############# I SOLEMNLY SWEAR THAT I AM UP TO NO GOOD ##############

## Loading package

library(MASS)
library(metafor)
library(lavaan)

#################
## Data
## Correlations reported in Chen et al. (2026)
## order: shame 1-3, loneliness 1-3, injury 1-3

rm <- matrix(c(
  
  1.00, 0.64, 0.54, 0.40, 0.34, 0.27, 0.33, 0.28, 0.14,
  0.64, 1.00, 0.65, 0.34, 0.42, 0.37, 0.29, 0.29, 0.22,
  0.54, 0.65, 1.00, 0.33, 0.38, 0.43, 0.29, 0.36, 0.33,
  0.40, 0.34, 0.33, 1.00, 0.55, 0.47, 0.32, 0.24, 0.21,
  0.34, 0.42, 0.38, 0.55, 1.00, 0.68, 0.22, 0.30, 0.26,
  0.27, 0.37, 0.43, 0.47, 0.68, 1.00, 0.19, 0.27, 0.31,
  0.33, 0.29, 0.29, 0.32, 0.22, 0.19, 1.00, 0.41, 0.25,
  0.28, 0.29, 0.36, 0.24, 0.30, 0.27, 0.41, 1.00, 0.40,
  0.14, 0.22, 0.33, 0.21, 0.26, 0.31, 0.25, 0.40, 1.00), nrow=9)

n <- 664  ## sample size

dfall <- data.frame(mvrnorm(n=n, ## generating data frame with required size and corr.
            mu=rep(0,9), Sigma=rm, empirical=T))

df.sl <- dfall[,1:6] ## shame and loneliness
df.si <- dfall[,c(1:3,7:9)] ## shame and injury
df.li <- dfall[,4:9] ## loneliness and injury

dflist <- list(df.sl,df.si,df.li) ## list with all three combinations

## The six models, with dy1 = Y2-Y1 and dy2 = Y3-Y2

m1 <- "dy1 ~ x1 + y1"
m2 <- "dy2 ~ x2 + y2"
m3 <- "dy1 ~ x1 + y2"
m4 <- "dy2 ~ x2 + y3"
m5 <- "dy1 ~ x1"
m6 <- "dy2 ~ x2"

modlist <- list(m1,m2,m3,m4,m5,m6) ## list with all six models

#################
## Figure

panlab <- c("Shame → Loneliness", "Loneliness → Shame",
            "Shame → Injury", "Injury → Shame",
            "Loneliness → Injury", "Injury → Loneliness")

slab <- c("7.RMA","",
          "6.y3-y2.no.adj", "5.y2-y1.no.adj", ## y-labels
          "4.y3-y2.adj.y3", "3.y2-y1.adj.y2", 
          "2.y3-y2.adj.y2", "1.y2-y1.adj.y1")

cx <- 0.8 ## sizing factor
r.low <- -0.3 ## range, lower
r.upp <- 0.3 ## range, upper
f.upp <- r.upp+0.8*(r.upp-r.low) ## room for text
tic1 <- 0.1 ## distance, tics
tic2 <- 0.3 ## distance, labels

if(dev.cur()==2) dev.off() ## removing earlier plots
par(mar=c(1,1,1.2,0), oma=c(1.5,4.5,0.5,1), mfrow=c(3,2)) ## setting margins and layout

pl <- 1 ## keeping track of panels

for(k in 1:3){ ## three rows of panels
  
  df <- dflist[[k]] ## picking the data
  
  for(i in 1:2){ ## two orders
    
    if(i==1) names(df) <- c("x1","x2","x3","y1","y2","y3") ## order 1
    if(i==2) names(df) <- c("y1","y2","y3","x1","x2","x3") ## order 2
    
    dy1 <- df$y2 - df$y1 ## difference score 1
    dy2 <- df$y3 - df$y2 ## difference score 2
    
    numeff <- 8 ## number of rows per panel (one is empty)
    
    plot(c(r.low,f.upp),c(0.5,8.5), type="n",xaxt="n",yaxt="n",xlab="",ylab="") ## empty plot
    
    mtext(panlab[pl],3, line=0.2, cex=cx) ## panel label
    
    axis(1,at=seq(r.low,r.upp,tic1),labels=F, cex.axis=cx) ## x-axis
    if(k==3) axis(1,at=seq(r.low,r.upp,tic2),labels=seq(r.low,r.upp,tic2), cex.axis=cx) ## x-labels
    axis(2,at=1:8,labels=F, las=1, cex.axis=cx) ## y-tics
    if(i==1) axis(2,at=1:8,labels=slab, las=1, cex.axis=cx) ## y-labels
    
    lines(c(0,0),c(-1,15),col="gray") ## vertical gray line at x=0
    
    ball <- vector() ## to be filled with effects below
    seall <- vector() ## to be filled with standard errors below
    
    for(j in 1:6){ ## for the six models
      
      fit <- lm(modlist[[j]], data=df) ## fitting the model
      b <- fit$coefficients[2] ## reg. coefficient
      low <- confint(fit)[2,1] ## CI, low
      upp <- confint(fit)[2,2] ## CI, upp
      se <- summary(fit)$coefficients[2,2] ## standard error
      tx <- paste(round(b,2)," [", round(low,2),"; ", 
                  round(upp,2),"]", sep="") ## string with values
      
      ball <- c(ball,b) ## adding effect to object
      seall <- c(seall,se) ## adding se to object
      
      lines(c(-2,r.upp),c(numeff,numeff), col="gray") ## vertical gray line
      arrows(low,numeff,upp,numeff,angle=90,code=3,length=0.05,lwd=2) ## CI
      points(b,numeff,pch=21,col="black",bg="black") ## point for effect
      text(r.upp,numeff,tx,pos=4,cex=cx) ## adding string with values
      
      numeff <- numeff-1 ## next row, please
    }
    
    ## Meta-analysis of the six effects
    
    b.fish <- 0.5*log((1+ball)/(1-ball)) ## Fisher's transformation of effects
    se.fish <- 0.5*log((1+seall)/(1-seall)) ## Fisher's transformation of SE
    
    ###### Following Bartos et al.
    
    we <- rep(1/6,6) ## equal weight to all effects
    
    random1 <- rma(yi=b.fish, vi=se.fish^2)
    res.ma <- rma(yi=b.fish, vi=(se.fish^2)/we, tau=random1$tau2)
    
    ######
    
    pred <- predict(res.ma, transf=transf.ztor) ## transforms back from Fisher's
    metb <- pred$pred ## estimate
    metlow <- pred$ci.lb ## lower CI
    metupp <- pred$ci.ub ## upper CI
    
    ##
    
    lines(c(-2,4),c(2,2),lty=2) ## dashed line
    lines(c(-2,r.upp),c(1,1),col="gray") ## gray vertical line
    
    polygon(c(metlow,metb,metupp,metb), c(1,1.5,1,0.5), ## diamond for RMA 
            col = "black", border = "black", lwd = 1)
    
    tx.ma <- paste(round(metb,2)," [", round(metlow,2),"; ", 
                   round(metupp,2),"]", sep="") ## string with values
    text(r.upp,1,tx.ma,pos=4,cex=cx) ## adding string of values
    legend("topleft", title=LETTERS[pl], legend="", bty="n", inset=0, cex=1.7*cx) ## A-B legend
    
    pl <- pl+1
  }
}

####################
## Alternative model

alt <- "

## Trait

gs =~ 1*s1+1*s2+1*s3
gl =~ 1*l1+1*l2+1*l3
gi =~ 1*i1+1*i2+1*i3

cse =~ -1*gs+start(-0.5)*gl+start(-0.5)*gi

## State

st1 =~ ss*s1+sl*l1+si*i1
st2 =~ ss*s2+sl*l2+si*i2
st3 =~ ss*s3+sl*l3+si*i3
 
st1 ~~ rst*st2
st2 ~~ rst*st3

## (Error) variances

s1 ~~ s1
s2 ~~ s2
s3 ~~ s3

l1 ~~ l1
l2 ~~ l2
l3 ~~ l3

i1 ~~ i1
i2 ~~ i2
i3 ~~ i3

gs ~~ gs
gl ~~ gl
gi ~~ gi

cse ~~ cse

st1 ~~ 1*st1
st2 ~~ 1*st2
st3 ~~ 1*st3

## Intercepts

s1 ~ 0*1
s2 ~ 0*1
s3 ~ 0*1

l1 ~ 0*1
l2 ~ 0*1
l3 ~ 0*1

i1 ~ 0*1
i2 ~ 0*1
i3 ~ 0*1

gs ~ 0*1
gl ~ 0*1
gi ~ 0*1

cse ~ 0*1

st1 ~ 0*1
st2 ~ 0*1
st3 ~ 0*1

"

df <- dfall
names(df) <- c("s1","s2","s3","l1","l2","l3","i1","i2","i3")
fit <- lavaan(alt, data=df) ## fitting the model
summary(fit, fit.measures=T, standardized=T, ci=T,rsq=T) ## let's have a look


########################## MISCHIEF MANAGED #########################

############################# AU REVOIR #############################


