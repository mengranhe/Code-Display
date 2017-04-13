dec.data <- read.csv("dec-data.csv",header=T)

str(dec.data)

library(R2jags)
library(rube)

attach(dec.data)

lrt <- log(reacTime)

sID <- factor(SID)

cq <- factor(currentQuestion)

###################

joint.model <-
"
data{

    for (n in 1:Nresp) {

        lrt[n] <- log(reacTime[n])

        # this is a data transformation, which
        # is why it's in its own section in the
        # jags model
    }

}

model {

    # LEVEL 1

    for (n in 1:Nresp) {

        resp[n] ~ dbern(p[n])
        logit(p[n]) <- lp[n]

        lrt[n] ~ dnorm(rho*lp[n],invsig2)

        lp[n] <- b[question[n]] + alpha[player[n]]
    }

    # FIXED EFFECTS PRIORS

    for (j in 1:J) {
        bb[j] ~ dnorm(0,1e-10)          # the question parameters get
    }                                   # flat priors...

    for (j in 1:J) {
        b[j] <- bb[j] - mean(bb[])      # ...except that we make them
    }                                   # sum to zero...

    rho ~ dnorm(0,1e-10)

    # LEVEL 2

    for (i in 1:N) {
        alpha[i] ~ dnorm(b0,invtau2)    # ...so that we can have an
    }                                   # overall intercept at level 2

    # LEVEL 2 PRIORS

    invtau2 <- pow(tau,-2)
    invsig2 <- pow(sig,-2)

    tau ~ dunif(0,10)
    sig ~ dunif(0,10)

    b0 ~ dnorm(0,1e-10)

}"

#####################################

joint.data <- list(resp=resp,reacTime=reacTime,question=as.numeric(cq),
                   player=as.numeric(sID),Nresp=length(resp),
                   N=length(unique(sID)),J=length(unique(cq)))

joint.inits <- function() {
    return(list(tau=runif(1),sig=runif(1),bb=rnorm(20),rho=rnorm(1),b0=rnorm(1)))
}

rube(joint.model,data=joint.data,inits=joint.inits)

#####################################

joint.mcmc.test <- rube(joint.model,data=joint.data,inits=joint.inits,
                   n.chains=1,n.iter=1000,
                   parameters.to.save=c("b","sig","tau","alpha","rho","b0"))

#####################################

joint.mcmc.0 <- rube(joint.model,data=joint.data,inits=joint.inits,
#                   parallel=T,
                    parameters.to.save=c("b","sig","tau","alpha","rho","b0"))

#####################################

b.mean <- joint.mcmc.0$median$b
b.sd <- joint.mcmc.0$sd$b

tau.mean <- joint.mcmc$median$tau
tau.sd <- joint.mcmc$sd$tau

sig.mean <- joint.mcmc$median$sig
sig.sd <- joint.mcmc$sd$sig

rho.mean <- joint.mcmc$median$rho
rho.sd <- joint.mcmc$sd$rho

b0.mean <- joint.mcmc$median$b0
b0.sd <- joint.mcmc$sd$b0

joint.inits <- function() {
    return(list(tau=runif(1,tau.mean-2*tau.sd,tau.mean+2*tau.sd),
                sig=runif(1,sig.mean-2*sig.sd,sig.mean+2*sig.sd),
                bb=rnorm(20,b.mean,b.sd),
                rho=rnorm(1,rho.mean,rho.sd),
                b0=rnorm(1,b0.mean,b0.sd)))
}

joint.mcmc.1 <- rube(joint.model,data=joint.data,inits=joint.inits,
#                   parallel=T,
                   parameters.to.save=c("b","sig","tau","alpha","rho","b0"))

#############################################

detach()

q()

#############################################

### To look at the mcmc fitted model objects I generated for you:
###
### (1) copy fitted-mcmc.RData to your working directory on your
###     laptop
###
### (2) use the command load("fitted-mcmc.RData") to bring the
###     following objects into your working directory:
###
###     cq, dec.data, joint.mcmc.0, joint.mcmc.1, joint.mcmc.test,
###     lrt, sID

library(R2jags)
library(rube)

load("fitted-mcmc.RData")

#############################################
