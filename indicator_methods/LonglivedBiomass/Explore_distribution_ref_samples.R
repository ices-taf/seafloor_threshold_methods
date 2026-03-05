
par(mfrow=c(2,2))

biom_ref <- c(0.03957205621167, 0.0811918545211271, 0.908829228583368, 0.922249894291561, 0.0462640015420659, 0.034283891473264, 
              0.00962059832789064, 0.492276399029422, 9.59232693276135e-14, 0.606834621564842, 0.0015883537470871, 0.0226450990829886, 
              0.0338317385921383, 0.149291081670944, 0.0257352831295881, 0.281418315885206, 0.0257930781814254)

###
# ASSUME NORMAL DISTRIBUTION
###

plot(biom_ref, main="Biomass (Normal distribution)\nData are skewed", ylim=c(-0.5,2))

n_records <- length(biom_ref)
mean_biom <- mean(biom_ref)
sd_biom   <- sd(biom_ref)
se_biom   <- sd_biom / sqrt(n_records)
tval     <- qt(0.975, df = max(n_records-1, 1))
err_margin <- tval * se_biom
DC <- CIlow <- mean_biom - err_margin
DC
abline(h=mean_biom, col="grey75")
abline(h=DC, col="darkred", lwd=2)
nv_lower <- quantile(biom_ref, 0.05)
nv_upper <- quantile(biom_ref, 0.95)
nv_lower
nv_upper
rect(xleft=0.5, xright=length(biom_ref)+0.5, 
     ybottom=nv_lower, ytop=nv_upper, 
     col=rgb(t(col2rgb("skyblue2")), maxColorValue = 255, alpha = 30), 
     border=NA)
NV = nv_lower
NV
abline(h=NV, col="skyblue2", lwd=2)
mtext(paste0("Mean = ", round(mean_biom, 3)),col="grey75",
      side = 3, line = -1.5, adj = 0.5, cex=0.75)
mtext(paste0("Detectable change = ", round(DC, 3)),col="darkred",
      side = 3, line = -2.5, adj = 0.5, cex=0.75)
mtext(paste0("Natural variation = ", round(NV, 3)), col="skyblue2",
      side = 3, line = -3.5, adj = 0.5, cex=0.75)
#points(biom_ref, pch=19, cex=1.1)

###
# ASSUME BETA DISTRIBUTION
###
library(fitdistrplus)
biom_ref_adj <- pmin(pmax(biom_ref, 1e-6), 1-1e-6)
fit_beta <- fitdist(biom_ref_adj, "beta", method="mme")
# fit_beta
plot(biom_ref, main="Biomass (Beta distribution)\nData are skewed", ylim=c(-0.5,2))

n <- length(biom_ref_adj)
alpha <- fit_beta$estimate["shape1"]
beta  <- fit_beta$estimate["shape2"]
mean_beta <- alpha / (alpha + beta)
var_beta <- (alpha * beta) / ((alpha + beta)^2 * (alpha + beta + 1))
se_beta <- sqrt(var_beta / n)
tval <- qt(0.975, df=n-1)
DC <- mean_beta - tval * se_beta
DC
abline(h=mean_biom, col="grey75")
abline(h=DC, col="darkred", lwd=2)

NV_lower <- qbeta(0.05, fit_beta$estimate["shape1"], fit_beta$estimate["shape2"])
NV_upper <- qbeta(0.95, fit_beta$estimate["shape1"], fit_beta$estimate["shape2"])
NV <- NV_lower  # same as before if you want to plot the lower bound
rect(xleft=0.5, xright=length(biom_ref)+0.5, 
     ybottom=NV_lower, ytop=NV_upper, 
     col=rgb(t(col2rgb("skyblue2")), maxColorValue = 255, alpha = 30), 
     border=NA)
abline(h=NV_lower, col="skyblue2", lwd=2)
mtext(paste0("Mean = ", round(mean_beta, 3)),col="grey75",
      side = 3, line = -1.5, adj = 0.5, cex=0.75)
mtext(paste0("Detectable change = ", round(DC, 3)),col="darkred",
      side = 3, line = -2.5, adj = 0.5, cex=0.75)
mtext(paste0("Natural variation = ", round(NV, 3)), col="skyblue2",
      side = 3, line = -3.5, adj = 0.5, cex=0.75)
#points(biom_ref, pch=19, cex=1.1)


###
# ASSUME LOG-NORMAL DISTRIBUTION
###
# rel_biom_ref = Biomass estimates of all reference samples relative to their mean
rel_biom_ref <- biom_ref / mean(biom_ref) 
mean(rel_biom_ref) # The mean of biomass of all reference samples is on average equal to 1.

plot(log(rel_biom_ref+1e-6), main="Log (relative biomass)\nData are centered around log(1) = 0", ylim=c(-14,5)) 
resp_rel_log <- log(rel_biom_ref+1e-6)
n_records <- length(resp_rel_log)
mean_biom <- mean(resp_rel_log, na.rm=T)
sd_biom   <- sd(resp_rel_log, na.rm=T)
se_biom   <- sd_biom / sqrt(n_records)
tval     <- qt(0.975, df = max(n_records-1, 1))
err_margin <- tval * se_biom
DC <- CIlow <- mean_biom - err_margin
DC
abline(h=mean_biom, col="grey75")
abline(h=DC, col="darkred", lwd=2)
nv_lower <- quantile(resp_rel_log, 0.05)
nv_upper <- quantile(resp_rel_log, 0.95)
nv_lower
nv_upper
rect(xleft=0.5, xright=length(resp_rel_log)+0.5, 
     ybottom=nv_lower, ytop=nv_upper, 
     col=rgb(t(col2rgb("skyblue2")), maxColorValue = 255, alpha = 30), 
     border=NA)
NV = nv_lower
NV
abline(h=NV, col="skyblue2", lwd=2)
mtext(paste0("Mean = ", round(mean_biom, 3)),col="grey75",
      side = 3, line = -1.5, adj = 0.5, cex=0.75)
mtext(paste0("Detectable change = ", round(DC, 3)),col="darkred",
      side = 3, line = -2.5, adj = 0.5, cex=0.75)
mtext(paste0("Natural variation = ", round(NV, 3)), col="skyblue2",
      side = 3, line = -3.5, adj = 0.5, cex=0.75)
#points(log(rel_biom_ref+1e-6), pch=19, cex=1.1)


###
# EXP(LOG-NORMAL) DISTRIBUTION
###
plot(exp(resp_rel_log), 
     main="exp(Log relative biomass)\nData are centered around exp(0) = 1") 
abline(h=exp(mean_biom), col="grey75")
abline(h=exp(DC), col="darkred", lwd=2)
NV <- nv_lower <- exp(quantile(resp_rel_log, 0.05))
nv_upper <- exp(quantile(resp_rel_log, 0.95))
nv_lower
nv_upper
abline(h=NV, col="skyblue2", lwd=2)
rect(xleft=0.5, xright=length(resp_rel_log)+0.5, 
     ybottom=nv_lower, ytop=nv_upper, 
     col=rgb(t(col2rgb("skyblue2")), maxColorValue = 255, alpha = 30), 
     border=NA)
mtext(paste0("Mean = ", round(exp(mean_biom), 3)),col="grey75",
      side = 3, line = -1.5, adj = 0.5, cex=0.75)
mtext(paste0("Detectable change = ", round(exp(DC), 3)),col="darkred",
      side = 3, line = -2.5, adj = 0.5, cex=0.75)
mtext(paste0("Natural variation = ", round(NV, 3)), col="skyblue2",
      side = 3, line = -3.5, adj = 0.5, cex=0.75)
#points(exp(resp_rel_log), pch=19, cex=1.1)

# 
# tmp <- rel_biom_ref
# plot(tmp, main="Relative biomass")
# n_records <- length(tmp)
# mean_log <- mean(log(tmp + 1e-6), na.rm=T)
# mean_log
# sd_log   <- sd(log(tmp + 1e-6), na.rm=T)
# sd_log
# se_log   <- sd_log / sqrt(n_records)
# se_log
# tval     <- qt(0.975, df = max(n_records-1, 1))
# tval
# Q90long_DCref = exp(mean_log - tval * se_log)
# Q90long_DCref
# Q90long_NVref = exp(quantile(log(tmp + 1e-6), 0.15,na.rm=T))
# rect(xleft=0.5, xright=length(resp_rel_log)+0.5, 
#      ybottom=exp(quantile(log(tmp + 1e-6), 0.15,na.rm=T)), 
#      ytop=exp(quantile(log(tmp + 1e-6), 0.85,na.rm=T)), 
#      col=rgb(t(col2rgb("skyblue2")), maxColorValue = 255, alpha = 30), 
#      border=NA)
# abline(h=exp(mean_log), col="grey75")
# abline(h=Q90long_DCref, col="darkred", lwd=2)
# abline(h=Q90long_NVref, col="skyblue2", lwd=2)
# 
