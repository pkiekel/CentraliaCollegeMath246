# lm(formula, data, subset, weights, na.action,
#   method = "qr", model = TRUE, x = FALSE, y = FALSE, qr = TRUE,
#   singular.ok = TRUE, contrasts = NULL, offset, ...)


#####Create subset data set#####
Psych_Clear <- subset(Psych, subset=IE != "?" & SN != "?" & FT != "?" & PJ != "?")

AV4way <- lm(Stress ~ factor(IE_clear)*factor(SN_clear)*factor(FT_clear)*factor(PJ_clear), data=Psych_Clear)
anova(AV4way)


#####Plot Means#####
windows(width=7, height=7); par(lwd=1, las=1, family="sans", cex=1, mgp=c(3.0,1,0))
StatMedplotMeans(Psych_Clear$Stress, as.factor(Psych_Clear$PJ_clear), as.factor(Psych_Clear$SN_clear), error.bars="se", xlab="PJ_clear", lty=1, lwd=1, ylab="Stress", legend.lab="SN_clear")

library(tcltk, pos=4)
#####Numerical summaries#####
res <- numSummary(Psych_Clear[,"Stress"], groups=Psych_Clear$IE_clear, statistics=c("mean", "sd", "quantiles"), quantiles=c(0,.25,.5,.75,1))
colnames(res$table)[1:2] <- gettext(domain="R-RcmdrPlugin.EZR", colnames(res$table)[1:2])
windows(width=7, height=7); par(lwd=1, las=1, family="sans", cex=1, mgp=c(3.0,1,0))
dot.plot(Psych_Clear$IE_clear, Psych_Clear$Stress, xlab="IE_clear", ylab="Stress")
res
remove(res)

