library(glmmTMB)
library(AICcmodavg)
library(DHARMa)

#1a. Testing model fits for # EGGS LAID response variable
    laid.pois <- glmmTMB(laid ~ meanT + (1|weather_station), data=data_eg, family=poisson)
    laid.nb <- glmmTMB(laid ~ meanT + (1|weather_station), data=data_eg, family=nbinom2)
    laid.zip <- glmmTMB(laid ~ meanT + (1|weather_station), data=data_eg, family=poisson, ziformula=~1)
    laid.zinb <- glmmTMB(laid ~ meanT + (1|weather_station), data=data_eg, family=nbinom2, ziformula=~1)
    models <- list(laid.pois, laid.nb, laid.zip, laid.zinb)
    mod.names <- c("pois", "nb", "zip", "zinb")
    aictab(cand.set=models, modnames=mod.names)
      #1b. Testing for presence of ZI on laid.nb model:
      laid.nb_simres <- simulateResiduals(laid.nb, quantreg=T)
      testZeroInflation(laid.nb_simres)

#2a. Testing model fits for # EGGS HATCHED response variable
    hatched.pois <- glmmTMB(hatched ~ maxT + maxofminT + (1|weather_station), data=data_eg, family=poisson)
    hatched.nb <- glmmTMB(hatched ~ maxT + maxofminT + (1|weather_station), data=data_eg, family=nbinom2)
    hatched.zip <- glmmTMB(hatched ~ maxT + maxofminT + (1|weather_station), data=data_eg, family=poisson, ziformula=~1)
    hatched.zinb <- glmmTMB(hatched ~ maxT + maxofminT + (1|weather_station), data=data_eg, family=nbinom2, ziformula=~1)
    models <- list(hatched.pois, hatched.nb, hatched.zip, hatched.zinb)
    mod.names <- c("pois", "nb", "zip", "zinb")
    aictab(cand.set=models, modnames=mod.names)
      #2b. Testing for presence of ZI on hatched.nb model:
      hatched.nb_simres <- simulateResiduals(hatched.nb, quantreg=T)
      testZeroInflation(hatched.nb_simres)

#3a. Testing model fits for # EGGS FLEDGED response variable
    fledged.pois <- glmmTMB(fledged ~ maxT + maxofminT + (1|weather_station), data=data_eg, family=poisson)
    fledged.nb <- glmmTMB(fledged ~ maxT + maxofminT + (1|weather_station), data=data_eg, family=nbinom2)
    fledged.zip <- glmmTMB(fledged ~ maxT + maxofminT + (1|weather_station), data=data_eg, family=poisson, ziformula=~1)
    fledged.zinb <- glmmTMB(fledged ~ maxT + maxofminT + (1|weather_station), data=data_eg, family=nbinom2, ziformula=~1)
    models <- list(fledged.pois, fledged.nb, fledged.zip, fledged.zinb)
    mod.names <- c("pois", "nb", "zip", "zinb")
    aictab(cand.set=models, modnames=mod.names)
      #3b. Testing for presence of ZI on fledged.nb model:
      fledged.nb_simres <- simulateResiduals(fledged.nb, quantreg=T)
      testZeroInflation(fledged.nb_simres)



