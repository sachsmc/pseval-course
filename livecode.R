library(pseval)
library(survival)
library(splines)

set.seed(721)

test <- generate_example_data(2000)
test2 <- generate_example_data(500)


simple <- test[, c(1, 2, 5, 8)]

tte <- test[, c(1, 2, 3, 5, 6, 7)]
tte$S.obs <- exp(tte$S.obs)

cat <- test2[, c(1, 8, 9, 10)]
cat$S.obs.cat <- cut(test2$S.obs, c(-Inf,-1, 0, 1, Inf))


p1 <- psdesign(simple, Z = Z, Y = Y.obs, S = S.obs, BIP = BIP)
p2 <- psdesign(tte, Z = Z, Y = Surv(time.obs, event.obs),
               S = S.obs, BIP = BIP, CPV = CPV, tau = 0.001)

p3 <- psdesign(cat, Z = Z, Y = Y.obs, S = S.obs.cat, BIP = BIP.cat)


## integration models
hist(p1$augdata$S.1)
plot(S.1 ~ BIP, data = p1$augdata)

hist(p2$augdata$S.1)
plot(S.1 ~ BIP, data = p2$augdata)

p1 <- p1 + integrate_semiparametric(formula.location = S.1 ~ BIP,
                                    formula.scale = S.1 ~ 1)
p2 <- p2 + integrate_parametric(formula = S.1 ~ BIP + I(BIP^2))

p3 <- p3 + integrate_nonparametric(S.1 ~ BIP)


## risk models and estimation

plot(survfit(Y ~ 1, data = p2$augdata))

p1 <- p1 + risk_binary(Y ~ S.1 * Z, D = 200) + ps_estimate()
p1 <- p1 + risk_binary(Y ~ S.1 * Z, D = 200, risk = risk.probit) + ps_estimate()
p1 <- p1 + risk_binary(Y ~ bs(S.1, df = 2, degree = 1) * Z, D = 200, risk = risk.probit) + ps_estimate()


p2 <- p2 + risk_weibull(D = 200) + ps_estimate() + ps_bootstrap()
p2 <- p2 + risk_exponential(D = 200) + ps_estimate()

p3 <- p3 + risk_binary(D = 1000) + ps_estimate(method = "pseudo-score") + ps_bootstrap()
plot(p3)

## post-estimation


p1 <- p1 + ps_bootstrap(n.boots = 50)

