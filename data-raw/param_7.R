require(data.table)
require(MCMCglmm)

data <- fread("data-raw/Allometry-database-2012m_data.csv")
data_loc = fread("data-raw/Allometry-database-2012n_envsub.csv")
data[data_loc, on = "Locality", ':='(temp = i.Temp.Seasonality..SD.100.*1e-3, 
                                     cwd = CWD*1e-3,
                                     prec = Precip.Seasonality..CV.*1e-3) ]


# work on the data
data[, AGBlog := log(Dry.total.AGB..kg.)]
setnames(data, c("Trunk.diameter..cm.", "Total.height..m.", "Wood.specific.gravity"), c("D", "H", "WD"))
data[, Comp := H * WD * D^2 ]
data <- data[!is.na(Comp), ]

# remove the trees where the heigth is less than 1.3 m
data <- data[H >= 1.3, ]






# param_6 -----------------------------------------------------------------

form = as.formula("I(log(H)) ~ temp + cwd + prec + I(log(D)) + I(log(D)^2)")
mod = MCMCglmm(form, data = data, pr = T, nitt = 3000 + nrow(data) * 10)


# extract the data from the MCMC
param_6 <- data.frame(mod$Sol, sqrt(as.matrix(mod$VCV)))
colnames(param_6) = c("intercept", "temp", "cwd", "prec", "logdbh", "logdbh2", "sd")


# Env : matrix of the 1001 values of E (columns) for each individual (row)
Env = -( tcrossprod(param_6$temp, param_6$temp) + 
  tcrossprod(param_6$cwd, param_6$cwd) + 
  tcrossprod(param_6$prec, param_6$prec) )

colnames(Env) = paste0("Env", 1:ncol(Env))


# param_7 -----------------------------------------------------------------
data_tmp = data.table(AGBlog = data$AGBlog,
                      logwsg = log(data$WD),
                      D = data$D,
                      Env[, 1:1000])

data2 = melt(data_tmp, measure.vars = grep("Env", colnames(data_tmp)))


form = as.formula("AGBlog ~ value + logwsg + I(log(D)) + I(log(D)^2)")
mod2 = system.time( MCMCglmm(form, random = ~ variable, data = as.data.frame(data2), pr = T, nitt = 1) )

param7_bis = data.frame(mod2$Sol, sqrt(as.matrix(mod2$VCV)))
colnames(param7_bis) = c("intercept", "temp", "cwd", "prec", "logwsg", "logdbh", "logdbh2", "sd")

summary(param7_bis)
summary(param_7)








form = as.formula("AGBlog ~ (I(log(WD)) + I(log(D)) + I(log(D)^2)) ")
form_ra = as.formula("~ temp + cwd + prec")
mod2 = MCMCglmm(form, random = form_ra, data = data, pr = T, nitt = 13001)
