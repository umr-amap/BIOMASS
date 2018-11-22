require(data.table)
require(MCMCglmm)

# Load Chave et al. 2014 data
data <- fread("data-raw/Allometry-database-2012m_data.csv")

# work on the data
data[, AGBlog := log(Dry.total.AGB..kg.)]
setnames(data, c("Trunk.diameter..cm.", "Total.height..m.", "Wood.specific.gravity"), c("D", "H", "WD"))
data[, Comp := H * WD * D^2 ]
data <- data[!is.na(Comp), ]

# remove the trees where the heigth is less than 1.3 m
data <- data[H >= 1.3, ]



# Model and MCMC
form <- as.formula("AGBlog~I(log(Comp))")
mod <- MCMCglmm(form, data = data, pr = T, nitt = 13001)

# extract the data from the MCMC
mod$VCV <- as.matrix(mod$VCV)
param_4 <- data.frame(mod$Sol, sqrt(mod$VCV))


colnames(param_4) = c("intercept", "logagbt", "sd")
usethis::use_data(param_4)

