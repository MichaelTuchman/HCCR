library('data.table')
library('numDeriv')
library('parallel')

cores=detectCores(_
cl <- makeCluster(3, type = 'PSOCK')

markets <- unique(dt[, market_time])
R = 10000
nu_p <- rnorm(n = R, -2, 0.5)
nu_xr <- rnorm(n = R, 2, 0.5)
nu_xm <- rnorm(n = R, 2, 0.5)
nu_xj <- rnorm(n = R, 2, 0.5)

clusterExport(cl,c('dt','nu_p','nu_xr','nu_xm','nu_xj')
              
              temp <- parLapply(cl, markets,calc_mc_w, dt=dt,nu_p=nu_p,nu_xr= nu_xr,
                                nu_xm=nu_xm,nu_xj=nu_xj)