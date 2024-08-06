code for parameter fitting/AIC calculating of TVGR, TVCC, & TVGRCC models
the code fits the differential equation 
dx/dt = r0*x*(1-(x/K0))
to obtained data on zoospore density in fungal pathogens, where r0 and K0 are constants, and x is zoospore density
the code also fits special cases, namely when r0 and K0 are functions of time
the quality of fits is compared by calculating AIC for each modek
