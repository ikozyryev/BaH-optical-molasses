# use rate equations to model optical molasses for BaH to figure out what's going on in the experiment
# following Tarbutt NJP (2015) try to reproduce Fig. 9 from the paper
# setwd("/Volumes/NO NAME/calculations/MOT_loading_simulations")
setwd("C:/Users/ikozy/Dropbox/work/calculations/MOT_loading_simulations/transverse_cooling_sims/full_model")
options(digits=10)
####### define experimental parameters ##########
source('BaH_MOT_constants.R')
source('BaH_MOT_functions.R')
library(plotly)
# library("deSolve")

# load all the states in |J,F,mf> format
pop_init=1/dim(Xstates)[1]
num_X_init=rep(pop_init,dim(Xstates)[1]) # initial population for the ground states; assume equally populated for simplicity

num_A_init=rep(0,dim(Astates)[1]) # initial population for the excited states

#rcut=2.5e-2/2 # [m] effective cut-off radius
w=4e-3 # [m] 1/e2 radius of the trapping beams used in the SrF experiment
t_step=0.01/Gamma_n

# six laser beams from each direction because of the EOM 20 MHz splittings
k_vec=rbind(c(0,0,1),c(0,0,1),c(0,0,1),c(0,0,1),c(0,0,1),c(0,0,1),c(0,0,-1),c(0,0,-1),c(0,0,-1),c(0,0,-1),c(0,0,-1),c(0,0,-1))
Ptot=100e-3 # [W] total laser power in the APi light


# k_vec=rbind(c(0,0,-1),c(0,0,-1),c(0,0,-1),c(0,0,-1),c(0,0,1),c(0,0,1),c(0,0,1),c(0,0,1))
# pol_vec=rbind(c(1,0,0),c(1,0,0),c(1,0,0),c(0,0,1),c(1,0,0),c(1,0,0),c(1,0,0),c(0,0,1)) # polarization vectors; use spherical basis set
#pol_vec=rbind(c(1,0,0),c(1,0,0),c(1,0,0),c(0,0,1),c(1,0,0),c(1,0,0),c(1,0,0),c(0,0,1)) # check that this is correct
# initialize polarizations
# pol_vec=rbind(c(0,0,1),c(1,0,0),c(1,0,0),c(1,0,0),c(0,0,1),c(1,0,0),c(1,0,0),c(1,0,0)) 
# pol_vec=rbind(c(1,0,0),c(1,0,0),c(1,0,0),c(0,0,1),c(0,0,1),c(0,0,1),c(0,0,1),c(1,0,0)) 
#pol_vec=rbind(c(1,0,0),c(1,0,0),c(1,0,0),c(0,0,1),c(1,0,0),c(1,0,0),c(1,0,0),c(0,0,1)) 
#pol_vec=rbind(c(0,1,0),c(0,1,0),c(0,1,0),c(0,1,0),c(0,1,0),c(0,1,0),c(0,1,0),c(0,1,0),c(0,1,0),c(0,1,0),c(0,1,0),c(0,1,0)) # assume pi polarized light
beam_pols=c(1/2,1/sqrt(2),1/2) # polarization vector at 45 degree angle relative to the B-field
pol_vec=beam_pols
for (cnt in 2:beam_nums){
  pol_vec=rbind(pol_vec,beam_pols)
}
beam_nums=dim(k_vec)[1] # number of beams/freq from one direction
P_laser=rep(Ptot/beam_nums*2,dim(k_vec)[1]) # [W] laser power per frequency component, note that beam are rertoreflected (i.e. x2 factor)
#pol_vec=rbind(c(0,0,1),c(0,0,1),c(0,0,1),c(1,0,0),c(0,0,1),c(0,0,1),c(0,0,1),c(1,0,0))

# pol_vec_local=rep(NA,3) # for storing local laser polarization
# norm_sp_decay=sp_decay_table/10#/sum(sp_decay_table) # normalized
#gamma_sp=0 # summation for number of t_steps_numed photons

# Xstate_split=-2*pi*c(0,55,129,170)*1e6 # relative detunings of SrF hyperfine ground state sublevels
# delta_lup=rep(-Gamma_n,dim(k_vec)[1]) # make sure the units are correct, assume 1 Gamma detuning like Tarbutt
# for a given laser polarization p there is a specific detuning to assign
# delta_lup_res=c(Xstate_split[1],Xstate_split[2],Xstate_split[3],Xstate_split[4],Xstate_split[1],Xstate_split[2],Xstate_split[3],Xstate_split[4])# on resonnance

# Xstate_detun=c(Xstate_split[1],Xstate_split[1],Xstate_split[1],Xstate_split[2],Xstate_split[3],Xstate_split[3],Xstate_split[3],Xstate_split[4],Xstate_split[4],Xstate_split[4],Xstate_split[4],Xstate_split[4])#
EOMfreq=20
rel_detunMHz=c(EOMfreq,0,-1*EOMfreq,-1*8642.8,-1*(8642.8+EOMfreq),-1*(8642.8+2*EOMfreq))# #rep(0,beam_nums) #-1*Gamma_n # relative detuning for each state
plot(seq(1,10),rep(rel_detunMHz[1],10),ylim=c(-50,50),type='l',lwd=2,col='black')
# plot(seq(1,10),rep(rel_detunMHz[1],10),ylim=-1*c(8642.8-50,8642.8+50),type='l',lwd=2,col='black')
for (i in 2:length(rel_detunMHz)){
  abline(h=rel_detunMHz[i],lwd=2)
}
# rel_detun=rel_detunMHz*2*pi*1e6
# delta_lup=laser_beam_detun12(Xstate_split,rel_detun,EOMfreq) # import all the relative detunings
# delta_lup=rep(0,6) # make sure the units are correct
# initialize polarization vecotors for each laser beam with multiple frequencies
# gu=c(-0.51,-0.51,-0.51,0)
#gu=rep(0,4)

#Afield=1e-2 # [T/m] 
Afield=1e-4 # [T] B field constant

t_steps_num=1000 # number of time steps

vel=c(0,0,2) # [m/s] velocity vector
#accel=c(0,0,0)
# velx_store=rep(NA,1000)
#zsteps=10
#accelz_store=rep(NA,zsteps+1)
#sat_param=rep(NA,zsteps+1)
#mag_field=rep(NA,zsteps+1)
# pos=c(0,0,1)*1e-2 # [m] position vector
# posx_store=rep(NA,1000)
#rcut=2e-2 # [m] cut off radius equal to 2 cm 
# zpos_step=2*rcut/2/zsteps
# zpos_step=rcut/2/zsteps
#zpos=seq(-rcut,rcut,by=4e-4) # positions to evaluate acceleration
# zpos=seq(0,rcut,by=zpos_step)
# zpos=seq(-rcut/2,0,by=zpos_step) # positions to evaluate acceleration
#zsteps=length(zpos)
#w=10e-3 # [m] 1/e2 radius of the trapping beams is 2.5 mm
#t_step=0.1/Gamma_n
# t_steps_num=1000
# sat_p=1 # saturation parameter
# line_strength_table=cbind(c(6,3,1,0,0),c(0,3,4,3,0),c(0,0,1,3,6)) #rbind(c(6,0,1,0,0),c(0,3,0,3,0),c(0,0,1,0,6))
# r_lu=rbind(c(1/2,1/3,0),c(1/2,1/3,1/2),c(0,1/3,1/2))
#r_lu=cbind(c(6,3,1,0,0),c(0,3,4,3,0),c(0,0,1,3,6))/10
# strength_norm=line_strength_table/3#sum(line_strength_table)

# sp_decay_table=cbind(c(6,3,1,0,0),c(0,3,4,3,0),c(0,0,1,3,6))
pol_vec_local=rep(NA,3) # for storing local laser polarization

#Isat2level=pi*h_Planck*c_light*Gamma_n/(3*lambda^3)
# P_laser=c(0.01,0.01) # [W] laser power per frequency component
# store the t_steps_numing rates
##### Generic loop for calculating the rate equations
parmax=50 # max value of the parameter
parvals=seq(-parmax,parmax,by=0.25) # parameter values for the loop iterations
parsteps=length(parvals) # number of steps to take
accelz_store=matrix(NA, nrow = t_steps_num, ncol = parsteps)
# accelz_store=rep(NA,ysteps)
gamma_sp=matrix(0, nrow = t_steps_num, ncol = parsteps) # record the number of scattered photons

for (parcnt in 1:parsteps){ # loop over the parameter counter now
  print(parcnt)
  # Rlup_neg=cbind(rep(0,dim(Xstates)[1]),rep(0,dim(Xstates)[1]),rep(0,dim(Xstates)[1]),rep(0,dim(Xstates)[1])) # for neg k-vec
  num_X=num_X_init # at each position start with equal population distribution
  num_X_zero=rep(0,dim(Xstates)[1]) # initial population for the ground states
  for (i in 1:(t_steps_num-1)) {
    num_X=rbind(num_X,num_X_zero)
  }

  num_A=num_A_init
  num_A_zero=rep(0,dim(Astates)[1])
  for (i in 1:(t_steps_num-1)) {
    num_A=rbind(num_A,num_A_zero)
  }
  
  pos=c(0,1e-3,0) # the spatial potition to use
  # calculate the laser power at the specific position
  Ip=2*P_laser[1]/(pi*w^2)*exp(-2*(pos[1]^2+pos[2]^2)/w^2)
  Rabi_ij=kij*trans_dipole*sqrt(2*Ip/(hbar^2*c_light*eps0)) # calculate the Rabi frequency at that position
  #flup_satp=4*kij^2*Ip*trans_dipole^2/(hbar^2*c_light*eps0*Gamma_n^2)
  #sat_p=Ip/Isat2level
  rel_detun=(rel_detunMHz+parvals[parcnt])*2*pi*1e6
  delta_lup=laser_beam_detun12(Xstate_split,rel_detun) # import all the relative detunings
  Rlup_all=scattering_rate_matrix_2OM(pos,vel,Afield,k_vec,pol_vec,gl,gu,delta_lup,Rabi_ij) # calculate the t_steps_numing array  
  # Rlup_all=scattering_rate_matrix_2OM(pos,vel,Afield,k_vec,pol_vec,gl,gu,delta_lup,sat_p)

  for (i in 1:t_steps_num){
    Nldot=rep(0,dim(Xstates)[1])
    Nudot=rep(0,dim(Astates)[1])
    # print(i)
    for (u in 1:dim(Astates)[1]) {
      Nldot=Nldot+(Rlup_all[,u,1]+Rlup_all[,u,2])*(num_A[i,u]-num_X[i,])+Gamma_n*r_lu[,u]*num_A[i,u]
       if (i<t_steps_num) gamma_sp[i+1,parcnt]=gamma_sp[i,parcnt]+num_A[i,u] # count photons
    }
    for (l in 1:dim(Xstates)[1]) {
      Nudot=Nudot+(Rlup_all[l,,1]+Rlup_all[l,,2])*(num_X[i,l]-num_A[i,])
    }
    Nudot=Nudot-Gamma_n*num_A[i,]
    if (i<t_steps_num){
     num_X[i+1,]=num_X[i,]+Nldot*t_step
     num_A[i+1,]=num_A[i,]+Nudot*t_step
    }
  }
  for (i in 1:t_steps_num){
   #print(i)
   # accel=c(0,0,0)
    accel=0
   # delta_accel
   for (l in 1:dim(Xstates)[1]) {
     for (u in 1:dim(Astates)[1]) {
       # accel=accel+(h_Planck/(mass*lambda))*k_vec[p,]*R_lup*(num_X[i,l]-num_A[i,u])
       accel=accel+(h_Planck/(mass*lambda))*(Rlup_all[l,u,1]-Rlup_all[l,u,2])*(num_X[i,l]-num_A[i,u])
       #     # print(c(delta_accel*1e-3,Xstates[l,2],Astates[u,2]))
      }
    }
   accelz_store[i,parcnt]=accel
  }
  
}

# for (cntr in 1:5){
#   if(cntr==1){
#     plot(zpos,accelz_store[cntr,]*1e-3,ylim=c(-4,4),type='l',lwd=2,col='blue')
#   }else{
#     lines(zpos,accelz_store[cntr*100,]*1e-3,lwd=2,col='red')
#   }
# }
# plot(gamma_sp[,110])

plot(parvals,accelz_store[500,]*1e-3,col='darkgreen',lwd=2,type='l',ylim=c(-5,5),xlim=c(-50,50),xlab='Detuning (MHz)',ylab='Acceleration (km/s^2)')#type='l',lwd=2,col='blue',xlab='Position (mm)',ylab='Acceleration (km/s^2)')
abline(v=0,lty=2,lwd=2)
# compare to the 2-level scaled model now
Rbeam=4e-1 # [cm]
Abeam=(pi*Rbeam^2)
intens_eff=Ptot*1e3/Abeam # [mW/cm^2]
sat_param=intens_eff/Is_eff
# sat_param=sat_p/seff # effective saturation parameter
source('BaH_params_2-level.R')
source('Constants_2019April5.R')
delta=parvals*2*pi*1e6
a_pos=hbar*k*gamma_n_eff*(sat_param)/(1+sat_param+4*(delta-k*vel[3])^2/gamma_n^2)/2/mass_BaH
a_neg=-hbar*k*gamma_n_eff*(sat_param)/(1+sat_param+4*(delta+k*vel[3])^2/gamma_n^2)/2/mass_BaH
lines(parvals,(a_pos+a_neg)*1e-3)

x=parvals
y=accelz_store[500,]*1e-3
data2plot=data.frame(x,y)
plot_ly(data = data2plot, x = ~x, y = ~y,type = 'scatter', mode = 'lines')

title(main='BaH 1D optical molasses no J=1/2 hyperfine (100 mW power)')
save_file='BaH_1D_OM_100mW_no_1by2_HF.pdf'
dev.copy(pdf,save_file,useDingbats=F)
dev.off()
# accelz_store_i=accelz_store[1000,]*1e-3
#accelz_store_ii=accelz_store[1000,]*1e-3
#accelz_store_iii=accelz_store[1000,]*1e-3

plot(accelz_store[1000,])

# layout(matrix(c(1,2,3,3), 2, 2, byrow = TRUE))
par(mfrow=c(2,2))
plot(time2plot,-1*accelz_store*1e-3,col='blue',type='l',lwd=2,xlab='Time (usec)',ylab='-1*Acceleration (km/s^2)',ylim=c(0,2),main='SrF DC MOT Force (++++)')
abline(h=0,lty=2,lwd=2)

# plot(time2plot,gamma_sp,type='l',lwd=2,col='blue',xlab='Time (usec)',ylab='t_steps_numed photons',main='z=4mm')

cols4plot=c('black','blue','red','green','magenta','darkgreen','orange','violet','darkgreen','orange','cyan','brown','maroon','grey','purple','pink')
time2plot=t_step*seq(0,t_steps_num-1,by=1)*1e6 # [microsec]
plot(time2plot,num_X[,1],col=cols4plot[1],xlim=c(0,15),ylim=c(0,0.4),type='l',lwd=2,xlab='Time (usec)',ylab='Population',main='Pupulation dynamics\n(++++) beams')
lines(time2plot,num_X[,2],col=cols4plot[2],lwd=2)
lines(time2plot,num_X[,3],col=cols4plot[3],lwd=2)
lines(time2plot,num_X[,4],col=cols4plot[4],lwd=2)
lines(time2plot,num_X[,5],col=cols4plot[5],lwd=2)
lines(time2plot,num_X[,6],col=cols4plot[6],lwd=2)
lines(time2plot,num_X[,7],col=cols4plot[7],lwd=2)
lines(time2plot,num_X[,8],col=cols4plot[8],lwd=2)
lines(time2plot,num_X[,9],col=cols4plot[9],lwd=2)
lines(time2plot,num_X[,10],col=cols4plot[10],lwd=2)
lines(time2plot,num_X[,11],col=cols4plot[11],lwd=2)
lines(time2plot,num_X[,12],col=cols4plot[12],lwd=2)

lines(time2plot,num_A[,1],col=cols4plot[13],lwd=2)
lines(time2plot,num_A[,2],col=cols4plot[14],lwd=2)
lines(time2plot,num_A[,3],col=cols4plot[15],lwd=2)
lines(time2plot,num_A[,4],col=cols4plot[16],lwd=2)
legend(10,0.4,c('X(1,-1)','X(1,0)','X(1,1)','X(0,0)','X(1,-1)','X(1,0)','X(1,1)','X(2,-2)','X(2,-1)','X(2,0)','X(2,1)','X(2,2)','A(1,-1)','A(1,0)','A(1,1)','A(0,0)'),col=cols4plot,box.col='white',lwd=rep(2,5),lty = rep(1,5),cex=0.75)
#legend(1,0.5,c('MX=-1','MX=0','MX=1','MA=-1','MA=0','MA=1'),col=cols4plot,box.col='white',lwd=rep(2,6),lty = rep(1,6),cex=1)

title(main='SrF DC MOT forces for different polarization configurations')
save_file='SrF_MOT_model_trapping_forces_ItoIII.pdf'
dev.copy(pdf,save_file,useDingbats=F)
dev.off()
