# functions for SrF calculations
laser_beam_detun <- function(Xstate_split,rel_detun){
  Xstate_detun=c(Xstate_split[1],Xstate_split[1],Xstate_split[1],Xstate_split[2],Xstate_split[3],Xstate_split[3],Xstate_split[3],Xstate_split[4],Xstate_split[4],Xstate_split[4],Xstate_split[4],Xstate_split[4])#
  delta_lup=cbind(rep(0,12),rep(0,12),rep(0,12),rep(0,12),rep(0,12),rep(0,12),rep(0,12),rep(0,12))
  delta_lup[,1]=rel_detun+Xstate_split[1]-Xstate_detun#c(Xstate_split[1],Xstate_split[1],Xstate_split[1],Xstate_split[2],Xstate_split[3],Xstate_split[3],Xstate_split[3],Xstate_split[4],Xstate_split[4],Xstate_split[4],Xstate_split[4],Xstate_split[4])# -1.2*Gamma_n
  delta_lup[,2]=rel_detun+Xstate_split[2]-Xstate_detun#(Xstate_split[1],Xstate_split[1],Xstate_split[1],Xstate_split[2],Xstate_split[3],Xstate_split[3],Xstate_split[3],Xstate_split[4],Xstate_split[4],Xstate_split[4],Xstate_split[4],Xstate_split[4])-1.2*Gamma_n
  delta_lup[,3]=rel_detun+Xstate_split[3]-Xstate_detun
  delta_lup[,4]=rel_detun+Xstate_split[4]-Xstate_detun
  delta_lup[,5]=rel_detun+Xstate_split[1]-Xstate_detun
  delta_lup[,6]=rel_detun+Xstate_split[2]-Xstate_detun
  delta_lup[,7]=rel_detun+Xstate_split[3]-Xstate_detun
  delta_lup[,8]=rel_detun+Xstate_split[4]-Xstate_detun
  return(delta_lup)
}

laser_beam_detun_var <- function(Xstate_split,rel_detun){
  Xstate_detun=c(Xstate_split[1],Xstate_split[1],Xstate_split[1],Xstate_split[2],Xstate_split[3],Xstate_split[3],Xstate_split[3],Xstate_split[4],Xstate_split[4],Xstate_split[4],Xstate_split[4],Xstate_split[4])#
  delta_lup=cbind(rep(0,12),rep(0,12),rep(0,12),rep(0,12),rep(0,12),rep(0,12),rep(0,12),rep(0,12))
  delta_lup[,1]=rel_detun[1]+Xstate_split[1]-Xstate_detun#c(Xstate_split[1],Xstate_split[1],Xstate_split[1],Xstate_split[2],Xstate_split[3],Xstate_split[3],Xstate_split[3],Xstate_split[4],Xstate_split[4],Xstate_split[4],Xstate_split[4],Xstate_split[4])# -1.2*Gamma_n
  delta_lup[,2]=rel_detun[2]+Xstate_split[2]-Xstate_detun#(Xstate_split[1],Xstate_split[1],Xstate_split[1],Xstate_split[2],Xstate_split[3],Xstate_split[3],Xstate_split[3],Xstate_split[4],Xstate_split[4],Xstate_split[4],Xstate_split[4],Xstate_split[4])-1.2*Gamma_n
  delta_lup[,3]=rel_detun[3]+Xstate_split[3]-Xstate_detun
  delta_lup[,4]=rel_detun[4]+Xstate_split[4]-Xstate_detun
  delta_lup[,5]=rel_detun[1]+Xstate_split[1]-Xstate_detun
  delta_lup[,6]=rel_detun[2]+Xstate_split[2]-Xstate_detun
  delta_lup[,7]=rel_detun[3]+Xstate_split[3]-Xstate_detun
  delta_lup[,8]=rel_detun[4]+Xstate_split[4]-Xstate_detun
  return(delta_lup)
}

# 6 beams only for BaH
laser_beam_detun6 <- function(Xstate_split,rel_detun){
  Xstate_detun=c(Xstate_split[1],Xstate_split[1],Xstate_split[1],Xstate_split[2],Xstate_split[3],Xstate_split[3],Xstate_split[3],Xstate_split[4],Xstate_split[4],Xstate_split[4],Xstate_split[4],Xstate_split[4])#
  delta_lup=cbind(rep(0,12),rep(0,12),rep(0,12),rep(0,12),rep(0,12),rep(0,12))
  delta_lup[,1]=rel_detun+Xstate_split[1]-Xstate_detun#c(Xstate_split[1],Xstate_split[1],Xstate_split[1],Xstate_split[2],Xstate_split[3],Xstate_split[3],Xstate_split[3],Xstate_split[4],Xstate_split[4],Xstate_split[4],Xstate_split[4],Xstate_split[4])# -1.2*Gamma_n
  delta_lup[,2]=rel_detun+Xstate_split[3]-Xstate_detun#(Xstate_split[1],Xstate_split[1],Xstate_split[1],Xstate_split[2],Xstate_split[3],Xstate_split[3],Xstate_split[3],Xstate_split[4],Xstate_split[4],Xstate_split[4],Xstate_split[4],Xstate_split[4])-1.2*Gamma_n
  delta_lup[,3]=rel_detun+Xstate_split[4]-Xstate_detun
  delta_lup[,4]=rel_detun+Xstate_split[1]-Xstate_detun
  delta_lup[,5]=rel_detun+Xstate_split[3]-Xstate_detun
  delta_lup[,6]=rel_detun+Xstate_split[4]-Xstate_detun
  #delta_lup[,7]=rel_detun+Xstate_split[3]-Xstate_detun
  #delta_lup[,8]=rel_detun+Xstate_split[4]-Xstate_detun
  return(delta_lup)
}

laser_beam_detun10 <- function(Xstate_split,rel_detun){
  Xstate_detun=c(Xstate_split[1],Xstate_split[1],Xstate_split[1],Xstate_split[2],Xstate_split[3],Xstate_split[3],Xstate_split[3],Xstate_split[4],Xstate_split[4],Xstate_split[4],Xstate_split[4],Xstate_split[4])#
  delta_lup=cbind(rep(0,12),rep(0,12),rep(0,12),rep(0,12),rep(0,12),rep(0,12),rep(0,12),rep(0,12),rep(0,12),rep(0,12))
  # beams 1 and 6 are special here
  delta_lup[,1]=rel_detun[1]+Xstate_split[1]-Xstate_detun#c(Xstate_split[1],Xstate_split[1],Xstate_split[1],Xstate_split[2],Xstate_split[3],Xstate_split[3],Xstate_split[3],Xstate_split[4],Xstate_split[4],Xstate_split[4],Xstate_split[4],Xstate_split[4])# -1.2*Gamma_n
  delta_lup[,2]=rel_detun[2]+Xstate_split[2]-Xstate_detun#(Xstate_split[1],Xstate_split[1],Xstate_split[1],Xstate_split[2],Xstate_split[3],Xstate_split[3],Xstate_split[3],Xstate_split[4],Xstate_split[4],Xstate_split[4],Xstate_split[4],Xstate_split[4])-1.2*Gamma_n
  delta_lup[,3]=rel_detun[3]+Xstate_split[3]-Xstate_detun
  delta_lup[,4]=rel_detun[4]+Xstate_split[4]-Xstate_detun
  delta_lup[,5]=rel_detun[5]+Xstate_split[4]-Xstate_detun
  delta_lup[,6]=rel_detun[6]+Xstate_split[1]-Xstate_detun
  delta_lup[,7]=rel_detun[7]+Xstate_split[2]-Xstate_detun
  delta_lup[,8]=rel_detun[8]+Xstate_split[3]-Xstate_detun
  delta_lup[,9]=rel_detun[9]+Xstate_split[4]-Xstate_detun
  delta_lup[,10]=rel_detun[10]+Xstate_split[4]-Xstate_detun
  return(delta_lup)
}

laser_beam_detun12 <- function(Xstate_split,rel_detun){ # assume EOM driven sidebands
  # hyperfine and SR splittings for the X state
  Xstate_detun=c(Xstate_split[1],Xstate_split[1],Xstate_split[1],Xstate_split[2],Xstate_split[3],Xstate_split[3],Xstate_split[3],Xstate_split[4],Xstate_split[4],Xstate_split[4],Xstate_split[4],Xstate_split[4])#
  delta_lup=cbind(rep(0,12),rep(0,12),rep(0,12),rep(0,12),rep(0,12),rep(0,12),rep(0,12),rep(0,12),rep(0,12),rep(0,12),rep(0,12),rep(0,12)) # total number of beams
  # columns are for each laser beam and rows are for each hyperfine state
  delta_lup[,1]=rel_detun[1]-Xstate_detun#c(Xstate_split[1],Xstate_split[1],Xstate_split[1],Xstate_split[2],Xstate_split[3],Xstate_split[3],Xstate_split[3],Xstate_split[4],Xstate_split[4],Xstate_split[4],Xstate_split[4],Xstate_split[4])# -1.2*Gamma_n
  delta_lup[,2]=rel_detun[2]-Xstate_detun#(Xstate_split[1],Xstate_split[1],Xstate_split[1],Xstate_split[2],Xstate_split[3],Xstate_split[3],Xstate_split[3],Xstate_split[4],Xstate_split[4],Xstate_split[4],Xstate_split[4],Xstate_split[4])-1.2*Gamma_n
  delta_lup[,3]=rel_detun[3]-Xstate_detun
  delta_lup[,4]=rel_detun[4]-Xstate_detun
  delta_lup[,5]=rel_detun[5]-Xstate_detun
  delta_lup[,6]=rel_detun[6]-Xstate_detun
  # repeat now
  delta_lup[,7]=rel_detun[1]-Xstate_detun
  delta_lup[,8]=rel_detun[2]-Xstate_detun
  delta_lup[,9]=rel_detun[3]-Xstate_detun
  delta_lup[,10]=rel_detun[4]-Xstate_detun
  delta_lup[,11]=rel_detun[5]-Xstate_detun
  delta_lup[,12]=rel_detun[6]-Xstate_detun
  # delta_lup/(2*pi*1e6)
  return(delta_lup)
}

scattering_rate_matrix <- function(pos,vel,Afield,k_vec,pol_vec,gl,gu,delta_lup,sat_p){
  Rlup_init=cbind(rep(0,dim(Xstates)[1]),rep(0,dim(Xstates)[1]),rep(0,dim(Xstates)[1]),rep(0,dim(Xstates)[1]))
  Rlup_all=array(Rlup_init,dim=c(dim(Rlup_init)[1],dim(Rlup_init)[2],2)) # create array for storing scattering rates from all beams
  Bfield=Afield*c(pos[1],pos[2],-2*pos[3]) # magnetic field
  Bfield_mag=sqrt(Bfield[1]^2+Bfield[2]^2+Bfield[3]^2) # calculate B field magnitude
  for (l in 1:dim(Xstates)[1]){
    for (u in 1:dim(Astates)[1]){
      delta_omega=(gu[u]*Astates[u,3]-gl[l]*Xstates[l,3])*Bfield_mag*Bohr_magneton/hbar
      for (p in 1:dim(pol_vec)[1]){# loop over all laser beams
        # if ((pos[1]^2+pos[2]^2)<(rcut)^2){
        #   Ip=2*P_laser[p]/(pi*w^2)*exp(-2*(pos[1]^2+pos[2]^2)/w^2)
        # #   #sat_p=1
        # }else{
        #   Ip=0
        # #   #sat_p=0
        # }
        # sat_p=Ip/Isat2level
        #sat_p=1 # assume all transitions saturated
        #sat_param[i]=sat_p
        if(Bfield_mag!=0) {
          angle=acos((Bfield[1]*k_vec[p,1]+Bfield[2]*k_vec[p,2]+Bfield[3]*k_vec[p,3])/Bfield_mag) # angle between magnetic field and k-vector of laser beam
        }else{
          angle=0
        }
        # print(angle)
        pol_vec_local[1]=0.5*((1+cos(angle))*pol_vec[p,1]-sqrt(2)*sin(angle)*pol_vec[p,2]+(1-cos(angle))*pol_vec[p,3])
        pol_vec_local[2]=0.5*(sqrt(2)*sin(angle)*pol_vec[p,1]+2*cos(angle)*pol_vec[p,2]-sqrt(2)*sin(angle)*pol_vec[p,3])
        pol_vec_local[3]=0.5*((1-cos(angle))*pol_vec[p,1]+sqrt(2)*sin(angle)*pol_vec[p,2]+(1+cos(angle))*pol_vec[p,3])
        #print(pol_vec[p,])
        #print(pol_vec_local)
        # pol_vec_local=pol_vec[p,] # don't change the polarization
        if (abs(pol_vec_local[1])>1e-10 & (Xstates[l,3]+1==Astates[u,3])){ # this will ensure that they are coupled
          f_lup=r_lu[l,u]*pol_vec_local[1]^2
          #print(c('sigma+',p,l,u))
        }else if(abs(pol_vec_local[2])>1e-10 & (Xstates[l,3]==Astates[u,3])){ # this will ensure that they are coupled
          f_lup=r_lu[l,u]*pol_vec_local[2]^2
          #print(c('pi',p,l,u))
        }else if(abs(pol_vec_local[3])>1e-10 & (Xstates[l,3]-1==Astates[u,3])){ # this will ensure that they are coupled
          f_lup=r_lu[l,u]*pol_vec_local[3]^2
          #print(c('sigma-',p,l,u))
        }else{
          f_lup=0
        }
        R_lup=0.5*Gamma_n*f_lup*sat_p/(1+sat_p+4*(delta_lup[l,p]-2*pi/lambda*(k_vec[p,1]*vel[1]+k_vec[p,2]*vel[2]+k_vec[p,3]*vel[3])-delta_omega)^2/Gamma_n^2)
        # R_lup=0.5*Gamma_n*f_lup*sat_p/(1+4*(delta_lup[l,p]-2*pi/lambda*(k_vec[p,1]*vel[1]+k_vec[p,2]*vel[2]+k_vec[p,3]*vel[3])-delta_omega)^2/Gamma_n^2)
        if (k_vec[p,3]==1) Rlup_all[l,u,1]=Rlup_all[l,u,1]+R_lup
        if (k_vec[p,3]==(-1)) Rlup_all[l,u,2]=Rlup_all[l,u,2]+R_lup
      }# sum over p ends here
    }# sum over u ends here
  }# sum over l ends here
  return(Rlup_all) # return the scattering array at this positions
} # function ends here

# no saturation parameter in the denominator this is the correct one to use for optica molasses
scattering_rate_matrix_2OM <- function(pos,vel,Afield,k_vec,pol_vec,gl,gu,delta_lup,Rabi_ij){
# scattering_rate_matrix_2OM <- function(pos,vel,Afield,k_vec,pol_vec,gl,gu,delta_lup,sat_p){
  Rlup_init=cbind(rep(0,dim(Xstates)[1]),rep(0,dim(Xstates)[1]),rep(0,dim(Xstates)[1]),rep(0,dim(Xstates)[1]))
  Rlup_all=array(Rlup_init,dim=c(dim(Rlup_init)[1],dim(Rlup_init)[2],2)) # create array for storing scattering rates from all beams
  Bfield=Afield*c(1,0,0)# uniform B field is in the x-direction *c(pos[1],pos[2],-2*pos[3]) # magnetic field
  Bfield_mag=sqrt(Bfield[1]^2+Bfield[2]^2+Bfield[3]^2) # calculate B field magnitude
  for (l in 1:dim(Xstates)[1]){
    for (u in 1:dim(Astates)[1]){
      delta_omega=(gu[u]*Astates[u,3]-gl[l]*Xstates[l,3])*Bfield_mag*Bohr_magneton/hbar
      for (p in 1:dim(pol_vec)[1]){# loop over all laser beams
        # if ((pos[1]^2+pos[2]^2)<(rcut)^2){
        #   Ip=2*P_laser[p]/(pi*w^2)*exp(-2*(pos[1]^2+pos[2]^2)/w^2)
        # #   #sat_p=1
        # }else{
        #   Ip=0
        # #   #sat_p=0
        # }
        # sat_p=Ip/Isat2level
        #sat_p=1 # assume all transitions saturated
        #sat_param[i]=sat_p
        if(Bfield_mag!=0) {
          angle=acos((Bfield[1]*k_vec[p,1]+Bfield[2]*k_vec[p,2]+Bfield[3]*k_vec[p,3])/Bfield_mag) # angle between magnetic field and k-vector of laser beam
        }else{
          angle=0
        }
        # print(angle)
        pol_vec_local[1]=0.5*((1+cos(angle))*pol_vec[p,1]-sqrt(2)*sin(angle)*pol_vec[p,2]+(1-cos(angle))*pol_vec[p,3])
        pol_vec_local[2]=0.5*(sqrt(2)*sin(angle)*pol_vec[p,1]+2*cos(angle)*pol_vec[p,2]-sqrt(2)*sin(angle)*pol_vec[p,3])
        pol_vec_local[3]=0.5*((1-cos(angle))*pol_vec[p,1]+sqrt(2)*sin(angle)*pol_vec[p,2]+(1+cos(angle))*pol_vec[p,3])
        #print(pol_vec[p,])
        #print(pol_vec_local)
        # pol_vec_local=pol_vec[p,] # don't change the polarization
        if (abs(pol_vec_local[1])>1e-10 & (Xstates[l,3]+1==Astates[u,3])){ # this will ensure that they are coupled
          f_lup=pol_vec_local[1]^2
          #print(c('sigma+',p,l,u))
        }else if(abs(pol_vec_local[2])>1e-10 & (Xstates[l,3]==Astates[u,3])){ # this will ensure that they are coupled
          f_lup=pol_vec_local[2]^2
          #print(c('pi',p,l,u))
        }else if(abs(pol_vec_local[3])>1e-10 & (Xstates[l,3]-1==Astates[u,3])){ # this will ensure that they are coupled
          f_lup=pol_vec_local[3]^2
          #print(c('sigma-',p,l,u))
        }else{
          f_lup=0
        }
        # here we use the correct expression in terms of Rabi frequency instead of saturation intensity
        R_lup=0.5*Gamma_n*f_lup*2*Rabi_ij[l,u]^2/Gamma_n^2/(1+4*(delta_lup[l,p]-2*pi/lambda*(k_vec[p,1]*vel[1]+k_vec[p,2]*vel[2]+k_vec[p,3]*vel[3])-delta_omega)^2/Gamma_n^2)
        # R_lup=0.5*Gamma_n*f_lup*kij[l,u]^2*sat_p/(1+4*(delta_lup[l,p]-2*pi/lambda*(k_vec[p,1]*vel[1]+k_vec[p,2]*vel[2]+k_vec[p,3]*vel[3])-delta_omega)^2/Gamma_n^2)
        if (k_vec[p,3]==1) Rlup_all[l,u,1]=Rlup_all[l,u,1]+R_lup
        if (k_vec[p,3]==(-1)) Rlup_all[l,u,2]=Rlup_all[l,u,2]+R_lup
      }# sum over p ends here
    }# sum over u ends here
  }# sum over l ends here
  return(Rlup_all) # return the scattering array at this positions
} # function ends here