amu=1.66e-27 # [kg]
m_BaH=(138+1)*amu # [kg] 
mass_BaH=m_BaH
mass_He=4*amu
lifetime=136e-9 # [s]
lambda=1060*1e-9 # [m] SrOH X-A2Pi1/2 transition wavelength
gamma_n=1/lifetime
vr=h_Planck/(mass_BaH*lambda) # recoil velocity
#gamma_n=2*pi*gamma_n_MHz*1e6 # [Hz] 1/lifetime
k=2*pi/lambda # [1/m] wavevector
Is=pi*h_Planck*c*gamma_n/(3*lambda^3)*0.1 # [mW/cm^2] saturation intensity for a corresponding 2-level system
# print(Is) # for a corresponding two-level system
ng=12 # number of ground state mf sublevel
ne=4 # number of excited state mf sublevels
seff=2*ng^2/(ng+ne) # scaling for the effective saturation parameter in a multi-level system
Is_eff=seff*Is # assume rotationally closed transition
scale_factor=1 # match the actual scattering rate measured in the experiment
gamma_n_eff=scale_factor*2*gamma_n*ne/(ng+ne) # effective linewidth
