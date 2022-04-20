program colli
implicit none
double precision :: x1,x2,v1,v2,m1,m2,t,tmax,dt
double precision :: v1_old, v2_old,freq_old,freq
double precision :: dt_old, x1_old, tol
integer :: counter, counter2, lines 
x1=3d0
x2=5d0
t=0d0
tmax=30d0
dt=1d-5
v1=0d0
v2=-1d0
m1=1d0
m2=100d4
freq=0d0
freq_old=0d0
tol=1d-6   !tolarance or "target error" 
counter=0
counter2=0


 x1_old=x1
 dt_old=dt


lines=(tmax/dt)
open (unit=1, file="results.txt")
do while (t<=tmax)        !abs(v1)>v2 .or. v1<=0d0)  for automation
lines=(tmax/dt)



counter2=counter2+1

x1=x1+v1*dt
x2=x2+v2*dt



if (x2-x1<0.01d0)then             
	v1_old=v1
	v2_old=v2

	v1=(v1_old)*((m1-m2)/(m1+m2))+((2*m2*v2_old)/(m1+m2))
	v2=(v2_old)*((m2-m1)/(m1+m2))+((2*m1*v1_old)/(m1+m2))
	!print *,1
	counter=counter+1
end if 


if (x1<=tol*1d-1) then  
	v1=-v1 !abs(v1)
	counter=counter+1
end if
freq_old=counter/t


if (freq_old>freq) freq=freq_old



if(mod(counter2,(lines/6000))==1) then

write (1,*) t, x1,x2,counter, freq  

end if




t=t+dt




if (.not. x1_old==x1)then                               
	 dt=(0.9d0)*(tol/abs(x1-x1_old))*(dt_old)           

end if  


dt_old=dt
x1_old=x1 

end do



print *, counter 

close(1)
end program 




