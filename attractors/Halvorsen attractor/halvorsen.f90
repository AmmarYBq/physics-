program halvorsen 
implicit none
double precision :: x,y,z,der_x,der_y,der_z
double precision :: a,t,tmax,dt
integer :: counter, lines
x=1.001d0 !small diffrance in initial conditions results in a totally different paths "chaotic"
y=1d0    
z=1d0
t=0d0
tmax=30d0
dt=0.0001d0
a=1.4d0
lines=tmax/dt

open (unit=1 , file="results.txt") 
do while (t<tmax)
counter=counter+1

der_x=-a*x-4d0*y-4d0*z-y**2       !"Euler-Cromer numerical method" 
x=x+der_x*dt

der_y=-a*y-4d0*z-4d0*x-z**2
y=y+der_y*dt

der_z=-a*z-4d0*x-4d0*y-x**2
z=z+der_z*dt

t=t+dt

if (mod(counter,lines/6000)==1) write (1,*) x,y,z    

end do

close(1)

end program 