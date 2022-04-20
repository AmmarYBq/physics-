program halvorsen 
implicit none
double precision :: x,y,z,der_x,der_y,der_z
double precision :: a,b,c,d,e,t,tmax,dt
integer :: counter, lines
x=1d0 
y=1d0    
z=1d0
t=0d0
tmax=30d0
dt=0.0001d0
a=3d0
b=2.7d0
c=1.7d0
d=2d0
e=9d0

lines=tmax/dt

open (unit=1 , file="results.txt") 
do while (t<tmax)
counter=counter+1

der_x=y-a*x+b*y*z       !"Euler-Cromer numerical method" 
x=x+der_x*dt

der_y=c*y-x*z+z
y=y+der_y*dt

der_z=d*x*y-e*z
z=z+der_z*dt

t=t+dt

if (mod(counter,lines/6000)==1) write (1,*) x,y,z    

end do

close(1)

end program 