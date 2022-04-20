program lorenz
implicit none 
double precision :: x,y,z, sigma, b, r, dt, tmax,t
double precision ::der_x,der_y,der_z, counter
sigma=10d0
b=8d0/3d0
x=1d0
y=0d0
z=0d0
r=25d0
t=0d0
dt=1d-4
tmax=500d0


open (unit=1, file="results.txt")
do while (t<=tmax)
der_x=sigma*(y-x)
x=x+der_x*dt

der_y=-x*z+r*x-y
y=y+der_y*dt

der_z=x*y-b*z
z=z+der_z*dt

t=t+dt

if (mod(counter,100d0)==1) then 
write (1,*) x,y,z

end if 
counter=counter+1
end do

close (1)

end program