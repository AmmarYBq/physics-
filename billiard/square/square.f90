program square
implicit none 
double precision :: y,x,pi
double precision :: y1,x1,angle 
double precision :: v0,vx,vy,t,tmax,dt
integer :: counter,lines

pi=4.0d0*atan(1.0d0)
y1=0d0
t=0d0
tmax=3000d0
dt=0.01d0
v0=1d0
angle=20d0*pi/180d0
counter=0d0




open (unit=1 , file="results.txt")
open (unit=2 , file ="position.txt")
x1=5d0
y1=5d0 
 do while (x1>=0) 
write (1,*) x1,y1,-x1,-y1
x1=x1-0.01d0
y1=y1-0.01d0
end do





x=0d0
y=0d0
lines=tmax/dt
counter=0d0
vx=v0*cos(angle)
vy=v0*sin(angle)
do while(t<tmax)
counter=counter+1

x=x+vx*dt
y=y+vy*dt

if (x>5d0 .or. x<-5d0 ) vx=-vx
 
 if (y>5d0 .or. y<-5d0) vy=-vy


if (mod(counter,lines/6000)==1 .or. x>0.999*5d0 .or. x<-0.999*5d0 .or. y>0.999*5d0 .or. y<-0.999*5d0) then 
 write (2,*) x,y
 end if 
t=t+dt

end do
close (1)
close (2)



end program