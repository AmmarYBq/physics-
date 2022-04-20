program circle
implicit none 
double precision :: y,x,pi
double precision :: y1,x1,r,th,thc,vr,vth
double precision :: v0,vx,vy,t,tmax,dt
integer :: counter,lines

pi=4.0d0*atan(1.0d0)
y1=0d0
t=0d0
tmax=15000d0
dt=0.01d0
v0=1d0
! angle=20d0*pi/180d0
counter=0d0


th=0*pi/180d0

open (unit=1 , file="results.txt")
open (unit=2 , file ="position.txt")

r=2d0
 do while (th<=360d0*pi/180d0) 
y1=r*sin(th)
x1=r*cos(th)
write (1,*)  x1,y1

th=th+0.1d0*pi/180d0

end do



x=0.8d0    !when it start at x=0 y=0 it just go back and forth 
y=0.0d0
th=20d0*pi/180d0
lines=tmax/dt
counter=0d0
vx=v0*cos(th)
vy=v0*sin(th)
do while(t<tmax)
counter=counter+1

x=x+vx*dt
y=y+vy*dt

if (x**2+y**2>=r**2)  call reflection(vx,vy,y,x,r)
 

if ((mod(counter,lines/6000)==1) .or. (x**2+y**2>0.95*r**2) ) then
 write (2,*) x,y
 end if
t=t+dt

end do
close (1)
close (2)



end program

subroutine reflection(vx,vy,y,x,r)
implicit none

double precision :: vx, vy,thc ,r
double precision :: vNor, vPar,y,x

thc=atan((y-0d0)/(x-0d0))  ! thc=atan ((y-yc)/(x-xc))
                           ! where xc,yc represent the center of the circle 
						   ! since our circle is in the middle xc=0 yc=0
vNor=vx*cos(thc)+vy*sin(thc)
vPar=-vx*sin(thc)+vy*cos(thc)

vx=-vNor*cos(thc)-vPar*sin(thc) 
vy=-vNor*sin(thc)+vPar*cos(thc) 


end subroutine reflection