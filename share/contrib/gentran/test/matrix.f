c
c --- Calculate Matrix Values ---
c
      mat(1,1)=-9*m30*p**2*sin(q3)**2+j30z*sin(q3)**2-j30y*sin(q3)**2+18
     . *m30*p**2*cos(q2)*cos(q3)+18*m30*p**2+m10*p**2+j30y+j10y
      mat(1,2)=-9*m30*p**2*sin(q3)**2+j30z*sin(q3)**2-j30y*sin(q3)**2+9*
     . m30*p**2*cos(q2)*cos(q3)+9*m30*p**2+j30y
      mat(1,3)=-9*m30*p**2*sin(q2)*sin(q3)
      mat(2,2)=-9*m30*p**2*sin(q3)**2+j30z*sin(q3)**2-j30y*sin(q3)**2+9*
     . m30*p**2+j30y
      mat(2,3)=0
      mat(3,3)=9*m30*p**2+j30x
