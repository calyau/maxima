/*-*-MACSYMA-*-*/
if properties(cray_fortran) = [] then load(cfortr)$
eqns3:[t\(k\)=a\(k\)*(b1\(i\,j\)+exb1\(i\,j\))-exp(-hnu/ti\(i\,j\)),
       u\(k\)=b\(k\)*(b2\(i\,j\)+exb2\(i\,j\))*(b3\(i\,j\)+exb3\(i\,j\)),
       v\(k\)=(b1\(i\,j\)+exb1\(i\,j\))*(b3\(i\,j\)+exb3\(i\,j\))^2,
       w\(k\)=(b2\(i\,j\)+exb2\(i\,j\))/sqrt((b1\(i\,j\)+exb1\(i\,j\))^2+
               (b2\(i\,j\)+exb2\(i\,j\))^2+(b3\(i\,j\)+exb3\(i\,j\))^2),
       x\(k\)=sqrt((b1\(i\,j\)+exb1\(i\,j\))^2+
               (b2\(i\,j\)+exb2\(i\,j\))^2+(b3\(i\,j\)+exb3\(i\,j\))^2)
              *sqrt(b1\(i\,j\)+exb1\(i\,j\))-exp(-hnu/ti\(i\,j\))];
open_fortran_file("sys$scratch:scrat.out")$
comment_fortran("The following list corresponds to the FORTRAN which follows")$
comment_fortran(eqns3)$
map('cray_fortran,eqns3)$
close_fortran_file()$
/* printfile("gcook\;scrat out")$ 
delfile("gcook\;scrat out")$ */
?type\-file(?"sys$scratch:scrat.out")$


