

;; to make the coordinates be X,Y,Z do
;; set dummy X,Y,Z   
;; a=surface{[x^2-y^2]}
;;
;; a=surface{[x^2-y^2,x^3+y^3,x]}
;; a=surface{[x^2-y^2,x^3+y^3,x][x=-5,5][y=x:5]}
;; a=tube{[t,t+2,t^3][t=0,10][radius=.3*t]}
;; a=curve{[t,t+2,t^3][t=0,10][style=-1]}
;; if [grid,12,80]
;; a=surface{["foo.zplot"][sample=13,81]}
;; plot a
;; julia sets:
;; 


