# prolog_problems
Simple prolog predicates and CSP solving

## Usage
```
? - cart_prod([[a,b,c],[1,2],[x,y,z,w]], CP).

CP = [[a,1,x],[a,1,y],[a,1,z],[a,1,w],[a,2,x],[a,2,y],
      [a,2,z],[a,2,w],[b,1,x],[b,1,y],[b,1,z],[b,1,w],
      [b,2,x],[b,2,y],[b,2,z],[b,2,w],[c,1,x],[c,1,y],
      [c,1,z],[c,1,w],[c,2,x],[c,2,y],[c,2,z],[c,2,w]]

? - matr_transp([[5,8,9,7,2],[3,6,1,1,4],[2,4,2,8,0]],M).

M = [[5,3,2],[8,6,4],[9,1,2],[7,1,8],[2,4,0]]

? - matr_mult([[1,2,3],[4,5,6],[6,5,4],[3,2,1]],[[3,4],[5,6],[7,8]],M).

M = [[34,40],[79,94],[71,86],[26,32]]

? - matr_det([[4,3,2,1],[3,2,1,2],[1,4,1,3],[2,1,3,2]],D).

D = 51
```
```
? - jobshop(S).

S = [execs(m1,[t(t11,0,2),t(t41,2,7),t(t22,7,10)]),
     execs(m2,[t(t12,2,8),t(t42,8,10),t(t23,10,13)]),
     execs(m2,[t(t21,0,5),t(t31,5,9)])] -> ;

? - findall(S,jobshop(S),L), length(L,N).

N = 9136
```

### CSP
```
? - seed(2020), vertexcover(9, 30, C).

Found a solution with cost 6
Found a solution with cost 5
Found no solution with cost 0.0 .. 4.0

C = [1, 5, 6, 8, 9]
```
```
? - stable(M).

M = [rick-helen, jack-lilly, john-maria, hugh-diana, greg-jenny,
     nick-sally, bill-wanda, andy-tracy, alan-patty, dick-linda]
More --> ;
M = [rick-maria, jack-lilly, john-diana, hugh-helen, greg-jenny,
     nick-sally, bill-linda, andy-tracy, alan-patty, dick-wanda]
More --> ;
M = [rick-maria, jack-lilly, john-linda, hugh-diana, greg-jenny,
     nick-sally, bill-helen, andy-tracy, alan-patty, dick-wanda]
More --> ;
M = [rick-tracy, jack-lilly, john-diana, hugh-helen, greg-jenny,
     nick-sally, bill-linda, andy-maria, alan-patty, dick-wanda]
```
```
? - jobshop_opt([j1,j2,j3,j4,j5], 7, Schedule, Cost, 1.0, 0).

Found a solution with cost 19
Found a solution with cost 18
Found a solution with cost 17
Found a solution with cost 16
Found a solution with cost 14
Found a solution with cost 13
Found no solution with cost 12.0 .. 12.0

Schedule = [execs(m1,[t(t11,0,3),t(t41,3,4),t(t13,6,8),t(t42,8,11),t(t43,11,13)]),
            execs(m1,[t(t21,0,2),t(t31,2,5),t(t23,6,9),t(t24,9,13)]),
            execs(m2,[t(t51,0,3),t(t12,3,5),t(t32,5,6),t(t33,6,10),t(t14,10,13)]),
            execs(m2,[t(t22,3,6)])]

Cost = 13
```

## Author
Vassilis Panagakis
