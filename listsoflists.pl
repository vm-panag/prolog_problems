%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Vassilis Panagakis         %
% listoflists.pl             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- set_flag(print_depth, 1000).

/*** cartesian product ***/

cart_prod([X], CP) :-
   cart_prod2([[]], X, CP).
cart_prod([X | L], CP) :-
   cart_prod(L, M),
   cart_prod2(M, X, CP).

cart_prod2([], _, []).
cart_prod2([X | Rest], L, CP) :-
	distribute(X, L, Dis),
	cart_prod2(Rest, L, M),
	append(Dis, M, CP).

/* predicate that constructs the cartesian product in a list of lists format */

distribute(_, [], []).
distribute(Y, [X | L], [CP | M]) :-
   append([X], Y, CP),
   distribute(Y, L, M).


/*** transpose matrix ***/

matr_transp([R | M1], M2) :- transp(R, [R | M1], M2).

transp([], _, []).
transp([_ | M1], Rest1, [M2 | Ms]) :-
	first_column(Rest1, M2, NewRest),
	transp(M1, NewRest, Ms).

/* predicate that returns the first column of a matrix */

first_column([], [], []).
first_column([[X | Row] | L], [X | Xs], [Row | Rest]) :- first_column(L, Xs, Rest).


/*** matrix multiplication ***/

matr_mult([], _, []).
matr_mult([R1 | M1], M2, [M3 | Rest]) :- 
	maintain_row(R1, M2, M3),
	matr_mult(M1, M2, Rest). 
	
/*predicate to calculate the product of a row of the first matrix with all the columns of the second matrix */	
	
maintain_row(_, [[] | _], []).	
maintain_row(Row, NewM2, [NewRow | NewRest]) :-	
	first_column(NewM2, Col, CutM2),
	row_element(Row, Col, NewRow),
	maintain_row(Row, CutM2, NewRest).

/* predicate that calculates the value of an element of the final matrix */
	
row_element([], [], 0).
row_element([X | R], [Y | C], Res) :-
	row_element(R, C, Tres),
	Res is X * Y + Tres. 	
	
	
/*** matrix determinant ***/

matr_det(M, D) :- det(M, D).

det([[X]], D) :- length([[X]], N), N =:= 1, D is X.
det([[X1, X2], [X3, X4]], D) :- length([[X1, X2], [X3, X4]], N), N =:= 2, two_dim([[X1, X2], [X3, X4]], D).
det([[X | Row] | L], D) :- length([X | Row], N), N =\= 2, rec_det([X | Row], L, 0, D).

/* predicate that calculates the determinant of a n-dimensional matrix */ 	

rec_det([], _, _, 0).
rec_det([X | Row], L, I, D) :-	
	I1 is I + 1,
	isPos(I1, S),
	SX is X * S,
	allrow_deleteN(L, I1, NewL),
	matr_det(NewL, SubD),
	rec_det(Row, L, I1, TempD), 
	D is SX * SubD + TempD.
	
/* predicate that determines the sign of each factor in the determinant formula */ 
	
isPos(Counter, Sign) :- 0 =\= Counter mod 2, Sign is 1. 
isPos(Counter, Sign) :- 0 =:= Counter mod 2, Sign is -1. 
		
/* predicate that calculates the determinant of a 2-dimensional matrix */ 				

two_dim([[X1, X2], [X3, X4]], D1) :-
	Da is X1 * X4,
	Db is X2 * X3,
	D1 is Da - Db.

/* predicate that deletes elements from a list of lists based on a common index */

allrow_deleteN([], _, []).
allrow_deleteN([R1 | Rest], Ind, [NewR1 | NewRest]) :-
	row_deleteN(R1, Ind, NewR1),
	allrow_deleteN(Rest, Ind, NewRest).

/* predicate that deletes an element from a list based on an index */

row_deleteN(In, Ind, Out) :-
   append(Pre, [_ | Suf], In),
   length([_ | Pre], Ind),
   append(Pre, Suf, Out).