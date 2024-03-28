%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Vassilis Panagakis         %
% stable.pl                  %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- lib(fd).

/* uncomment following line in case you haven't compiled stablefd_data.pl file */

/* :- compile(stablefd_data). */

/*** predicates that returns a list of N stable marriages ***/

stable(Marriages) :-
	men(Men),	
	women(Women),	
	def_vars(Men, Women, Couples1),			%define variables and domains of men
	def_vars(Women, Men, Couples2),			%define variables and domains of women
	preferance_constrs(Couples1, Couples2),		%define preferance constraints for every man and woman
	def_marriage(Couples1, Couples2),		%define the concept of marriage for the given lists of men and women
	generate(Couples1),				%generate solution for men
	marry(Couples1, Marriages). 			%return the solution list in the wanted format

/* predicate where variables and their domains are defined */

def_vars([], _, []).
def_vars([H1 | Humans1], Humans2, [marries(H1, X) | Couples]) :-
	X #:: Humans2,
	def_vars(Humans1, Humans2, Couples).

/* predicate where preferance constraints are defined */

preferance_constrs([], _).
preferance_constrs([marries(H1, X) | Couples1], Couples2) :-
	prefers(H1, Prefs2),	
	matching(marries(H1, X), Prefs2, Couples2),
	preferance_constrs(Couples1, Couples2).

/* predicate that imposes the effects of preferance constraints on the opposite sex */

matching(_, [], _).
matching(marries(H1, X), [H2 | Humans2], Couples2) :-
	stability_constrs(Couples2, H2, marries(H1, X)),
	matching(marries(H1, X), Humans2, Couples2).

/* predicate that defines the concept of stable marriage */

stability_constrs([], _, _).
stability_constrs([marries(H, _) | Couples2], H2, marries(H1, X)) :-
	H \= H2,
	stability_constrs(Couples2, H2, marries(H1, X)).
stability_constrs([marries(H2, Y) | Couples2], H2, marries(H1, X)) :-
	prefers(H1, Prefs2),				%get preferance list of human H1
	element(I1, Prefs2, H2),			%get index of human H2 in H1's preferance list
	element(Ix, Prefs2, X),				%get index of human X (the one H1 will marry) in H1's preferance list
	prefers(H2, Prefs1),				%get preferance list of human H2
	element(I2, Prefs1, H1),			%get index of human H1 in H2's preferance list
	element(Iy, Prefs1, Y),				%get index of human Y (the one H2 will marry) in H2's preferance list
	(I1 #< Ix) #=> (Iy #< I2), 			%if H1 prefers H2 more than the human to marry then H2 must prefer the human to marry more than H1
	stability_constrs(Couples2, H2, marries(H1, X)).

/* predicate that defines the concept of marriage */

def_marriage([], _).
def_marriage([marries(H1, X) | Couples1], Couples2) :-
	relationship_constrs(Couples2, marries(H1, X)),
	def_marriage(Couples1, Couples2).
	
/* predicate where relationship constraints are defined */
	
relationship_constrs([], _).
relationship_constrs([marries(H2, Y) | Couples2], marries(H1, X)) :-
	(X #= H2) #<=> (Y #= H1),
	relationship_constrs(Couples2, marries(H1, X)).

/* predicate that generates the married couples in input order */

generate([]).
generate([marries(_, X) | Couples1]) :-
   indomain(X),
   generate(Couples1).

/* predicate that returns the list of married couples */

marry([], []).
marry([marries(H1, X) | Couples], [H1 - X | Marriages]) :-
	marry(Couples, Marriages).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/* predicate that returns the number of marriages, the number of solutions and the execution time for one execution of the program */

go :-
	writeln('--------------------------------'),
	write('  Marriages: '),
	men(Men),
	length(Men, N),
	writeln(N),
	writeln('--------------------------------'),
	cputime(T1),
	findall(M, stable(M), Sols),
	cputime(T2),
	length(Sols, L),
	T is T2-T1,
	write('There are '),
	write(L),
	writeln(' solutions.'),
	write('Time: '),
	write(T),
	writeln(' secs.').
	
/*

************ RESULTS ***********

--------------------------------
  Marriages: 10
--------------------------------
There are 4 solutions.
Time: 0.0 secs.

--------------------------------
  Marriages: 50
--------------------------------
There are 10 solutions.
Time: 3.984375 secs.

--------------------------------
  Marriages: 100
--------------------------------
There are 35 solutions.
Time: 84.390625 secs.

--------------------------------
  Marriages: 110
--------------------------------
There are 69 solutions.
Time: 194.484375 secs.

--------------------------------
  Marriages: 120
--------------------------------
There are 66 solutions.
Time: 367.578125 secs.

--------------------------------
  Marriages: 130
--------------------------------
There are 65 solutions.
Time: 658.9375 secs. 

--------------------------------
  Marriages: 140
--------------------------------
There are 120 solutions.
Time: 867.0 secs.

*/