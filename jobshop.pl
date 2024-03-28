%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Vassilis Panagakis         %
% jobshop.pl                 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*** jobshop ***/

jobshop(Schedule) :-
	deadline(Deadline),
	get_machines(Deadline, MachAvs),
	findall(TL, job(_, TL), Tasks),
	timetable(Tasks, MachAvs),
	schedule(MachAvs, [], Schedule).

/*** jobshop with manpower ***/

jobshop_with_manpower(Schedule) :-
	deadline(Deadline),
	get_machines(Deadline, MachAvs),
	findall(TL, job(_, TL), Tasks),
	timetable_with_manpower(Tasks, MachAvs),
	schedule(MachAvs, [], Schedule).

/* predicate that organises every task of jobshop */

timetable([], _).
timetable([J | Tasks], MachAvs) :-
	expand_job(J, [], JL),
	job_matching(JL, MachAvs, []),
	timetable(Tasks, MachAvs).

/* predicate that organises every job of jobshop with man power */
	
timetable_with_manpower([], _).
timetable_with_manpower([J | Tasks], MachAvs) :-
	expand_job(J, [], JL),
	job_matching_with_manpower(JL, MachAvs, []),
	timetable_with_manpower(Tasks, MachAvs).	
	
/* predicate that organises every task of jobshop */	
	
job_matching([], _, _).
job_matching([mt(Tm, TL) | JL], MachAvs, Pref) :-
	deadline(Deadline),
	append(Pref, TL, TL1),
	length(TL1, N),
	N =< Deadline, 
	MT1 = mt(Tm, TL1),
	machine_matching(MT1, MachAvs, 0, Pref, Pref1),
	job_matching(JL, MachAvs, Pref1).

/* predicate that organises every task of jobshop with man power */
	
job_matching_with_manpower([], _, _).
job_matching_with_manpower([mt(Tm, TL) | JL], MachAvs, Pref) :-
	deadline(Deadline),
	append(Pref, TL, TL1),
	length(TL1, N),
	N =< Deadline, 
	MT1 = mt(Tm, TL1),
	machine_matching(MT1, MachAvs, 0, Pref, Pref1),
	findall(X, member(m(_, X), MachAvs), Avs),
	machine_staff(Avs),
	job_matching_with_manpower(JL, MachAvs, Pref1).

/* predicate that checks if there is enough staff at each time unit until the deadline */	

machine_staff([[] | _]).
machine_staff(L) :- 
	staff(Staff),
	task_staff(L, L1, Staff, 0),
	machine_staff(L1). 

/* predicate that checks if there is enough staff at a specific time unit */	

task_staff([], [], _, _).
task_staff([[T | TL] | Avs], [TL | Avs1], Staff, CurS) :-
	staff_per_time(T, SN), 
	CurS1 is SN + CurS,
	CurS1 =< Staff,
	task_staff(Avs, Avs1, Staff, CurS1).

/* predicate that returns the number of staff members that a task needs at each time unit */	
	
staff_per_time(T, StaffNum) :-
	var(T),
	StaffNum is 0.
staff_per_time(T, StaffNum) :-
	atom(T),
	task(T, _, _, StaffNum).

/* predicate that matches a task with a machine and returns the prefix of the next task */	

machine_matching(_, [], _, Pref4, Pref4).
machine_matching(MT, [_ | MachAvs], Updated, Pref, Pref3) :-
	Updated =:= 1,
	machine_matching(MT, MachAvs, Updated, Pref, Pref3).
machine_matching(MT, [MA | MachAvs], Updated, _, Pref3) :-
	Updated =:= 0,
	machine_member(MA, MachAvs, MachNum),
	task_matching(MT, MA, MachNum, RetUpd, Pref1),
	machine_matching(MT, MachAvs, RetUpd, Pref1, Pref3).

/* predicate that checks if a task matches with a machine type */	
		
task_matching(mt(Tm, _), m(M, _), _, RetUpd, []) :-
	Tm \= M,
	RetUpd is 0.
task_matching(mt(M, TL), m(M, Avs), MachNum, RetUpd, Pref) :-
	fits_machine(TL, Avs, MachNum, RetUpd, Pref).

/* predicate that returns every possible position of a task in a machine */	
		
fits_machine(TL, Avs, _, RetUpd, Pref) :-
	atom_head(TL, H),
	findall(X, indexOf(TL, H, X), Is),
	Is = [I | _],
	sublist(TL, Avs),
	task(H, _, D),
	expand_task(H, D, TL1),			%
	length(TL2, I),					%	re-construct the task list as it was before sublist/2 execution
	append(TL2, TL1, TL3),			%
	create_prefix(TL3, Avs, Pref),
	RetUpd is 1.
fits_machine(_, _, MachNum, RetUpd, []) :-
	MachNum > 0,
	RetUpd is 0.	

/* predicate that checks if there are more machines of the same type as the input machine */	
		
machine_member(_, [], 0).
machine_member(m(M1, _), [m(M2, _) | MachAvs], MachNum) :-
	M1 \= M2,
	machine_member(m(M1, _), MachAvs, MachNum).
machine_member(m(M, _), [m(M, _) | MachAvs], MachNum) :-
	machine_member(m(M, _), MachAvs, MachNum1),
	MachNum is MachNum1 + 1.

/* predicate that creates a list of anonymous variables, which is used as a prefix for the next task  */	
		
create_prefix(TL, Avs, Pref) :-
	atom_head(TL, H),
	findall(X, indexOf(Avs, H, X), Is),
	get_last(Is, I),
	I1 is I + 1, 
	length(Pref, I1). 

/* predicate that returns the final time schedule */	
		
schedule([], Schedule, Schedule).
schedule([m(M, Avs) | MachAvs], Sched, Sched3) :-
	machine_scheduling(m(M, Avs), TSched),
	bubblesort(TSched, TSched1),
	create_struct(M, TSched1, [], Sched1),
	scheduler(Sched, Sched1, Sched2),
	schedule(MachAvs, Sched2, Sched3).

/* predicate that creates a list of lists */	
		
scheduler(L1, [], L1).
scheduler(L1, L2, L3) :- L2 \= [], append(L1, [L2], L3).	

/* predicate that creates the homework's wanted list format */	
		
create_struct(_, [], _, []).
create_struct(M, TSched, Sched, Schedule) :- TSched \= [], append(Sched, execs(M, TSched), Schedule).

/* predicate that returns the time schedule of a machine */	
	
machine_scheduling(m(_, Avs), []) :-
	all_var(Avs).
machine_scheduling(m(_, Avs), TSched) :-
	setof(X, (member(X, Avs), atom(X)), Tasks),
	task_scheduling(Tasks, Avs, TSched).

/* predicate that returns the time schedule of a list of tasks */	

task_scheduling([], _, []).
task_scheduling([T | Tasks], Avs, [t(T, St, Fin) | TSched]) :-
	findall(X, indexOf(Avs, T, X), Is),
	Is = [St | _],
	task(T, _, D),
	Fin is St + D,
	task_scheduling(Tasks, Avs, TSched).	

/* predicate that returns a job's expanded task lists */	

expand_job([], JL, JL).
expand_job([T | J], JL1, JL3) :-
	task(T, M, D),
	expand_task(T, D, TL),
	append(JL1, [mt(M, TL)], JL2),
	expand_job(J, JL2, JL3).
	
/* predicate that returns an expanded task list based on task's duration */	
	
expand_task(_, 0, []).
expand_task(T, D, [T | Ts]) :-
	D > 0,
	D1 is D - 1,
	expand_task(T, D1, Ts).
		
/* predicate that returns all the available machines */		
		
get_machines(Deadline, MachAvs) :-
	findall(m(M, N), machine(M, N), L),
	expand(L, Deadline, [], MachAvs).

/* predicate that creates lists for all the machines */

expand([], _, MachAvs, MachAvs).
expand([m(M, N) | L], Deadline, MachAvs1, MachAvs4) :-
	expand_one(M, N, Deadline, MachAvs2),
	append(MachAvs1, MachAvs2, MachAvs3),
	expand(L, Deadline, MachAvs3, MachAvs4).

/* predicate that creates a list for a machine based on a deadline */
	
expand_one(_, 0, _, []).
expand_one(M, N, Deadline, [m(M, Avs) | MachAvs]) :-
	N > 0,
	N1 is N - 1,
	length(Avs, Deadline),
	expand_one(M, N1, Deadline, MachAvs).

/* predicate that returns an element from a list based on a specific index (indexing starts at 0) */

indexOf([Element1 | _], Element, 0) :- 
	atom(Element1), 
	atom(Element), 
	Element1 = Element. 
indexOf([_ | L], Element, Index):-
	indexOf(L, Element, Index1), 
	Index is Index1 + 1.

/* sublist predicate */

sublist(S, L) :-
	append(_, L2, L),
	append(S, _, L2).
	
/* predicate that returns the first atom in a list */	
	
atom_head([H1 | _], Head) :- atom(H1), H1 = Head, !.	
atom_head([_ | L], Head) :- atom_head(L, Head).

/* predicate that succeeds if every element in a list is a variable (here: anonymous variables) */

all_var([]).
all_var([H | L]) :- var(H), all_var(L).	

/* predicate that returns the last element of a list */

get_last([Last], Last).
get_last([_ | Ls], L) :-	
	get_last(Ls, L).

/* bubblesort predicate */
	
bubblesort(List, Sorted) :-
	swap(List, List1), !,
	bubblesort(List1, Sorted).
bubblesort(Sorted, Sorted).

swap([t(T1, St1, Fin1), t(T2, St2, Fin2) | Rest], [t(T2, St2, Fin2), t(T1, St1, Fin1) | Rest]) :-
	St1 > St2.
swap([t(T, St, Fin) | Rest], [t(T, St, Fin) | Rest1]) :-
	swap(Rest, Rest1).