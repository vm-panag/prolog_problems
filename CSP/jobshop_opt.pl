%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Vassilis Panagakis         %
% jobshop_opt.pl             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- set_flag(print_depth, 1000).

:- lib(ic).
:- lib(branch_and_bound).

/* uncomment following line in case you haven't compiled jobshop_opt_data.pl file */

/* :- compile(jobshop_opt_data). */

/*** predicate that finds the optimal scheduling of a number of tasks ***/

jobshop_opt(Jobs, Staff, Schedule, Cost, Delta, Timeout) :-
	findall(Dur, task(_, _, Dur, _), Durations),
	max_deadline(Durations, 0, Deadline),				%get maximum deadline
	findall([M, Num], machine(M, Num), MacL),	
	enumerate_machines(MacL, 0, MacTypes),
	findall(TL, (member(J, Jobs), job(J, TL)), Tasks),		%get lists of all tasks in Jobs
	job_vars(Tasks, Deadline, MacTypes, Mytasks),			%define variables and domains of tasks and machines
	job_constrs(Mytasks),						%constraints concerning jobs and tasks 
	fixed_flatten(Mytasks, Flatasks),
	machine_constrs(Flatasks),					%constraints concerning machines
	allbetween(0, Deadline, Timeslots),
	staff_constrs(Timeslots, Staff, Flatasks),			%constraints concerning staff
	finish_times(Flatasks, FinTime),					
	Cost #= max(FinTime),						%the cost of the program is the maximum finish time among all tasks
	machines(Flatasks, Macs),
	append(Macs, FinTime, Vars),
	bb_min(search(Vars, 0, input_order, indomain, complete, []), Cost, bb_options{delta:Delta, timeout:Timeout}),
	split_list(Vars, MacVars, FinVars),
	find_solution(MacVars, FinVars, Flatasks),
	findall(MT, member(mac(_, MT), MacTypes), MD),
	fixed_flatten(MD, MacDom),					%get a list of all unique machine ids
	schedule(MacDom, MacTypes, Flatasks, Schedule).			%create a schedule containing all machines and the tasks they execute


/* predicate where variables and domains of every task in the list of jobs are defined */

job_vars([], _, _, []).
job_vars([J | TL], Deadline, MacTypes, [Task | Mytasks]) :-
	def_vars(J, Deadline, MacTypes, Task),
	job_vars(TL, Deadline, MacTypes, Mytasks).

/* predicate where variables and their domains are defined */

def_vars([], _, _, []).
def_vars([T | Tasks], Deadline, MacTypes, [mytask(T, St, Fin, Mac) | Mytasks]) :-
	St #:: 0..Deadline,
	Fin #:: 0..Deadline,
	task(T, M, _, _),
	member(mac(M, MacNo), MacTypes),
	Mac #:: MacNo,
	def_vars(Tasks, Deadline, MacTypes, Mytasks).

/* predicate where independent and dependent time constraints of every task in the list of jobs are defined */
	
job_constrs([]).
job_constrs([J | TL]) :-
	indep_time_constrs(J),
	dep_time_constrs(J),	
	job_constrs(TL).

/* predicate that defines the constraint between start and finish time for every task */

indep_time_constrs([]).
indep_time_constrs([mytask(T, St, Fin, _) | Mytasks]) :-
	task(T, _, Dur, _),
	Fin #= St + Dur,
	indep_time_constrs(Mytasks).	

/* predicate that defines the priority constraints among all tasks of the same job */

dep_time_constrs([]).
dep_time_constrs([Task | Mytasks]) :-
	priority_constrs(Task, Mytasks),
	dep_time_constrs(Mytasks).

/* predicate that defines the priority constraints between a specific task and every other task of the same job */

priority_constrs(_, []).
priority_constrs(mytask(T1, _, Fin1, _), [mytask(_, St2, _, _) | Mytasks]) :-
	Fin1 #=< St2,
	priority_constrs(mytask(T1, _, Fin1, _), Mytasks).

/* predicate that defines the constraints among the tasks that are executed in the same unique machine */

machine_constrs([]).
machine_constrs([Task | Flatasks]) :-
	overlapping_constrs(Task, Flatasks),
	machine_constrs(Flatasks).

/* predicate that defines the machine overlapping constraints for all tasks that are executed in the same unique machine */

overlapping_constrs(_, []).
overlapping_constrs(mytask(T1, St1, Fin1, Mac1), [mytask(T2, _, _, _) | Flatasks]) :- 		%if tasks T1, T2 belong in the same job, don't apply the constraint
	job(_, TL), 
	member(T1, TL), 
	member(T2, TL),
	overlapping_constrs(mytask(T1, St1, Fin1, Mac1), Flatasks).
overlapping_constrs(mytask(T1, St1, Fin1, Mac1), [mytask(T2, St2, Fin2, Mac2) | Flatasks]) :-	%if tasks T1, T2 belong in different jobs, apply the constraint
	job(_, TL), 
	member(T1, TL), 
	not member(T2, TL),	
	Con1 #= (Mac1 #\= Mac2),	
	Con2 #= (Fin1 #=< St2),		
	Con3 #= (Fin2 #=< St1),		
	Con1+Con2+Con3 #> 0,						%(Mac1 #= Mac2) => ((Fin1 #=< St2) or (Fin2 #=< St1))
	overlapping_constrs(mytask(T1, St1, Fin1, Mac1), Flatasks).


/* predicate that defines the staff constraints for every timeslot */

staff_constrs([], _, _).
staff_constrs([TS | Timeslots], Staff, Flatasks) :-
	timeslot_constrs(TS, Flatasks, ReqStaff),
	sum(ReqStaff) #=< Staff,					%the sum of required staff at each timeslot must be at most equal to the available staff
	staff_constrs(Timeslots, Staff, Flatasks).

/* predicate that returns the required staff of every task for a specific timeslot */

timeslot_constrs(_, [], []).
timeslot_constrs(TS, [mytask(T, St, Fin, _) | Flatasks], [RS | ReqStaff]) :-
	task(T, _, _, S),						%get the required staff for task T
	Con1 #= (St #> TS),					
	Con2 #= (TS #>= Fin),		
	Res1 #= (RS #= S),			
	Con1+Con2+Res1 #> 0,						%(St #=< TS and TS #< Fin) => (RS #= S)
	Con3 #= (TS #>= St),
	Con4 #= (TS #< Fin),
	Res2 #= (RS #= 0),
	Comb1 #= (Con3 + Res2),
	Comb2 #= (Con4 + Res2),
	Comb1 #> 0,
	Comb2 #> 0,
	Comb1+Comb2 #> 1,						%(TS #< St or TS #>= Fin) => (RS #= 0) //// (TS #>= St or RS #= 0) and (TS #< Fin or RS #= 0)  
	timeslot_constrs(TS, Flatasks, ReqStaff).

/* predicate that returns the final time schedule */	

schedule([], _, _, []).
schedule([M | MacDom], MacTypes, Flatasks, [execs(MT, Sch1) | Schedule]) :-
	member(mac(MT, ML), MacTypes),
	member(M, ML),
	machine_scheduling(M, Flatasks, Sch),
	bubblesort(Sch, Sch1),
	schedule(MacDom, MacTypes, Flatasks, Schedule).

/* predicate that returns the time schedule of a machine */	

machine_scheduling(_, [], []).
machine_scheduling(M, [mytask(_, _, _, Mac) | Flatasks], Sch) :-
	M \= Mac,
	machine_scheduling(M, Flatasks, Sch).
machine_scheduling(M, [mytask(T, St, Fin, M) | Flatasks], [t(T, St, Fin) | Sch]) :-
	machine_scheduling(M, Flatasks, Sch).

/* predicate that returns a list of all tasks' finish times */	

finish_times([], []).
finish_times([mytask(_, _, Fin, _) | Flatasks], [Fin | FinL]) :-
	finish_times(Flatasks, FinL).

/* predicate that returns a list of available machines for every task */

machines([], []).
machines([mytask(_, _, _, Mac) | Flatasks], [Mac | MacL]) :-
	machines(Flatasks, MacL).

/* predicate that assigns the values of all tasks' start time variable */

find_solution([], [], []).
find_solution([MV | MacVars], [FV | FinVars], [mytask(_, _, Fin, Mac) | Flatasks]) :-
	Fin = FV,
	Mac = MV,
	find_solution(MacVars, FinVars, Flatasks).

/* predicate that matches each machine type with some unique machine ids */

enumerate_machines([], _, []).
enumerate_machines([[M, Num] | MacL], Ind, [mac(M, MacInd) | MacTypes]) :-
	Num1 is Num + Ind,
	enumerate_machine([M, Num1], Ind, MacInd),
	get_last(MacInd, LastInd),
	NextInd is LastInd + 1,
	enumerate_machines(MacL, NextInd, MacTypes).

/* predicate that matches a specific machine type with some unique machine ids */

enumerate_machine([_, Num1], Num1, []).
enumerate_machine([M, Num], MI, [MI | MacInd]) :-
	MI1 is MI + 1,
	MI1 =< Num,
	enumerate_machine([M, Num], MI1, MacInd).
	
/* predicates that returns the worst possible time required for a successful scheduling */	
	
max_deadline([], Deadline, Deadline).	
max_deadline([Dur | Durations], D, Deadline1) :-
	Deadline2 is D + Dur,
	max_deadline(Durations, Deadline2, Deadline1).

/* predicate that creates a list containing all integers between N1 and N2 */

allbetween(N1, N2, []):-
	N1 > N2.
allbetween(N1, N2, [N1 | Xs]):-
	N1 =< N2,
	NewN1 is N1 + 1,
	allbetween(NewN1, N2, Xs).

/* predicate that transforms a list of lists into a flat list */

fixed_flatten([], []) :- !.
fixed_flatten([L1 | L2], FL) :-
   !,
   fixed_flatten(L1, FL1),
   fixed_flatten(L2, FL2),
   append(FL1, FL2, FL).
fixed_flatten(X, [X]).

/* predicate that splits a list in half */

split_list(L, L1, L2) :-
    append(L1, L2, L),
    length(L1, N),
    length(L2, N).

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