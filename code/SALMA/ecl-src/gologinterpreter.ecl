/*                A GOLOG INTERPRETER IN ECLIPSE PROLOG

                             Feb. 6, 1998

This software was developed by the Cognitive Robotics Group under the
direction of Hector Levesque and Ray Reiter.  

       Do not distribute without permission.
       Include this notice in any copy made.


        Copyright (c) 1992-1997 by The University of Toronto,
                       Toronto, Ontario, Canada.

                         All Rights Reserved

Permission to use, copy, and modify, this software and its
documentation for research purpose is hereby granted without fee,
provided that the above copyright notice appears in all copies and
that both the copyright notice and this permission notice appear in
supporting documentation, and that the name of The University of
Toronto not be used in advertising or publicity pertaining to
distribution of the software without specific, written prior
permission.  The University of Toronto makes no representations about
the suitability of this software for any purpose.  It is provided "as
is" without express or implied warranty.

THE UNIVERSITY OF TORONTO DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS
SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND
FITNESS, IN NO EVENT SHALL THE UNIVERSITY OF TORONTO BE LIABLE FOR ANY
SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER
RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF
CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

**********************************************************************/

:- set_flag(print_depth,100). 
%:- nodbgcomp.
:- dynamic(proc/3).            /* Compiler directives. Be sure   */
:- dynamic(restoreSitArg/3).
% :- set_flag(all_dynamic, on).  /* that you load this file first! */

:- op(800, xfy, [&]).   /* Conjunction */ 
:- op(850, xfy, [v]).   /* Disjunction */ 
:- op(870, xfy, [=>]).  /* Implication */
:- op(880,xfy, [<=>]).  /* Equivalence */
:- op(950, xfy, [:]).   /* Action sequence */
:- op(960, xfy, [#]).   /* Nondeterministic action choice */
 
 
is_primitive_action(E) :-
	E =.. [ActionName | _],
	primitive_action(ActionName, _).
	



do2(E1 : E2,S,S1) :- do2(E1,S,S2), do2(E2,S2,S1).
do2(?(P),S,S) :- holds(P,S).
do2(E1 # E2,S,S1) :- do2(E1,S,S1) ; do2(E2,S,S1).
do2(if(P,E1,E2),S,S1) :- do2((?(P) : E1) # (?(not(P)) : E2),S,S1).
do2(star(E),S,S1) :- S1 = S ; do2(E : star(E),S,S1).
do2(while(P,E),S,S1):- do2(star(?(P) : E) : ?(not(P)),S,S1).
do2(pi(V,E),S,S1) :- 
	V = [VarName, Type],
	domain(Type, D),
	member(Value, D),
	sub(VarName,Value,E,E1), do2(E1,S,S1).
	
do2(E,S,do2(E,S)) :- is_primitive_action(E), poss(E,S).



do2(E,S,S1) :- 
	E =.. [ProcName | ParamValues],
	proc(ProcName,Signature,OriginalBody), 
	apply_proc_args(ParamValues, Signature, OriginalBody, GroundBody),	
	do2(GroundBody,S,S1).

apply_proc_args(ParamValues, Signature, OriginalBody, GroundBody) :-
	(foreach(Value, ParamValues), foreach(Param, Signature), fromto(OriginalBody, In, Out, GroundBody) do
		Param = (ParamName : _),
		sub(ParamName, Value, In, Out)
	).
/* sub(Name,New,Term1,Term2): Term2 is Term1 with Name replaced by New. */

sub(_,_,T1,T2) :- var(T1), T2 = T1.
sub(X1,X2,T1,T2) :- not var(T1), T1 = X1, T2 = X2.
sub(X1,X2,T1,T2) :- not T1 = X1, T1 =..[F|L1], sub_list(X1,X2,L1,L2),
                    T2 =..[F|L2].
sub_list(_,_,[],[]).
sub_list(X1,X2,[T1|L1],[T2|L2]) :- sub(X1,X2,T1,T2), sub_list(X1,X2,L1,L2).

/* The holds predicate implements the revised Lloyd-Topor
   transformations on test conditions.  */

% holds(P & Q,S) :- holds(P,S), holds(Q,S).
% holds(P v Q,S) :- holds(P,S); holds(Q,S).
% holds(P => Q,S) :- holds(-P v Q,S).
% holds(P <=> Q,S) :- holds((P => Q) & (Q => P),S).
% holds(-(-P),S) :- holds(P,S).
% holds(-(P & Q),S) :- holds(-P v -Q,S).
% holds(-(P v Q),S) :- holds(-P & -Q,S).
% holds(-(P => Q),S) :- holds(-(-P v Q),S).
% holds(-(P <=> Q),S) :- holds(-((P => Q) & (Q => P)),S).
% holds(-all(V,P),S) :- holds(some(V,-P),S).
% holds(-some(V,P),S) :- not holds(some(V,P),S).  /* Negation */
% holds(-P,S) :- isAtom(P), not holds(P,S).     /* by failure */
% holds(all(V,P),S) :- holds(-some(V,-P),S).
% holds(some(V,P),S) :- sub(V,_,P,P1), holds(P1,S).

/* The following clause treats the holds predicate for non fluents, including
   Prolog system predicates. For this to work properly, the GOLOG programmer
   must provide, for all fluents, a clause giving the result of restoring
   situation arguments to situation-suppressed terms, for example:
         restoreSitArg(ontable(X),S,ontable(X,S)).             */


		 
% holds(A,S) :- restoreSitArg(A,S,F), F ;
              % not restoreSitArg(A,S,F), isAtom(A), A.

% isAtom(A) :- not (A = -W ; A = (W1 & W2) ; A = (W1 => W2) ;
    % A = (W1 <=> W2) ; A = (W1 v W2) ; A = some(X,W) ; A = all(X,W)).

% restoreSitArg(poss(A),S,poss(A,S)).

holds(A,S) :-
	compile_formula(A, F, S), 
	time(T, S),
	evaluate_formula(null, [0], T, F, 0, Result, _, _, _),
	Result = ok.
	


		