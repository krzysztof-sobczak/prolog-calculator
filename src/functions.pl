%global limit for iterations
iterations(Iterations):-
	Iterations is 50.
	
% check if number is even
isEven(V):-
	Modulo is V mod 2,
	Modulo is 0.

% factorial of 0 is always 1
factorial(0,1).
% factorial of x is v
factorial(X,V):-
	X > 0,
	X1 is X - 1,
        factorial(X1,Z),
        V is Z*X,!.
% power of x to 0 is always 1
power(X,0,1).
% power of x to y is v
power(X,Y,V):-
	Y > 0,
	Y1 is Y - 1,
	power(X,Y1,Z),
	V is Z*X,!.
% initial state for recursive iteration
exp(X,0,V):-
	V is 1.
% exp iterative calculation
exp(X,Iterations,V):-
	Iterations1 is Iterations - 1,
	exp(X,Iterations1,Z),
	Power is Iterations,
	factorial(Power,Factorial),
	V is Z + X ^ Power / Factorial,!.
% exp of x is v
exp(X,V):-
	iterations(Iterations),
	exp(X,Iterations,V).
% initial state for recursive iteration
binomial(X,A,0,V):-
	V is 1.
% binomial iterative calculation
binomial(X,A,Iterations,V):-
	Iterations1 is Iterations - 1,
	binomial(X,A,Iterations1,Z),
	factorial(A,FactorialTop),
	factorial(Iterations,FactorialBottom1),
	K is A - Iterations,
	factorial(K,FactorialBottom2),
	V is Z + (FactorialTop / (FactorialBottom1 * FactorialBottom2)) * X ^ Iterations,!.
% binomial of (1 + x)^a is v
binomial(X,A,V):-
	iterations(Iterations),
	binomial(X,A,A,V).
% initial state for recursive iteration
ln(X,0,V):-
	V is 0.
% ln iterative calculation
ln(X,Iterations,V):-
	Iterations1 is Iterations - 1,
	ln(X,Iterations1,Z),
	Power is Iterations + 1,
	V is Z + (-1)^Power / Iterations * (X - 1)^Iterations,!.
% ln of x is v, where x is from range (0,2)
ln(X,V):-
	iterations(Iterations),
	ln(X,Iterations,V).
% initial state for recursive iteration
sqrt(X,0,V):-
	V is 1.
% sqrt iterative calculation (Taylor series)
sqrt(X,Iterations,V):-
	Iterations1 is Iterations - 1,
	sqrt(X,Iterations1,Z),
	factorial(2*Iterations,FactorialTop),
	factorial(Iterations,FactorialBottom),
	V is Z + (((-1)^Iterations * FactorialTop) / ((1 - 2 * Iterations) * FactorialBottom^2 * 4^Iterations)) * X^Iterations,!.
% sqrt of x is v, where x is in range (0,2)
sqrt(X,V):-
	iterations(Iterations),
	X1 is X - 1,
	sqrt(X1,Iterations,V).
% initial state for recursive iteration
sin(X,0,V):-
	V is X.
% sinus iterative calculation
sin(X,Iterations,V):-
	Iterations1 is Iterations - 1,
	sin(X,Iterations1,Z),
	Power is Iterations * 2 + 1,
	factorial(Power,Factorial),
	V is Z + (-1)^Iterations * X^Power / Factorial,!.
% sinus of x is v
sin(X,V):-
	iterations(Iterations),
	sin(X,Iterations,V).
	% initial state for recursive iteration
sinh(X,0,V):-
	V is X.
% sinush iterative calculation
sinh(X,Iterations,V):-
	Iterations1 is Iterations - 1,
	sinh(X,Iterations1,Z),
	Power is Iterations * 2 + 1,
	factorial(Power,Factorial),
	V is Z + X^Power / Factorial,!.
% sinush of x is v
sinh(X,V):-
	iterations(Iterations),
	sinh(X,Iterations,V).
% initial state for recursive iteration
cos(X,0,V):-
	V is 1.
% cosinus iterative calculation
cos(X,Iterations,V):-
	Iterations1 is Iterations - 1,
	cos(X,Iterations1,Z),
	Power is Iterations * 2,
	factorial(Power,Factorial),
	V is Z + (-1)^Iterations * X^Power / Factorial,!.
% cosinus of x is v
cos(X,V):-
	iterations(Iterations),
	cos(X,Iterations,V).
% initial state for recursive iteration
cosh(X,0,V):-
	V is 1.
% cosinush iterative calculation
cosh(X,Iterations,V):-
	Iterations1 is Iterations - 1,
	cosh(X,Iterations1,Z),
	Power is Iterations * 2,
	factorial(Power,Factorial),
	V is Z + X^Power / Factorial,!.
% cosinush of x is v
cosh(X,V):-
	iterations(Iterations),
	cosh(X,Iterations,V).
	
	
% Additionals
	
% solve(Left=Right)
%
% On entry, Left=Right is an arithmetic
% equation containing an uninstantiated
% variable.
%
% On exit, that variable is instantiated
% to an approximate numeric solution.
%
% The syntax of Left and Right is the same
% as for expressions for the 'is' predicate.
solve(Left=Right) :-
	free_in(Left=Right,X),
	!,/* accept only one solution of free_in */
	define_dif(X,Left=Right),
	solve_for(X).
% free_in(Term,Variable)
%
% Variable occurs in Term and is uninstantiated.
free_in(X,X) :- % An atomic term
	var(X).
free_in(Term,X) :- % A complex term
	Term \== [[]],
	Term =.. [_,Arg|Args],
	(free_in(Arg,X) ; free_in(Args,X)).
% define_dif(X,Left=Right)
% Defines a predicate to compute Left-Right
% for the specified equation, given X.
define_dif(X,Left=Right) :-
	abolish(dif,2),
	assert((dif(X,Dif) :- Dif is Left-Right)).
% solve_for(Variable)
%
% Sets up arguments and calls solve_aux (below).
solve_for(Variable) :-
	dif(1,Dif1),
	solve_aux(Variable,1,Dif1,2,1).
% solve_aux(Variable,Guess1,Dif1,Guess2,Iteration)
%
% Uses the secant method to find a value of
% Variable that will make the 'dif' procedure
% return a value very close to zero.
%
% Arguments are:
% Variable -- Will contain result.
% Guess1 -- Previous estimated value.
% Dif1 -- What 'dif' gave with Guess1.
% Guess2 -- A better estimate.
% Iteration -- Count of tries taken.
solve_aux(cannot_solve,_,_,_,100) :-
	!,
	write('[Gave up at 100th iteration]'),nl,
	fail.
solve_aux(Guess2,Guess1,_,Guess2,_) :-
	close_enough(Guess1,Guess2),
	!,
	write('[Found a satisfactory solution]'),nl.
solve_aux(Variable,Guess1,Dif1,Guess2,Iteration) :-
	write([Guess2]),nl,
	dif(Guess2,Dif2),
	Slope is (Dif2-Dif1) / (Guess2-Guess1),
	Guess3 is Guess2 - (Dif2/Slope),
	NewIteration is Iteration + 1,
	solve_aux(Variable,Guess2,Dif2,Guess3,NewIteration).
% close_enough(X,Y)
%
% True if X and Y are the same number to
% within a factor of 0.0001.
%
close_enough(X,Y) :-
	Quot is X / Y,
	Quot > 0.9999,
	Quot < 1.0001.