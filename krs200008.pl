% Keshav Santhanam 
% 4-4-23
% CS 4337.HON

% 1) Second Minimum
secondMin(List,Second):-
    all(List),
    diff(List),
    min(List,First),
    final(List,First,Second).
all([]).
all([X|List]):-
    number(X),!,all(List),!. 
all([X|_]):-
    write("ERROR: "),
    write(X),
    writeln(" is not a number."),false,!.  
min([X],X). % identifying the min position
min([X|List],X):-
    min(List,Y),X<Y,!.
min([X|List],Y):-
    min(List,Y),
    not(X<Y),!.  
diff([_]):- % two subroutines for comparing elements
    writeln("ERROR: List has fewer than two unique elements."),false,!. 
diff([X|List]):-
    diff_helper(X,List).  
diff_helper(X,[X]):-
    writeln("ERROR: List has fewer than two unique elements."),false,!. 
diff_helper(X,[X|List]):-
    diff_helper(X,List),!. 
diff_helper(X,[Y|_]):-
    not(X=Y),!.  
final([X],First,X):- % selects for the 2nd lowest value
    X>First,!. 
final([X|List],First,Y):-
    X=First,!,
    final(List,First,Y),!. 
final([X|List],First,Y):-
    final(List,First,Y),X>Y,Y>First,!. 
final([X|List],First,X):-
    final(List,First,Y),not(X>Y),!.

% 2) Classify
classify([],[],[]). % empty case
classify([X|Y],[X|Even],Odd) :- % selects internal even ints
    0 is X mod 2,!,
    classify(Y,Even,Odd).
classify([X|Y],Even,[X|Odd]) :- % same but for odd ints
    1 is X mod 2,!,
    classify(Y,Even,Odd).

% 3) Subslice
subslice([],_). % empty case
subslice([X|X1],[X|Y1]):-
    prefix(X1,Y1),!. % beginning of each segment
subslice(X1,[_|Y1]):-
    subslice(X1,Y1),!.
prefix([],_). % no prefix returned for base case (run out of chars)
prefix([X|X1],[X|Y1]):-
    prefix(X1,Y1),!.

% 4) Shift
shift(List,N,Shifted) :-
    length(List,Len),
    N1 is Len-(N mod Len), % shifting by Len doesn't change the array
    (N1 >= 0->
    append(R,F,List),
    length(F,N1),
    append(F,R,Shifted)
    ; N2 is Len + N1, % boosts N1 to a high enough N value
    shift(List,N2,Shifted)),!.

% 5) Luhn Algorithm
luhn(N):-     
    calc(N,0,Result),   
    Remainder is mod(Result,10), % rightmost digit of the Result
    Remainder = 0. 
calc(0,Sum,Result):-     
    Result is Sum,!. 
calc(N,Sum,Result):-  
    Remainder is mod(N,100), % 2 rightmost digits of the entire given integer
    Y is div(N,100),    
    doubles(Remainder,S2),  
    SS is Sum+S2, 
    calc(Y,SS,Result),!.
combineDigits(N,Result):- % combines only the rightmost digit and preceding number
    N1 is mod(N,10),
    N2 is div(N,10),
    Result is N1+N2,!. 
doubles(N,Result):- 
    N1 is mod(N,10),
    N2 is div(N,10),     
    N3 is N2*2,     
    N3>9 -> combineDigits(N3,X), % push rightmost digit in
    Result is X+N1,!;
    N1 is mod(N,10),   
    N2 is div(N,10),  
    N3 is N2*2,  
    Result is N1+N3,!. 

% 6) Graph
% info taken from the pdf
edge(a,b).
edge(b,c).
edge(c,d).
edge(d,a).
edge(d,e).
edge(b,a).
% code
cycle(X):-
    path(X,_),!.
path(X,Y):-
    edge(X,Y),!. % base case of recursion
path(X,Y):-
    edge(X,R),
    path(R,Y),!. % draw path recursively

% 10) Clue
% starting info
person(colMustard).
person(profPlum).
person(missScarlet).
person(msGreen).
person(mrBoddy).
rich(mrBoddy). 
rich(colMustard). % REMOVE THIS LINE FOR 2 POSSIBLE VICTIMS
affair(msGreen,mrBoddy).
affair(missScarlet,mrBoddy).
married(profPlum,msGreen).
greedy(colMustard). 
% relationship predicates
hasgreed(X,Y) :- 
    greedy(X), 
    not(rich(X)),
    rich(Y).
hates(X,Y) :- 
    married(X,Z), 
    affair(Z,Y).
% suspect/2 predicates
suspect(Killer,Victim) :-
    hasgreed(Killer,Victim).
suspect(Killer,Victim) :-
	hates(Killer,Victim). 

% 7) Zebra Puzzle
% starting info
shirt_colors(black). 
shirt_colors(blue). 
shirt_colors(green). 
shirt_colors(red). 
shirt_colors(white). 
nationality(american).
nationality(canadian). 
nationality(dutch). 
nationality(french). 
nationality(scottish). 
genre(drumandbass). 
genre(dubstep). 
genre(edm). 
genre(house).
genre(trance). 
stage(arcadia). 
stage(asgard). 
stage(shangrila).
stage(valhalla). 
stage(xibalba). 
age(25). 
age(30). 
age(35). 
age(40). 
age(45).
hobby(camping).
hobby(juggling). 
hobby(painting). 
hobby(singing). 
hobby(surfing). 
% "subroutines" for relative positions of DJs
select([A|As],S):-
	select(A,S,S1),select(As,S1).
select([],_). 
adj(A,B,C):-
	left_of(A,B,C);
	left_of(B,A,C).
left_of(A,B,C):-
	append(_,[A,B|_],C). 
right_of(A,B,C):-
	left_of(B,A,C).
left_of2(A,B,C):-
   select([A,B],C).
right_of2(A,B,C):-
    select([B,A],C).
between(A,B,C,D):-
   select([B,A,C],D).
edges(A,[A]).
edges(A,[_|T]):-
    edges(A, T).
edges(A,[A|_]). % prevent overrunning the length of the list
dj(Shirt_color, Nationality, Genre, Stage, Age, Hobby):-
    shirt_colors(Shirt_color),
    nationality(Nationality),
    genre(Genre),
    stage(Stage),
    age(Age),
    hobby(Hobby).
getDJs(DJs):- % underscores indicate the necessary spots to fill with traits from other DJs 
    DJs = [dj(_,_,_,_,30,_),_,dj(_,_,_,asgard,_,_),dj(_,_,_,arcadia,_,_),dj(_,_,drumandbass,_,_,_)],
    edges(dj(_,_,_,_,_,camping),DJs),
    DJs = [_,_,_,dj(_,_,_,_,40,_),_],
    left_of2(dj(_,scottish,_,_,_,_),dj(white,_,_,_,_,_),DJs),
    right_of(dj(_,_,edm,_,_,_),dj(_,canadian,_,_,_,_),DJs),
    adj(dj(_,_,_,_,_,painting),dj(_,_,dubstep,_,_,_),DJs),
    between(dj(black,_,_,_,_,_),dj(_,scottish,_,_,_,_),dj(_,_,dubstep,_,_,_),DJs),
    adj(dj(_,french,_,_,_,_),dj(blue,_,_,_,_,_),DJs),
    select([dj(blue,_,_,asgard,_,_)],DJs),
    between(dj(_,_,_,_,_,painting),dj(green,_,_,_,_,_),dj(blue,_,_,_,_,_),DJs),
    adj(dj(_,_,trance,_,_,_),dj(_,_,dubstep,_,_,_),DJs),
    left_of(dj(_,canadian,_,_,_,_),dj(_,_,_,_,_,juggling),DJs),
    right_of(dj(_,_,_,_,_,singing),dj(black,_,_,_,_,_),DJs),
    adj(dj(_,_,_,_,35,_),dj(_,_,_,_,_,juggling),DJs),
    between(dj(_,_,_,_,40,_),dj(_,dutch,_,_,_,_),dj(_,_,_,_,25,_),DJs),
    left_of2(dj(blue,_,_,_,_,_), dj(_,_,_,xibalba,_,_),DJs),
    member(dj(_,_,_,valhalla,_,surfing),DJs),
    right_of2(dj(red,_,_,_,_,_),dj(_,french,_,_,_,_),DJs),
    member(dj(_,american,_,_,_,_),DJs),
	member(dj(_,_,_,_,_,camping),DJs),
	member(dj(_,_,_,_,45,_),DJs),
	member(dj(_,_,_,shangrila,_,_),DJs),
    member(dj(_,_,house,_,_,_),DJs), 
    !.



















































