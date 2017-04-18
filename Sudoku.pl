:- use_module(library(lists)).
:- use_module(library(between)).

sudoku0(Unsolved, Solved):-
    get_row_size(Unsolved, Size),
    fix_domain(Unsolved, Solved, Size),
    consistent(Solved).

fix_domain([],[],_).
fix_domain([X|TX], [Y|TY], Size):-
    fix(X, Y, Size),
    fix_domain(TX, TY, Size).

fix([],[],_).
fix([0|TX], [Y|TY], Size):-
    between(1, Size, Y),
    Y \= 0,
    fix(TX, TY, Size).
fix([X|TX], [Y|TY], Size):-
    X = Y,
    X \= 0,
    Y \= 0,
    fix(TX, TY, Size).

eq([],[]).
eq([X|TX], [Y|TY]):-
    eql(X,Y),
    eq(TX, TY).

eql([],[]).
eql([X|TX],[Y|TY]):-
    (  X == 0 -> eql(TX, TY);
       ( Y == 0 ->  eql(TX, TY);
    ( X == Y ->  eql(TX, TY)) )).

consistent(Puzzle):-
    check_rows(Puzzle),
    transpose(Puzzle, Transposed),
    check_rows(Transposed),
    check_blocks(Puzzle).

check_rows([]).
check_rows([X|T]):-
    check_row(X),
    check_rows(T).

check_row([]).
check_row([X|T]):-
    ( X \= 0 -> all_different_or_zero(T, X); true),
    check_row(T).
    
all_different_or_zero([], _).
all_different_or_zero([H|T], X) :-
    X \= H, all_different_or_zero(T, X).

check_blocks([]).
check_blocks(Puzzle) :-
    get_row_size(Puzzle, RowSize),
    BlockSize is integer(sqrt(RowSize)),
    get_blocks(Puzzle, BlockSize, 0, Blocks),
    check_rows(Blocks).
   
get_row_size([X|_], Size) :-
    length(X, Size).

get_blocks(_, BlockSize, Twice, []):-
    Twice is BlockSize*BlockSize.
get_blocks(Puzzle, BlockSize, Row, Blocks):-
    get_all_row(Puzzle, BlockSize, Row, 0, Ans),
    NextRow is Row + BlockSize,
    get_blocks(Puzzle, BlockSize, NextRow, Ans2),
    append(Ans, Ans2, Blocks),
    !.

get_all_row(_, BlockSize, _, Twice, []) :-
    Twice is BlockSize*BlockSize.
get_all_row(Puzzle, BlockSize, Row, Col, [Ans|Ans2]):-
    get_block_by_coor(Puzzle, Row, Col, BlockSize, Ans),
    NextCol is Col + BlockSize,
    get_all_row(Puzzle, BlockSize, Row, NextCol, Ans2).
    
get_block_by_coor(Puzzle, StartRow, StartCol, Size, Block):-
    sublist(Puzzle, Part, StartRow, Size, _),
    get_part_of_row(Part, StartCol, Size, Block).

get_part_of_row([],_,_,[]).
get_part_of_row([X|T], StartCol, Size, Block):-
    sublist(X, Ans, StartCol, Size, _),
    get_part_of_row(T, StartCol, Size, Smaller),
    append(Ans, Smaller, Block).

sublist(List, Part, Before, Length, After):-
    append(List1, Aft, List),
    append(Bef, Part, List1),
    length(Part,Length),
    length(Bef,Before),
    length(Aft,After).
