% ----START THE GAME----
start:-
    pawnPick(_,_).

%Print indications to play
showGuide:-
    write('* * * Pawn identifier * * *'),nl,nl,
    write('* 1 : b (black) or w (white) *'),nl,
    write('* 2 : s (square) or c (circle) *'),nl,
    write('* 3 : t (tall) or l (low) *'),nl,
    write('* 4 : e (empty) or f (full) *'),nl,nl,
    write('Example : bste = black/square/tall/empty'),nl,nl,
    write('* * * * * * * * * * * * * * * * *').

% ----QUARTO'S PAWNS----

%Definition (color, shape, height, emptiness)

pawn(wste). %white/square/tall/empty
pawn(wstf). %white/square/tall/full
pawn(wsle). %white/square/low/empty
pawn(wslf). %white/square/low/full
pawn(wcte). %white/circle/tall/empty
pawn(wctf). %white/circle/tall/full
pawn(wcle). %white/circle/low/empty
pawn(wclf). %white/circle/low/full
pawn(bste). %black/square/tall/empty
pawn(bstf). %black/square/tall/full
pawn(bsle). %black/square/low/empty
pawn(bslf). %black/square/low/full
pawn(bcte). %black/circle/tall/empty
pawn(bctf). %black/circle/tall/full
pawn(bcle). %black/circle/low/empty
pawn(bclf). %black/circle/low/full

%All Quarto's pawns
listPawns([wste,wstf,wsle,wslf,
             wcte,wctf,wcle,wclf,
             bste,bstf,bsle,bslf,
             bcte,bctf,bcle,bclf]).

%Pawns which have been placed. Allow to know if a play is legal or not.
pawnLaid([' 01 ', ' 02 ', ' 03 ',' 04 ',
          ' 05 ', ' 06 ', ' 07 ', ' 08 ',
          ' 09 ', ' 10 ', ' 11 ', ' 12 ',
          ' 13 ', ' 14 ', ' 15 ',' 16 ']).

% ----GAMEPLAY----

%Running of the first turn
pawnPick(Code,X):-
    write('Welcome to our Quarto.'),nl,nl,
    write('The aim of the game is to line up four pawns with at least one thing in common. 
           But each player does not play what he wants to, the opponent chooses for him.'),nl,nl,
    %display the pawns code guide
    showGuide,    nl,
    %G is defined as the pawnLaid-list
    pawnLaid(G), 
    %display a fresh grid
    showGrid(G), 
    %L is defined as the the list of pawns
    listPawns(L),nl,
    write('Available Pawns : '), write(L),nl,nl,
    random(0,2,R),
    S is R+1,
    write('Player '),write(S),write(' starts'),
    (   S\=1 ->  
    %Start the main running function with player 2
    pawnPick(Code,X,G,3,0,L); 
    %Start the main running function with player 1
    pawnPick(Code,X,G,2,0,L)). 

%Main running function until the game ends
pawnPick(Code,C,L,T,WinState,A):-
    %Code: locate each pawn
    %C: cell
    %L: list of the pawns on grid
    %T: turn
    %WinState: is there a winner yet (0: no, 1: yes)
    %A: available pawns.
    (   WinState == 0 ->  %no one has won yet
    %ListLettes is defined as the list of pawns already played
    pawnsPlayed(ListLetters), 
    %make the player choose a pawn and test if it's available, Code2 is return if so
    testCode(Code,Code2,T,L),
    %make the player choose a cell and test if it's available, C2 is returned if so 
    testCell(C,C2,L,Code2,T),
    %overwrite the number of the cell by the pawn's code
    replace(C2,L,Code2,K),
    %display instructions to play
    showGuide,nl,
    %show the update grid
    showGrid(K), 
    %test if there is a winner and end the game if so
    won(K,T,0,0,ListLetters), 
    %Increases the number of turn by 1
    T2 is T+1,
    %Remove the code chosen from the Available pawns, and return this new list in A2
    select(Code2,A,A2), 
    nl,
    write('Available pawns : '),
    %A2 is the new list of available pawns
    write(A2), 
    %goes to the next round
    pawnPick(_,_,K,T2,WinState,A2); 
    %Run if a player win
    stopGame(0)). 

%Test if the cell Code is legal
testCode(Code,Code2,T,L):-
    %Code: code of the pawn
    %Code2: code returned if it's available
    %T: number of turn
    %L: list of pawns already played
    nl,nl,    
    %according to the turn, the instructions change
    (   0 is mod(T,2) ->      
    write('P1 chooses a pawn code'),nl,nl,
    %read the player's input for the code   
    read(Code),nl,
    %test if the pawn has already been played
    (   member(Code,L) -> 
    write('Already placed, please choose an other pawn.'),
    %re-running the function
    testCode(_,Code2,T,L); 
    %test if the pawn exists in the list
    (   pawn(Code) ->  Code2 = Code; 
    write('Code unknown, please enter a valid code.'),
    %re-running the function
    testCode(_,Code2,T,L))); 
    %alternative instructions
    write('P2 chooses a pawn code'),nl,nl,
    read(Code),nl,
    (   member(Code,L) ->
    write('Already placed, please choose an other pawn.'),
    testCode(_,Code2,T,L);
    (   pawn(Code) ->  Code2=Code;
    write('Code unknown, please enter a valid Code.'),
    testCode(_,Code2,T,L)))).

%Test if a cell is available
testCell(C,C2,L,Code,T):-
    %C: cell
    %C2: cell returned if it's available
    %L: list of the pawns on the grid
    %Code: code of the selected pawn
    %T: turn
    (   0 is mod(T,2) ->  %according to the turn, the instructions change
    
    write('P2 chooses a cell'),nl,nl,
    %read the player's input for the cell
    read(C),nl,
    (   C =< 16, C > 0 ->  %there are 16 cells
    C1 is C-1,
    %take the C1 th element of L and return it in E
    nth0(C1,L,E), 
    %If this item is a pawn
    (   pawn(E)->  
    %It is indicated that there is already a pawn in it
    write('A pawn has already been placed on this cell, please select an other cell.'),nl,nl,
    %The function is restarted if the cell is already occupied
    testCell(_,C2,L,Code,T); 
    C2=C);
    %it indicates that the cell does not exist
    write('Cell does not exist, please select an other one.'),nl,nl,
    %Relaunch the function if the selected cell does not exist
    testCell(_,C2,L,Code,T)); 
    %The display changes according to the lap
    write('P1 chooses a cell'),nl,nl,
    read(C),nl,
    (   C=<16 ->  
    C1 is C-1,
    nth0(C1,L,E),
    (   pawn(E)->
    write('A pawn has already been placed on this cell, please select an other cell.'),nl,nl,
    testCell(_,C2,L,Code,T) ;
    C2=C);
    write('Cell does not exist, please select an other one.'),nl,nl,
    testCell(_,C2,L,Code,T))).

%Function that replaces the I th element of the list of L by the element E, it returns the result in the list K
replace(I, L, E, K) :-
  nth1(I, L, _, R),
  nth1(I, K, E, R).

% ----END GAME----

%All the victory's positions assembled by lists
winPositions([[1,2,3,4],[5,6,7,8],[9,10,11,12],[13,14,15,16], %Rows
         [1,5,9,13],[2,6,10,14],[3,7,11,15],[4,8,12,16], %Columns
         [1,6,11,16],[4,7,10,13]]). %Diagonals
%Pawns already played
pawnsPlayed([]).

%Transforms the list of grid elements into a list of characters to send to getCharacter
won(L,T,E,Letter,ListLetters):-
    %L is the list of pawns, T is the turn, E is one of the elements of the victory conditions list
    %Letter is the index of the letter of the identifier, ListLetters is the list of letters
    (   Letter =< 3 ->  %Each cell has 4 letters
      (   E=<39 ->  %The set of victory possibilities represents 40 elements (10 times 4 squares)
      %The list of winning positions is called D
    winPositions(D),
    %We use flatten to transform D, which is a list of lists, into W, a simple list
      flatten(D,W),
      %We take cell number E from the flatten list W and return it to Q
    nth0(E,W,Q), 
      %We take in the list L which is the list containing the grid, the element number Q which we return in X
    nth1(Q,L,X), 
      %The desired letter of X is retrieved and returned as Char.
    sub_atom(X,Letter,1,_,Char), 
    %It is incremented by 1
      E1 is E+1, 
    %The Char character is added to a new list  
      append(ListLetters,[Char],NewList), 
    %The function is recalled recursively with the new list and the increment   
      won(L,T,E1,Letter,NewList); 
    %If you have done all 40 possibilities for a letter, you move on to the next letter
      NextLetter is Letter+1,
    %The function is recalled with the following letter
      won(L,T,0,NextLetter,ListLetters));
    %Once the list with all the letters of the tray is finished, we call the function getCharacter
    getCharacter(ListLetters,0,T)). 


%Compare 4 characters in order to know if 4 pawns with a common caracteristic are aligned
%First, it tests if it's a number or not (original cell's format : "nbCell")
%Then we check that it is not a space.
%Then if it is the same characters we end the game.
compareCharacter(C1,C2,C3,C4,T):-
    (   (atom_number(C1,_); atom_number(C2,_); atom_number(C3,_); atom_number(C4,_))
    -> write('');
    ((C1==C2, C2==C3, C3==C4)
    -> (C1==' ' ->
       write('');
       endGame(T));
    write(''))).

%Allows you to retrieve 4 consecutive characters from the list of parts on the board.
%Then the character comparison function is called.
getCharacter(L,I,T):-
    %We run the function 160 times because there are 10 possibilities of victory (4 rows, 4 columns, 2 diagonals)
    %In each possibility there are 4 cells and in each cell 4 characters, so 10*4*4=160.
    %so we have 4 consecutive characters in L1, L2, L3 and L4
    (   I =< 159 ->
    nth0(I,L,L1),
    I1 is I+1,
    I2 is I+2,
    I3 is I+3,
    nth0(I1,L,L2), 
    nth0(I2,L,L3), 
    nth0(I3,L,L4), 
    Ibis is I+4,
    %We call the function that compares the characters
    compareCharacter(L1,L2,L3,L4,T), 
    %The function is recalled using the first index + 4 as a starting index to compare 4 new letters
    getCharacter(L,Ibis,T);  
    write('')).

%Display which player win depending on if the turn is even or odd
endGame(T):-
    P is mod(T,2),
    nl,
    write('End of the game'),
    nl,
    write('Player : '),
    %Even turn which means the player 2 win
    (P==0 ->   
    P2 is P+2,
    write(P2);
    %or it's an odd turn, so the player 1 win
    write(P)), 
    write(' win.'),
    nl,
    %Call starting function indicating that a player has won
    pawnPick(_,_,_,_,1,_). 

%Stop the code and display false
stopGame(I):-
    pawn(I).

% ----DISPLAY FUNCTIONS----

%Show the grid
showGrid(Pawns):-
    nl,
    write('--- Display of the grid ---'),nl,nl,
    %display a line of "-"
    displayLign,nl,
    %display first line's pawns
    displayBoard(1,Pawns),nl,
    displayLign,nl,
    displayBoard(2,Pawns),nl,
    displayLign,nl,
    displayBoard(3,Pawns),nl,
    displayLign,nl,
    displayBoard(4,Pawns),nl,
    displayLign.

%Handle the display of the L-list pawns which belong to the I-line
displayBoard(I,L):-
    (I==1 ->  
    %display a "|" between each pawn
    write('|'), 
    %get the first element of the L-list and set it in X1
    nth0(0,L,X1),write(X1),write('|'),
    nth0(1,L,X2),write(X2),write('|'),
    nth0(2,L,X3),write(X3),write('|'),
    nth0(3,L,X4),write(X4),write('|');
    
    I ==2 ->  
    write('|'),
    nth0(4,L,X5),write(X5),write('|'),
    nth0(5,L,X6),write(X6),write('|'),
    nth0(6,L,X7),write(X7),write('|'),
    nth0(7,L,X8),write(X8),write('|');
    
    I==3 ->  
    write('|'),
    nth0(8,L,X9),write(X9),write('|'),
    nth0(9,L,X10),write(X10),write('|'),
    nth0(10,L,X11),write(X11),write('|'),
    nth0(11,L,X12),write(X12),write('|');
    
    I==4 ->  
    write('|'),
    nth0(12,L,X13),write(X13),write('|'),
    nth0(13,L,X14),write(X14),write('|'),
    nth0(14,L,X15),write(X15),write('|'),
    nth0(15,L,X16),write(X16),write('|')
    ).

%Display a line to divide the pawns
displayLign:-
    write('------------------------').

