# Dots And Boxes Game in Haskell

This is the classic Dots And Boxes game written in Haskell, with a basic command line UI and a dog-awful AI :-) that you can easily beat if you count chains for example.

To build do a

	cabal sandbox init  
	cabal install --only-dependencies  
	cabal build  
	dist\build\dotsandboxes-exe\dotsandboxes-exe.exe  

For the last line, I will be silly and assume you are a Windows user :-)

Output looks something like this:

	c:\Data\programming\dotsandboxes>dist\build\dotsandboxes-exe\dotsandboxes-exe.exe
	Welcome to dots and boxes.
	To play a move enter the row letter, the column letter, then the orientation (either dash: - or one: 1). E.g. aa-
	 a b c d
	a. . . .

	b. . . .

	c. . . .

	d. . . .

	It is the turn of Player1
	Please enter your move below or q to quit
	a-
	Move is not valid, please enter another move.
	aa-
	 a b c d
	a.-. . .

	b.-. . .

	c. . . .

	d. . . .

	It is the turn of Player1
	Please enter your move below or q to quit
	:q
	Move is not valid, please enter another move.
	q
	quit game. bye!
