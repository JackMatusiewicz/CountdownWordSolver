# CountdownWordSolver
A program that, given a set of letters, can find all possible words you can make from said letters.

The approach I'm using here is different from the one I've done previously. Normally I would create a dictionary of letters
(letter, number of occurrences) for the game input, then I would iterate through the dictionary creating similar dictionaries
and check that the set created from the dictionary word is a subset. If it is then that word can be made.

Here I've decided to go with an approach of creating an immutable trie structure to store my dictionary, I then create all
possible permutations of the game input and walk it through the trie, keeping track of the words I find.

This hasn't yet been checked for perf, and the way I am generating the permutations is probably far too slow. 
