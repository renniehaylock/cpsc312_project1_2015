QUESTION 1

My domain is about vehicles and there is one funny one in there about food, but the main subject are vehicles.

The first rule describes a race car, only if it had thick and wide tires and ran on the track with low ride height.

The second describes a bicycle, only if it has thin wheels and sits one person and has a tublar frame.

The third one describes a boat, only if it has external motors and floats on water and has sails.

The fourth and amusing one is a pizza, only if its shape is round and it feeds five people and smells awesome and has cheese toppings.

It was difficult and frustrating to write rules when you aren't sure about the facts of the domain yourself. You would rarely mentally ask yourself what makes this this, which can become troublesome, however what was simple was understanding the syntax of the rules because there was a file for reference. For words that were not know a "n:....." was used to explicitly state that that was a noun or a verb or an adverb or an adjective. The minimum amount of rules needed was 3 and was put in a knowledge base file called vehicles.kb. This satisfies the requirement for question 1.

QUESTION 2

The code for the interpreter loop came largely by modeling after Amzi's interpreter shell discussion. It can be found under the "Interpreter loop" heading in the '312-pess.pl' file. The loop is initiated with the main/0 predicate, which prompts the system to begin listening for incoming commands. After the user types in a command, the system checks to see if it is valid by trying to unify with do(X), X being the command the user inputs. the do(X) predicates just act as a bridge/connector between the interpreter and the predicates that actually do work. So, do(solve) for example, only calls the existing solve/0 predicate. The commands at the prompt must end with a period for them to work.

The usage for each of the commands is as follows:

load. 	-> Prompts user to enter the name of the knowledge base file, and if found proceeds to load the rules found in the file
help. 	-> Prints a short help message on the prompt.
solve. 	-> Asks a list of questions to solve the top level goal of the program
list.		-> Lists all rules currently asserted by the program
quit.		-> Ends the interpreter loop

QUESTION 3

The main reasoning behind the implementation is that a question is a sentence which requires an answer.

In order to implement the dynamically loading of goals from the kb file, we added a new process rule to parse goals. The process creates a question, and uses the answer received to load the rule.

312-pess
process(['goal:'|GoalText]) :-
		question(Attrs,Answer,GoalText,[]),
		write(rule(top_goal(Answer), Attrs)),nl,
		assertz(rule(top_goal(Answer), Attrs)).
		
The default rule moved to load_rules(F).
	
312-pess-grammar
question/4 is defined in the grammar a sentence that has an answer.
question(Attrs,A) --> sentence(Attrs,A).

For this to work, an extra parameter was added to each rule deduced from sentence/4.

For especific question syntax, 4 cases were created.

Question starting with does/do
sentence(Attrs,A) -->
		does, sentence(Attrs,A).

Questions starting with is it.
sentence(Attrs,A,[is,it|Rest],[]) :- 
		sentence(Attrs,A,[it,is|Rest],[]).

Questions starting with has it
sentence(Attrs,A,[has,it|Rest],[]) :-
		sentence(Attrs,A,[it,has|Rest],[]).

Questions starting with what + does
sentence(Attrs,A,[what|Rest],[]) :-
		append(Rest,[what],NewTerm),
		sentence(Attrs,A,NewTerm,[]).

For the bonus,
% interpretation of 'what the heack is THAT'
sentence([attr(is_a,A,[])],A) -->
		[what],
		[the,heck],
		vis,
		['THAT'].
		
In order for the what to work without hardcoding all possible places it could be placed, new nouns, adverbs, adjectives, and vdoes were added. In this way it is a what small bird, is it a very what bird, and is it a very small what work.

n([attr(is_a,A,[])],A) --> [what], { n(A) }.
adv([attr(is_how,A,[])],A) --> [what], { adv(A) }.
adj([attr(is_like,A,[])],A) --> [what], { adj(A) }.
vdoes([attr(does,A,[])],A) --> [what], { v(A) }.

Question with no unbound variables receives a yes answer.
n([attr(is_a,X,[])],yes) --> [X], { n(X) }.

There are still some doubts on what to expect from the answers. In a goal as: what does it has? Will the program query for a bird and return all the noun adjectives related to that bird? Due to this problem and how hard it would be to implement this, we decided to just parse correctly the goal and create the correct rules, and leave the program to decide what to do with it.

QUESTION 4
The implementation was trivial. Just add a rule in 312-pess load_goal that will ask for an user input, read the sentence given and call process(['goal:'|Text]).

load_goal :-
	write("Enter the new goal followed by a period: "),
	read_sentence(GoalText),
	process(['goal:'|GoalText]),
	bug(GoalText).
	
QUESTION 6
The purpose of question 6 was to add words to our local dictionary from WordNet that we did not have dynamically. The use of pronto_morph's morph_atoms_bag predicate returned a list of all the stems of the given word, such as [harder,harde,-er,hard,-er]. We would then check every stem against our local db with stem_word(X).

The predicate stem_word(X) was created to take any noun and check through our local dictionary for its existence. If it did not exist in our local db the check_wordnet_dictionary(X) predicate would check WordNet and see if it is there. If it was it would be added to our local db and the word would be known.

There are some limits we put on the types of words that we add. Firstly the check_not_in_local_dictionary predicate checks to see if it is not only a noun, but a verb, an adjective, and an adverb. This was needed because the s/1 predicate from WordNet returns more than just one result, which could mean duplication. A word can have multiple meanings and can be both a verb and a noun. But, due to the slowness of checking every single word that was unknown and adding every single possible version of that word from WordNet, it was decided to only add the first one returned. This sped up the load_rules process tremendously.

The predicate was then added to where the attributes were created. Again, it was only added in the noun attribute by design as we only wanted to add nouns that we did not know, again this was due to the performance of the program. It can be extended to include adverbs, adjectives and verbs. We have tried it but it is just very slow. 

This still satisfies the scope of dynamically adding words to our dictionary.
Phrases such as "It is an insect", "It is an amusing hyena", "It is a fat penguin" are all understood by the program. To check you can try_parse/0 on any of the sentences above and then run perhaps the predicate check_not_in_local_dictionary(penguin, n) and it should return false, thats if your ran "It is a fat penguin first" or else that would would not be in our dictionary. The n atom stands for the type of the word in this case n is noun, a is adjective. 

For example:
?- check_not_in_local_dictionary(penguin, n).
false.

?- check_not_in_local_dictionary(amusing, a).
false.

This is what we expect.
There are comments in the code to guide you through each interaction of predicates just like the ones stated above.