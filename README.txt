QUESTION 1

My domain is about vehicles and there is one funny one in there about food, but the main subject are vehicles.

The first rule describes a race car, only if it had thick and wide tires and ran on the track with low ride height.

The second describes a bicycle, only if it has thin wheels and sits one person and has a tublar frame.

The third one describes a boat, only if it has external motors and floats on water and has sails.

The fourth and amusing one is a pizza, only if its shape is round and it feeds five people and smells awesome and has cheese toppings.

It was difficult and frustrating to write rules when you aren't sure about the facts of the domain yourself. You would rarely mentally ask yourself what makes this this, which can become troublesome, however what was simple was understanding the syntax of the rules because there was a file for reference. For words that were not know a "n:....." was used to explicitly state that that was a noun or a verb or an adverb or an adjective. The minimum amount of rules needed was 3. This satisfies the requirement for question 1.

QUESTION 6
The purpose of question 6 was to add words to our local dictionary from WordNet that we did not have dynamically. The use of pronto_morph's morph_atoms_bag returned a list of all the stems of the given word, such as [harder,harde,-er,hard,-er]. We would then check every stem against our local db with stem_word(X).

The predicate stem_word(X) was created to take any noun and check through our local dictionary for its existence. If it did not exist in our local db the check_wordnet_dictionary(X) predicate would check WordNet and see if it is there. If it was it would be added to our local db and the word would be known.

There are some limits we put into the types of word that we add. Firstly the check_not_in_local_dictionary predicate checks to see if it is not only a noun, but a verb, an adjective, and an adverb. This was needed because the s/1 predicate from WordNet returns more than just one result, which could mean duplication. A word can have multiple meanings and can be both a verb and a noun. But, due to the slowness of checking every single word that was unknown and adding every single possible version of that word from WordNet, it was decided to only add the first one returned. This sped up the load_rules process tremendously.

The predicate was then added to where the attributes were created. Again, it was only added in the noun attribute by design as we only wanted to add nouns that we did not know, again this was due to the performance of the program. It can be extended to include adverbs, adjectives and verbs. We have tried it but it is just very slow. 

This still satisfies the scope of dynamically adding words to our dictionary.
Phrases such as "It is an insect", "It is an amusing hyena", "It is a fat penguin" are all understood by the program. To check you can try_parse/0 on any of the sentences above and then run perhaps the predicate check_not_in_local_dictionary(penguin, n) and it should return false, thats if your ran "It is a fat penguin first" or else that would would not be in our dictionary. The n atom stands for the type of the word in this case n is noun, a is adjective. 

For example:
?- check_not_in_local_dictionary(penguin, n).
false.

?- check_not_in_local_dictionary(amusing, a).
false.

This is what we expect.