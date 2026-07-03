Functional final project
Agatha Beffy, Justina Mateescu, Sacha Gregoire, Manon Vu Huu
___________________________________________________________________________________

DESCRIPTION

This project implements a natural language interface in Haskell that can understand, analyze, and evaluate a small sentence.
It is based on Lambek calculus, which treats sentence structures as a form of logical proof, and works as follows:

1. Grammatical checking: Each word is assigned a type and a sentence is well-formed if these types work together according to Lambek’s sequent-calculus rules.
2. Derivation generation: We derive all possible lambda expressions (syntactic derivations).
3. Semantic interpretation: We substitute every word's meaning into the derivation and normalize it.
4. Evaluation: We assess if statements are true or false and give answers based on a given environment (miniworld.py).

Our interface handles:
- declaratives (base): Awen likes Butor
- adjectival predicates (base): Awen is happy
- coordination (base): Awen is happy and Butor is confused
- quantifiers (base): Everyone likes someone
- interrogatives with answers: Who likes Awen --> Answer: Butor
- imperatives with answers: Tell me who likes Butor --> Answer: Awen
- reflexive pronouns: Céleste likes herself
- adverbs: Awen really likes Butor
- only construction: Only Butor likes Awen, Awen only likes Butor, Awen likes only Butor
- yes/no questions with answers: Does Awen like Butor --> Answer: Yes
- negation: Awen is not happy --> False 
- conditionals: Awen is happy if Butor is happy --> True
- modifying the environment (adding people, making them feel emotions, building relationships)

Not very important notes: 
- Sacha demanded we change Céleste to Celeste 
- Everyone goes by he/she (Awen likes himself = Awen likes herself)
- Sacha is proud that when you remove somebody, it also removes them from emotions and relationships

___________________________________________________________________________________

LIBRARIES

Beyond those included with GHC and the Haskell base system, we used:
- Text.Read
- Control.Monad
- Data.Char

___________________________________________________________________________________

HOW TO RUN

1. compile the program:         ghci -package process ChatLLaMbda.hs 
2. run the executable:          :main
3. specify the Lexicon file:    mini.elx
4. specify the world file:      miniworld.py
5. use our interface:           (any of the handled queries)
6. to exit:                     Ctrl + C

___________________________________________________________________________________

WORK REPARTITION

Here is how we divided the tasks for the project:
- Sacha:    grammatical checking, enable environment modifications
- Agatha:   derivation generation, semantic interpretation (go Agatha)
- Justina:  negation, conditionals, imperatives
- Manon:    interrogatives + answers, imperatives, reflexive pronouns, adverbs, only, yes/no, thank you, README (hi)

Please refer to our gitlab history for further details.

___________________________________________________________________________________

FUTURE IMPROVEMENT IDEAS

We could technically work on this until it works for the entire English language.
But for now here are the cool improvements we could still add (off the top of our heads):
- intensity of facts (difference between "Awen likes Butor a lot" and "Awen likes Butor a little")
- verb tenses ("Awen liked Butor", "Awen will like Butor") and how they affect the present relationships
- transitive relationships (if Awen likes Butor and Butor likes Céleste, then Awen likes Céleste) (this is not how English works but would be fun)

___________________________________________________________________________________

We had fun making this. Thank you for this semester.