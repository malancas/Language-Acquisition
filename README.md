##About
-------
###This a port of the program used for research found in the [CUNY CoLAG Sims 2016-17 repository](https://github.com/malancas/CUNY-CoLAG-Sims-2016-17)

That project is written in Python 2.7, however I decided to port it to Clojure for my own interest as a means of learning the language, getting acquainted with functional programming, and exploring concurrency methods that the project could benefit from.

This repository's code, as well as the one referenced above, is used to experiment with first language acquisition. It simulates learners, or "eChildren", learning a language's grammar by processing any number of sentences. Each sentence is described by different grammar parameters, each combination being a valid representation of the langauge's grammar. Since simulations can sometimes run 100,000 learners, each processing thousands of sentences each, I found concurrency worth looking into.

##Running the program
###With Leiningen
I used the Leiningen framework to manage and test this codebase. It can be run with Leiningen using the command:
`lein run <number of learners> <number of sentences to process> <language code>`

###Without Leiningen
Download the file research_clojure-0.1.0-SNAPSHOT-standalone.jar located in the target subdirectory
Run it with `java -jar target/uberjar/research_clojure-0.1.0-SNAPSHOT-standalone.jar`

###Language Code
The file EngFrJapGerm.txt contains 3522 sentences, corresponding to fake languages which represent (and mimic the grammatical structure of) either English, French, Japanese, or German. Each language has a code associated with it, allowing the user to specify which language the learners should be trained on.

French=584, English=611, German=2253, and Japanese=3856

###Output
Currently the program will write results to 'out.csv'. I will be changing the file naming format to more accurately reflect which language is being learned and when the simulation was run.