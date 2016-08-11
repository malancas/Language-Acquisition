About
-------
This a port of the program used for research found in my repository CUNY-CoLAG-Sims-2016-17_
.. _CUNY-CoLAG-Sims-2016-17: https://github.com/malancas/CUNY-CoLAG-Sims-2016-17

That project is written in Python 2.7, however I decided to port it to Clojure for my own interest as a means of learning the language, getting acquainted with functional programming, and exploring concurrency methods that the project could benefit from.

This repository's code, as well as the one referenced above, is used to experiment with first language acquisition. It simulates learners, or "eChildren", learning a language's grammar by processing any number of sentences. Each sentence is described by different grammar parameters, each combination being a valid representation of the langauge's grammar. Since simulations can sometimes run 100,000 learners, each processing thousands of sentences each, I found concurrency worth looking into.