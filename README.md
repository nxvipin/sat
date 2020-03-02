# 🔐 sat

A basic work-in-progress [SAT](https://en.wikipedia.org/wiki/Boolean_satisfiability_problem) solver that implements the standard [DPLL algorithm]((https://en.wikipedia.org/wiki/DPLL_algorithm)) with a slightly modified unit propagation algorithm described in the paper by [Crawford & Auton](https://www.aaai.org/Papers/AAAI/1993/AAAI93-004.pdf). This is used as a tool to learn SAT solving techniques and I hope to upgrade this to incorporate most of the advances in this field. Currently this is too slow for any benchmarks and is unlikely to be useful for any practical purposes.

The solver expects problems to be already in the conjunctive normal form (CNF) and accepts DIMACS files as input and will print the result to STDOUT as described in the [DIMACS specification](http://www.satcompetition.org/2011/rules.pdf). Some test inputs are available in the `resources` directory - some of which are sourced from the [SATLIB benchmark problems](https://www.cs.ubc.ca/~hoos/SATLIB/benchm.html).



## 👩‍💻 Usage


    $ lein run resources/test.cnf


## 👮🏽‍♂️ License

Copyright © 2020 Vipin Nair

This program and the accompanying materials are made available under the
terms of the Eclipse Public License 2.0 which is available at
http://www.eclipse.org/legal/epl-2.0.

This Source Code may also be made available under the following Secondary
Licenses when the conditions for such availability set forth in the Eclipse
Public License, v. 2.0 are satisfied: GNU General Public License as published by
the Free Software Foundation, either version 2 of the License, or (at your
option) any later version, with the GNU Classpath Exception which is available
at https://www.gnu.org/software/classpath/license.html.
