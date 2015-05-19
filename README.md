salma
=====

The main repository for the SALMA toolkit (http://www.salmatoolkit.org).


The main code of the SALMA toolkit is found in the directory `code/SALMA`.

Unit Tests, which also have to act as documentation for now, can be found at `code/SALMA/salma/test`. All tests have to be executed with the root folder SALMA set as working directory. In particular, the following unit test suites are a good start for reading and playing around:

* `basic_simulation_test.py`
* `property_evaluation_test_01.py`
* `property_evaluation_test_01.py`
* `smctest01.py`

Additionally, the file `code/SALMA/salma/test/experiment1.py` contains a simple standalone experiment that can by run directly using the Python interpreter, again with the with the root folder SALMA set as working directory.

The "Optimized Parking Lot Assignment" example described shortly in [1] can be found in `/code/experiments/e-mobility`. The experiment can be executed using the Python script `/code/experiments/e-mobility/src/run_scenario1.py` with the working directory set to `/code/experiments/e-mobility` and `PYTHONPATH` set to `../../SALMA` . In Windows, the batch file `/code/experiments/e-mobility/run_scenario1.bat` can be used.



Dependencies
------------
* ECLiPSe 6.1 or later (www.eclipseclp.org)
* Python 3.3 or later
* PyCLP 0.8 or later (http://pyclp.sourceforge.net/)
* NumPy
* Matplotlib
* Statsmodel (http://statsmodels.sourceforge.net/)
* NetworkX (https://networkx.github.io/)


References
----------

[1] C. Kroiß and T. Bureš: Logic-Based Modeling of Information Transfer in Cyber-Physical Multi-Agent Systems. Second International Workshop on Formal Methods for Self-Adaptive Systems (FMSAS 2014), 2014.

