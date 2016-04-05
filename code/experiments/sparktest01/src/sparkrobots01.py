import logging
from pyspark import *
import json
import subprocess as sp
import os


def run_experiment(trial_numer: int):
    # verdict, details = experiment.run()
    # pyclp.init()
    # myVar = pyclp.Var()  # Create a prolog variable
    # pyclp.Compound("lib", pyclp.Atom("random")).post_goal()
    # my_goal = pyclp.Compound("random", myVar)  # Create goal
    # my_goal.post_goal()  # Post goal
    # result, dummy = pyclp.resume()  # Resume execution
    # # of ECLiPSe engine
    # if result != pyclp.SUCCEED:
    #     raise Exception("Fehler in ECLiPSe")
    #
    # print(myVar.value())
    # print(trial_numer)
    # return trial_numer, myVar.value()
    p = sp.Popen(["python3.5", "src/robots01.py"], stdin=sp.DEVNULL, stdout=sp.PIPE, stderr=sp.PIPE)

    print("RESULT:\n\n\n")
    com = p.communicate()
    sout = com[0].decode('utf-8')
    if len(sout) > 0:
        o = json.loads(sout)
        return trial_numer, o
    else:
        return trial_numer, None


if __name__ == '__main__':
    print(os.environ.get("DYLD_LIBRARY_PATH"))

    logging.basicConfig()
    logger = logging.getLogger("salma")
    logger.setLevel(logging.DEBUG)
    logger.debug("Hey Robots!")

    trials = range(10)
    sc = SparkContext(appName="SparkRobots01")
    sc.setLogLevel("DEBUG")
    trialRDD = sc.parallelize(trials)
    results = trialRDD. \
        map(lambda x: run_experiment(x)). \
        collect()
    sc.stop() 
    print("\n\nRESULTS:\n")
    print(list(results))
