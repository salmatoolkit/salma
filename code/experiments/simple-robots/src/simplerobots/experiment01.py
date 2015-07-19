from salma.experiment import Experiment


class Experiment01(Experiment):

    def __init__(self):
        super().__init__("ecl-src/simple-robots-domaindesc.ecl")

    def create_initial_situation(self):
        pass

    def create_entities(self):
        pass

    def setup_distributions(self):
        super().setup_distributions()





