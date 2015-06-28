import unittest
from salma.model.distributions import *
class DistributionTest(unittest.TestCase):

    def test_composed_distrib(self):
        mu = CategoricalDistribution("float", [(0.0, 0.5), (10.0, 0.5)])
        std = UniformDistribution("float", (0.5, 5.0))
        cd = ComposedDistribution(NormalDistribution, "float", mu, std)
        sample = []
        for i in range(10):
            sample.append(cd.generateSample(None, None))
        print(sample)

    def test_geometric(self):
        d = GeometricDistribution(0.05)
        print(d.generateSample(None, None))