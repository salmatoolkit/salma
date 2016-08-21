"""
Created on 02.08.2013

@author: kroiss
"""
OK = 1
NOT_OK = -1
NONDET = 0
AMBIGUOUS = 2
CANCEL = -2
SELF = 'self'

MSG_TYPE_MULTICAST_SRC = "multicastSrc"
MSG_TYPE_UNICAST = "unicast"
MSG_TYPE_SENSOR = "sensor"
MSG_TYPE_REMOTE_SENSOR_SRC = "remoteSensorSrc"

INVARIANT, ACHIEVE, ACHIEVE_AND_SUSTAIN = range(3)