#!/usr/bin/python2
import csv
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import sys, getopt


def main(argv):
	dataFile = ''
	try:
		args = getopt.getopt(argv)
	except getopt.GetoptError:
		print 'No filename was entered'
		sys.exit(2)

	pd.read_csv(argv[0])


if __name__ == '__main__': 
    main(sys.argv[1])