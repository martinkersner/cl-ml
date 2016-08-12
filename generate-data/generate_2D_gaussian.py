#!/usr/binb/env python

'''
Martin Kersner, m.kersner@gmail.com
2016/08/12

Generate 2D Gaussian data distribution and print it to standard output 
in csv format without header.
'''

import numpy as np

def main():
  n_samples = 1000
  mean = np.array([0.0, 0.0])
  cov  = np.array([[6.0, -4.0],
                   [-4.0, 6.0]])

  data = np.random.multivariate_normal(mean, cov, size=n_samples)

  for record in data:
    print ' '.join(map(str, record))

if __name__ == "__main__":
  main()
