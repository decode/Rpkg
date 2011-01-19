#!/usr/bin/python
# -*- coding: utf-8 -*-

from models import *
from pymmseg import mmseg
from segment import *
import orange
import os.path

class Apriori:

  def analysis(self):
    if os.path.exists("apri.txt"):
      os.remove('apri.txt')

    f = file("apri.txt", "a")
    sep = ", "
    f.write("T1, T2, support, confidence\n")
    data = orange.ExampleTable("data")
    rules = orange.AssociationRulesSparseInducer(data, support = 0.10, confidence=0.5, maxItemSets=15000)
    for r in rules:
      if r.nLeft == 1 and r.nRight == 1:
        #print "%5.3f   %5.3f   %s" % (r.support, r.confidence, r)
        left = str(r.left.getmetas(orange.Variable).values()[0].variable)[15:-1]
        right = str(r.right.getmetas(orange.Variable).values()[0].variable)[15:-1]

        f.write(left + sep + right + sep + str(r.support) + sep + str(r.confidence) + "\n")
    f.close()


if __name__ == '__main__':
  #segment = SegmentTool()
  #segment.fetch()
  apriori = Apriori()
  apriori.analysis()

