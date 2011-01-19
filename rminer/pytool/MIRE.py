#! /usr/bin/env python

"""
$Revision: 0.1 $
$Date: 2004/06/29 22:00:00 $
$Id: MIRE.py,v 0.1 2004/06/29 22:00:00 dcavar Exp $

(C) 2004 by Damir Cavar <dcavar@indiana.edu>, Indiana University

License:

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  Respect copyrights and mention the author of this tool in any
  subsequent or modified version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
  or download it from http://www.gnu.org/licenses/gpl.txt
"""


import sys, string, glob, os.path, math
from stattools import sortNgrams, puncTrim, RE, getWordList, getTokens, getBigrams, rF
import re

sys.path.append("../")
from models import *
from segment import *


def MI(bigram, bigramprob, tokens, tokencount):
  """Returns the mutual information for bigrams.
    MI = P(XY|X) log2 ( P(XY) / P(X) P(Y) )
    P(XY|X) = num of bigrams XY over num bigrams with X left
  """
  tokenlist = string.split(bigram)

  if tokens.has_key(tokenlist[0]):
    px = float(tokens[tokenlist[0]])/float(tokencount)
  else:
    px = 0.0
  if tokens.has_key(tokenlist[1]):
    py = float(tokens[tokenlist[1]])/float(tokencount)
  else:
    py = 0.0
  if py == 0.0 or px == 0.0:
    return 0.0
  return bigramprob * math.log(bigramprob/(px * py) , 2)

def P(token, tokens, tokencount):
  if tokens.has_key(token):
    p = float(tokens[token])/float(tokencount)
  else:
    p = 0.0
  return p

def test():
  bigrams     = {}  # bigram as key, frequency as value
  tokens      = {}  # token as key, frequency as value
  tokencount  = 0   # number of tokens
  bigramcount = 0   # number of bigrams
  alphabet    = ""  # all characters used

  for i in sys.argv[1:]:
    for x in glob.glob(os.path.normcase(i)):
      try:
        file = open(x, "r")
        for i in file.readlines():
          #i = string.lower(string.strip(i))
          i = i.strip().lower()
          if i == "":
            continue
          wordlist = getWordList(i)
          bigrams, bigramcount = getBigrams(wordlist, bigrams, bigramcount)
          tokens, tokencount = getTokens(wordlist, tokens, tokencount)
        file.close()
      except IOError:
        file.close()

  print("Got total:\nBigrams: " + str(bigramcount) + "\nTokens: " + str(tokencount))
  print("Bigram\tFrequency\tRelative Frequency\tMutual Information\tRelative Entropy")
  #myTokens = string.split(i[0])
  for i in sortNgrams(bigrams):
    tokenlist = list(i)[0].split()
    re = RE(rF(i[1], bigramcount), P(tokenlist[1], tokens, tokencount), P(tokenlist[0], tokens, tokencount))
    #print(i[0] + "\t" + str(i[1]) + "\t" + str(rF(i[1], bigramcount)) + "\t" + str(MI(i[0], rF(i[1], bigramcount), tokens, tokencount)) + "\t" + str(RE(i[0], rF(i[1], bigramcount), rF(myTokens[1], tokencount), rF(myTokens[0], tokencount))))
    print(i[0] + "\t" + str(i[1]) + "\t" + str(rF(i[1], bigramcount)) + "\t" + str(MI(i[0], rF(i[1], bigramcount), tokens, tokencount)) + "\t" + str(re))

def caculate(filename, freq=100):
  bigrams     = {}  # bigram as key, frequency as value
  tokens      = {}  # token as key, frequency as value
  tokencount  = 0   # number of tokens
  bigramcount = 0   # number of bigrams
  alphabet    = ""  # all characters used

  try:
    file = open(filename, "r")
    for i in file.readlines():
      i = i.strip().lower()
      if i == "":
        continue
      wordlist = getWordList(i)
      bigrams, bigramcount = getBigrams(wordlist, bigrams, bigramcount)
      tokens, tokencount = getTokens(wordlist, tokens, tokencount)
    file.close()
  except IOError:
    file.close()

  if os.path.exists("mi.txt"):
    os.remove('mi.txt')
  if os.path.exists("dict.txt"):
    os.remove('dict.txt')

  f = open("mi.txt", "w")
  fl = open("dict.txt", "w")

  print("Got total:\nBigrams: " + str(bigramcount) + "\nTokens: " + str(tokencount))
  #print("Bigram\tFrequency\tRelative Frequency\tMutual Information\tRelative Entropy")
  f.write("T1, T2, Frequency, Relative Frequency, Mutual Information, Relative Entropy\n")
  sep = ", "
  for i in sortNgrams(bigrams):
    tokenlist = list(i)[0].split()
    re = RE(rF(i[1], bigramcount), P(tokenlist[1], tokens, tokencount), P(tokenlist[0], tokens, tokencount))
    mi = MI(i[0], rF(i[1], bigramcount), tokens, tokencount)
    if mi > freq:
      f.write(tokenlist[1] + sep + tokenlist[0] + sep + str(i[1]) + sep + str(rF(i[1], bigramcount)) + sep + str(mi) + sep + str(re) + "\n")
      fl.write(tokenlist[1] + " " + tokenlist[0]+ "\n")
  f.close()
  fl.close()

  ret = merge("dict.txt", "data.basket")
  '''
  print(ret)
  if ret > 0:
    caculate(filename, freq)
  merge("dict.txt", "data.basket")
  '''

def merge(dict_file, dest_file):
  try:
    f = open(dict_file, "r")
    lines = f.readlines()
    for i in lines:
      i = i.strip().lower()
      if i == "":
        continue
      words = i.split()
      find_replace(words, dest_file)
    f.close()
    return len(lines)
  except IOError:
    f.close()
    return len(lines)
  
def find_replace(words, filename):
  data = open(filename).read()
  data = re.sub(words[0] + ', ' + words[1], words[0]+words[1], data)
  data = re.sub(words[1] + ', ' + words[0], words[1]+words[0], data)
  #print("merge: " + words[0] + ' ' + words[1])
  open(filename, 'wb').write(data)

if __name__ == "__main__":
  caculate(sys.argv[1], 0.02)
