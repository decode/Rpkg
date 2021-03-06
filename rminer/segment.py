#!/usr/bin/python
# -*- coding: utf-8 -*-

import sys
from models import *
from pymmseg import mmseg
import orange

class SegmentTool:
  def fetch(self, quantity=5000, format_str=", "):
    mmseg.dict_load_defaults()
    items = Item.query.from_statement('select id, name from items order by id desc limit ' + str(quantity))

    f = open('data.basket', 'a')  
    o = open('original.txt', 'a')  

    banlist = ['、','（','）','★','【','】','！','：']

    for i in items:
      seg = Segment.query.filter_by(id=i.id).first()
      if(seg==None):
        seg = Segment(item_id=i.id)
        #session.commit()
      text = i.name
      text = text.encode("utf-8")
      o.write(i.name.encode('utf-8') + "\n")

      algor = mmseg.Algorithm(text)

      sep = "|"
      s = ""
      for tok in algor:
        if tok.text in banlist:
          continue
        sep += tok.text.decode('utf-8')
        sep += "|"
      seg.content = sep
      session.commit()
      f.write(self.format(sep).encode('utf-8') + "\n")

    f.close()

  def format(self, str, format_str=", "):
    str = str[1:-1].replace("|", format_str)
    return str

if __name__ == "__main__":
  segment = SegmentTool()
  segment.fetch(sys.argv[1])
