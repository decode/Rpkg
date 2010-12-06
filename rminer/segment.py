#!/usr/bin/python
# -*- coding: utf-8 -*-

from models import *
from pymmseg import mmseg
import orange

class SegmentTool:
  def fetch(self, quantity=1000, format_str=", "):
    mmseg.dict_load_defaults()
    items = Item.query.from_statement('select id, name from items order by id desc limit ' + str(quantity))

    f = open('data.basket', 'a')  

    banlist = ['、','（','）','★','【','】','！','：']

    for i in items:
      seg = Segment.query.filter_by(id=i.id).first()
      if(seg==None):
        seg = Segment(item_id=i.id)
        #session.commit()
      text = i.name
      text = text.encode("utf-8")
      algor = mmseg.Algorithm(text)

      sep = "|"
      for tok in algor:
        if tok.text in banlist:
          pass
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
  segment.fetch(1000)
