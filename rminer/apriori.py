#!/usr/bin/python
# -*- coding: utf-8 -*-

from models import *
from pymmseg import mmseg
#import rpy2.robjects as robjects

class Apriori:
  def fetch(self):
    mmseg.dict_load_defaults()
    items = Item.query.from_statement("select id, name from items order by id desc limit 40")
    for i in items:
      seg = Segment.query.filter_by(id=i.id).first()
      if(seg==None):
        seg = Segment(item_id=i.id)
        #session.commit()
      text = i.name
      text = text.encode("utf-8")
      algor = mmseg.Algorithm(text)
      str = "|"
      for tok in algor:
        str += tok.text.decode('utf-8')
        str += "|"
      print(str)
      seg.content = str
      session.commit()

if __name__ == '__main__':
  apriori = Apriori()
  apriori.fetch()

