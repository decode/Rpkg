#!/usr/bin/python
# -*- coding: utf-8 -*-

from models import *
from pymmseg import mmseg
import orange

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
      file = open('data.basket', 'a')  
      for tok in algor:
        str += tok.text.decode('utf-8')
        str += "|"
      seg.content = str
      session.commit()
      file.write(self.format(str).encode('utf-8') + "\n")

    file.close()

  def format(self, str):
    str = str[1:-1].replace("|", ", ")
    return str

  def analysis(self):
    data = orange.ExampleTable("data")
    rules = orange.AssociationRulesSparseInducer(data, support = 0.5)
    for r in rules:
      print "%5.3f   %5.3f   %s" % (r.support, r.confidence, r)


if __name__ == '__main__':
  apriori = Apriori()
  apriori.fetch()
  apriori.analysis()

