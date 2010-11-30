#!/usr/bin/python
# -*- coding: utf-8 -*-

from elixir import *
from datetime import datetime

metadata.bind = "mysql://root:123654@localhost/tbclawer?charset=utf8"
#metadata.bind.echo = True

class Account(Entity):
  id = Field(Integer, primary_key=True)

  user = ManyToOne('User')

  buyer_rate = Field(Integer)
  seller_rate = Field(Integer)
  sim_trade = Field(Unicode(30))
  good_trade = Field(Unicode(30))

  s1 = Field(Float)
  s2 = Field(Float)
  s3 = Field(Float)
  s1_count = Field(Integer)
  s2_count = Field(Integer)
  s3_count = Field(Integer)

  s_good = Field(Integer, default=0)
  s_normal = Field(Integer, default=0)
  s_bad = Field(Integer, default=0)
  b_good = Field(Integer, default=0)
  b_normal = Field(Integer, default=0)
  b_bad = Field(Integer, default=0)

  s_6month_good = Field(Integer, default=0)
  s_6month_normal = Field(Integer, default=0)
  s_6month_bad = Field(Integer, default=0)
  b_6month_good = Field(Integer, default=0)
  b_6month_normal = Field(Integer, default=0)
  b_6month_bad = Field(Integer, default=0)

  s_month6_good = Field(Integer, default=0)
  s_month6_normal = Field(Integer, default=0)
  s_month6_bad = Field(Integer, default=0)
  b_month6_good = Field(Integer, default=0)
  b_month6_normal = Field(Integer, default=0)
  b_month6_bad = Field(Integer, default=0)

  r1 = Field(Integer, default=0)
  r2 = Field(Integer, default=0)
  r3 = Field(Integer, default=0)
  r4 = Field(Integer, default=0)
  r5 = Field(Integer, default=0)
  r6 = Field(Integer, default=0)
  r7 = Field(Integer, default=0)
  r8 = Field(Integer, default=0)
  r9 = Field(Integer, default=0)
  r10 = Field(Integer, default=0)
  r11 = Field(Integer, default=0)
  r12 = Field(Integer, default=0)
  r13 = Field(Integer, default=0)
  r14 = Field(Integer, default=0)
  r15 = Field(Integer, default=0)
  r16 = Field(Integer, default=0)
  r17 = Field(Integer, default=0)
  r18 = Field(Integer, default=0)
  r19 = Field(Integer, default=0)
  r20 = Field(Integer, default=0)
  r21 = Field(Integer, default=0)
  r22 = Field(Integer, default=0)
  r23 = Field(Integer, default=0)
  r24 = Field(Integer, default=0)

  c1 = Field(Integer, default=0)
  c2 = Field(Integer, default=0)
  c3 = Field(Integer, default=0)

  favourate = Field(Integer, default=0)
  amount = Field(Integer, default=0)
  register_time = Field(DateTime)
  created_at = Field(DateTime, default=datetime.now)
  updated_at = Field(DateTime)

  using_options(tablename='accounts')
  
  def find_or_create(self, user_id):
    account = Account.query.filter_by(user_id=user_id).first()
    if(account == None):
      account = Account(user_id=user_id)
      #account.save()
      session.commit()
    return account

class TradeUser(Entity):
  using_options(tablename='trade_users')
  using_table_options(useexisting=True)

  seller = ManyToOne('User')
  buyer = ManyToOne('User')
  trade = ManyToOne('Trade')

class Trade(Entity):
  using_options(tablename='trades')
  using_table_options(useexisting=True)

  id = Field(Integer, primary_key=True)
  price = Field(Float)
  appraise = Field(Unicode(512))
  buyer_name = Field(Unicode(60))
  opinion = Field(Unicode(256))
  valid_trade = Field(Integer)
  trade_time = Field(DateTime)
  created_at = Field(DateTime)
  #seller_id = Field(Integer) #ManyToMany('User', tablename='trade_users')
  #buyer_id = Field(Integer) #ManyToMany('User', tablename='trade_users')
  #seller = ManyToMany('User')#, table='trade_users')
  #buyer = ManyToMany('User')#, table='trade_users')
  item = ManyToOne('Item')

  '''
  def find_or_create(self, seller_id, buyer_id, item_id, trade_time):
    trade = Trade.query.filter_by(seller_id=seller_id, buyer_id=buyer_id, item_id=item_id, trade_time=trade_time).first()
    if(trade == None):
      trade = Trade(seller_id=seller_id, buyer_id=buyer_id, item_id=item_id, trade_time=trade_time)
      trade.save()
    return trade 
  '''

class User(Entity):
  id = Field(Integer, primary_key=True)
  name = Field(Unicode(40), unique=True, required=True)
  shop_name = Field(Unicode(40))
  shop_url = Field(Unicode(300))
  place = Field(Unicode(20))
  rate_url = Field(Unicode(300))
  home_url = Field(Unicode(300))
  address = Field(Unicode(100))
  shop_type = Field(Unicode(20))
  status = Field(Unicode(20))
  open_at = Field(DateTime)
  created_at = Field(DateTime, default=datetime.now)

  taobao_user_id = Field(Unicode(10))

  using_options(tablename='users')
  
  def __repr__(self):
    return '<User "%s", "%s">' % (self.id, self.name)

  '''
  def find_or_create(self, user_name):
    user = User.query.filter_by(name=user_name).first()
    user.name = user_name
    return user 
  '''

class Item(Entity):
  id = Field(Integer, primary_key=True)
  name = Field(Unicode(128))
  url = Field(Unicode(300))
  description = Field(Unicode(512))
  item_type = Field(Unicode(30))
  month_count = Field(Integer)
  favorate_count = Field(Integer)
  view_count = Field(Integer)
  end_time = Field(DateTime)
  created_at = Field(DateTime, default=datetime.now)
  
  stock = Field(Integer) #存货量
  out_of_date = Field(Integer) #是否下架
  price = Field(Float)
  sold_count = Field(Integer) #己卖出
  
  seller = ManyToOne('User')
  using_options(tablename='items')

  '''
  def find_or_create(url, user_id, name):
    item = Item.query.filter_by(url=url, user_id=user_id, name=name).first()
    if(item == None):
      item = Item(name=name, url=url, user_id=user_id)
      item.save()
    return item
  '''

class Task(Entity):
  id = Field(Integer, primary_key=True)
  type = Field(Unicode(20))
  target_id = Field(Integer, required=True)
  url = Field(Unicode(300), required=True)
  update_count = Field(Integer, default=0)
  depth = Field(Integer, default=0)
  error_count = Field(Integer, default=0)
  error = Field(Unicode(40))
  created_at = Field(DateTime, default=datetime.now)
  updated_at = Field(DateTime)
  added_at = Field(DateTime)

  using_options(tablename='tasks')

class Segment(Entity):
  id = Field(Integer, primary_key=True)
  content = Field(Unicode(300))
  item_id = Field(Integer)
  using_options(tablename='segments')

setup_all(create_tables=True)
