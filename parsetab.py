
# parsetab.py
# This file is automatically generated. Do not edit.
_tabversion = '3.2'

_lr_method = 'LALR'

_lr_signature = 'y<\x8a#K\x00\xa8\x97\x01\xf2\xa6\xb4\x97\xfc\x14\xa7'
    
_lr_action_items = {'$end':([1,2,3,4,5,6,8,9,10,11,13,14,15,16,17,18,19,20,21,22,23,24,25,28,29,30,31,32,33,34,35,36,37,],[-27,-39,-14,-15,-13,-1,-36,-16,-35,-25,0,-37,-26,-38,-30,-32,-28,-11,-9,-29,-12,-10,-33,-1,-31,-24,-7,-6,-3,-8,-4,-34,-5,]),'DIGIT':([1,3,4,5,6,9,11,15,16,17,18,19,20,21,22,23,24,25,27,28,29,31,32,33,34,35,36,37,],[-27,-14,-15,-13,17,-16,-25,-26,17,-30,-32,-28,-11,-9,-29,-12,-10,-33,33,17,-31,33,-6,-3,-8,-4,17,-5,]),'AT_SYMBOL':([1,3,4,5,6,9,11,15,16,17,18,19,20,21,22,23,24,25,28,29,31,32,33,34,35,36,37,],[-27,-14,-15,-13,23,-16,-25,-26,23,-30,-32,-28,-11,-9,-29,-12,-10,-33,23,-31,-7,-6,-3,-8,-4,23,-5,]),'GREATER':([0,1,3,4,5,6,9,11,14,15,16,17,18,19,20,21,22,23,24,25,28,29,31,32,33,34,35,36,37,],[3,-27,-14,-15,-13,3,-16,-25,28,-26,3,-30,-32,-28,-11,-9,-29,-12,-10,-33,3,-31,-7,-6,-3,-8,-4,3,-5,]),'PERIOD':([0,1,3,4,5,6,7,9,11,15,16,17,18,19,20,21,22,23,24,25,26,28,29,31,32,33,34,35,36,37,],[7,-27,-14,-15,-13,20,26,-16,-25,-26,20,-30,-32,-28,-11,-9,-29,-12,-10,-33,30,20,-31,-7,-6,-3,-8,-4,20,-5,]),'PLUS':([0,1,3,4,5,6,9,11,15,16,17,18,19,20,21,22,23,24,25,28,29,31,32,33,34,35,36,37,],[8,-27,-14,-15,-13,21,-16,-25,-26,21,-30,-32,-28,-11,-9,-29,-12,-10,-33,21,-31,-7,-6,-3,-8,-4,21,-5,]),'LETTER':([0,1,3,4,5,6,9,11,12,15,16,17,18,19,20,21,22,23,24,25,27,28,29,31,32,33,34,35,36,37,],[9,-27,-14,-15,-13,9,-16,-25,27,-26,9,-30,-32,-28,-11,-9,-29,-12,-10,-33,35,9,-31,35,-6,-3,-8,-4,9,-5,]),'ESCAPE':([0,1,3,4,5,6,9,11,15,16,17,18,19,20,21,22,23,24,25,28,29,31,32,33,34,35,36,37,],[12,-27,-14,-15,-13,12,-16,-25,-26,12,-30,-32,-28,-11,-9,-29,-12,-10,-33,12,-31,-7,-6,-3,-8,-4,12,-5,]),'SPECIAL_INITIAL':([0,1,3,4,5,6,9,11,15,16,17,18,19,20,21,22,23,24,25,28,29,31,32,33,34,35,36,37,],[5,-27,-14,-15,-13,5,-16,-25,-26,5,-30,-32,-28,-11,-9,-29,-12,-10,-33,5,-31,-7,-6,-3,-8,-4,5,-5,]),'MINUS':([0,1,3,4,5,6,9,11,15,16,17,18,19,20,21,22,23,24,25,28,29,31,32,33,34,35,36,37,],[14,-27,-14,-15,-13,24,-16,-25,-26,24,-30,-32,-28,-11,-9,-29,-12,-10,-33,24,-31,-7,-6,-3,-8,-4,24,-5,]),'UNICODE_CONSTITUENT':([0,1,3,4,5,6,9,11,15,16,17,18,19,20,21,22,23,24,25,28,29,31,32,33,34,35,36,37,],[4,-27,-14,-15,-13,4,-16,-25,-26,4,-30,-32,-28,-11,-9,-29,-12,-10,-33,4,-31,-7,-6,-3,-8,-4,4,-5,]),}

_lr_action = { }
for _k, _v in _lr_action_items.items():
   for _x,_y in zip(_v[0],_v[1]):
      if not _x in _lr_action:  _lr_action[_x] = { }
      _lr_action[_x][_k] = _y
del _lr_action_items

_lr_goto_items = {'inline_hex_escape':([0,6,16,28,36,],[1,1,1,1,1,]),'subsequent_star':([6,28,],[16,36,]),'peculiar_identifier':([0,],[2,]),'hex_digit':([27,31,],[32,37,]),'subsequent':([6,16,28,36,],[18,29,18,29,]),'initial':([0,6,16,28,36,],[6,19,19,19,19,]),'hex_digit_plus':([27,],[31,]),'hex_scalar_value':([27,],[34,]),'special_subsequent':([6,16,28,36,],[22,22,22,22,]),'ellipsis':([0,],[10,]),'constituent':([0,6,16,28,36,],[11,11,11,11,11,]),'identifier':([0,],[13,]),'empty':([6,28,],[25,25,]),'special_initial':([0,6,16,28,36,],[15,15,15,15,15,]),}

_lr_goto = { }
for _k, _v in _lr_goto_items.items():
   for _x,_y in zip(_v[0],_v[1]):
       if not _x in _lr_goto: _lr_goto[_x] = { }
       _lr_goto[_x][_k] = _y
del _lr_goto_items
_lr_productions = [
  ("S' -> identifier","S'",1,None,None,None),
  ('empty -> <empty>','empty',0,'p_empty','sample.py',179),
  ('character_tabulation -> ESCAPE LETTER','character_tabulation',2,'p_character_tabulation','sample.py',185),
  ('hex_digit -> DIGIT','hex_digit',1,'p_hex_digit','sample.py',238),
  ('hex_digit -> LETTER','hex_digit',1,'p_hex_digit','sample.py',239),
  ('hex_digit_plus -> hex_digit_plus hex_digit','hex_digit_plus',2,'p_hex_digit_plus','sample.py',243),
  ('hex_digit_plus -> hex_digit','hex_digit_plus',1,'p_hex_digit_plus','sample.py',244),
  ('hex_scalar_value -> hex_digit_plus','hex_scalar_value',1,'p_hex_scalar_value','sample.py',251),
  ('inline_hex_escape -> ESCAPE LETTER hex_scalar_value','inline_hex_escape',3,'p_inline_hex_escape','sample.py',255),
  ('special_subsequent -> PLUS','special_subsequent',1,'p_special_subsequent','sample.py',259),
  ('special_subsequent -> MINUS','special_subsequent',1,'p_special_subsequent','sample.py',260),
  ('special_subsequent -> PERIOD','special_subsequent',1,'p_special_subsequent','sample.py',261),
  ('special_subsequent -> AT_SYMBOL','special_subsequent',1,'p_special_subsequent','sample.py',262),
  ('special_initial -> SPECIAL_INITIAL','special_initial',1,'p_special_initial','sample.py',266),
  ('special_initial -> GREATER','special_initial',1,'p_special_initial','sample.py',267),
  ('constituent -> UNICODE_CONSTITUENT','constituent',1,'p_constituent','sample.py',272),
  ('constituent -> LETTER','constituent',1,'p_constituent','sample.py',273),
  ('character_types -> special_subsequent','character_types',1,'p_character_types','sample.py',277),
  ('character_types -> special_initial','character_types',1,'p_character_types','sample.py',278),
  ('character_types -> DIGIT','character_types',1,'p_character_types','sample.py',279),
  ('character_types -> LETTER','character_types',1,'p_character_types','sample.py',280),
  ('character_types -> CHARACTER_NAME','character_types',1,'p_character_types','sample.py',281),
  ('character -> inline_hex_escape','character',1,'p_character','sample.py',285),
  ('character -> ESCAPE character_types','character',2,'p_character','sample.py',286),
  ('ellipsis -> PERIOD PERIOD PERIOD','ellipsis',3,'p_ellipsis','sample.py',293),
  ('initial -> constituent','initial',1,'p_initial','sample.py',297),
  ('initial -> special_initial','initial',1,'p_initial','sample.py',298),
  ('initial -> inline_hex_escape','initial',1,'p_initial','sample.py',299),
  ('subsequent -> initial','subsequent',1,'p_subsequent','sample.py',304),
  ('subsequent -> special_subsequent','subsequent',1,'p_subsequent','sample.py',305),
  ('subsequent -> DIGIT','subsequent',1,'p_subsequent','sample.py',306),
  ('subsequent_star -> subsequent_star subsequent','subsequent_star',2,'p_subsequent_star','sample.py',310),
  ('subsequent_star -> subsequent','subsequent_star',1,'p_subsequent_star','sample.py',311),
  ('subsequent_star -> empty','subsequent_star',1,'p_subsequent_star','sample.py',312),
  ('peculiar_identifier -> MINUS GREATER subsequent_star','peculiar_identifier',3,'p_peculiar_identifier','sample.py',319),
  ('peculiar_identifier -> ellipsis','peculiar_identifier',1,'p_peculiar_identifier','sample.py',320),
  ('peculiar_identifier -> PLUS','peculiar_identifier',1,'p_peculiar_identifier','sample.py',321),
  ('peculiar_identifier -> MINUS','peculiar_identifier',1,'p_peculiar_identifier','sample.py',322),
  ('identifier -> initial subsequent_star','identifier',2,'p_identifier','sample.py',329),
  ('identifier -> peculiar_identifier','identifier',1,'p_identifier','sample.py',330),
]
