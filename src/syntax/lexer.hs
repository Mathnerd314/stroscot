wbc c = unicodeCharacterWhere "Word_Break" c

cr = wbc "CR"
lf = wbc "LF"
newline = wbc "Newline"
extend = wbc "Extend"
zwj = wbc "ZWJ"
ri = wbc "Regional_Indicator"
format = wbc "Format"
katakana = wbc "Katakana"
hebrew_letter = wbc "Hebrew_Letter"
aletter = wbc "ALetter"
single_quote = wbc "Single_Quote"
double_quote = wbc "Double_Quote"
midnumlet = wbc "MidNumLet"
midletter = wbc "MidLetter"
midnum = wbc "MidNum"
numeric = wbc "Numeric"
extendnumlet = wbc "ExtendNumLet"
wsegspace = wbc "WSegSpace"

any = unicodeCharacter

ahletter = aletter <|> hebrew_letter
midnumletq = midnumlet <|> single_quote

-- Haskell syntax categories
special = oneOf "()[]{},;`"
pureSpecial = oneOf "()[]{}@=\|;"
LETTER = Unicode category Ll,Lt,Lu,Lo <|> '_'
SYMBOL = Unicode category Pc,Pd,Po,Sc,Sm,Sk,So
other_graphic = Unicode category Mc,Me,Nl,Ps,Pe,Pi,Pf
uniidchar = Lm, Mn
symbol -> (/[!#$%&*+./<=>?@\'^<|>-~:]/ <|> any Unicode symbol or punctuation) &&& not special <|> '_' <|> '"' <|> '\''

wordSegmentation =
  [ ("WB1", sot 	÷ 	any)
  , ("WB2", any 	÷ 	eot)
  , ("WB3", cr 	× 	lf)
  , ("WB3a", (newline <|> cr <|> lf) 	÷)
  , ("WB3b",   	÷ 	(newline <|> cr <|> lf))
  , ("WB3c", zwj 	× unicodeCharacterWhere "Extended_Pictographic" "Yes")
  , ("WB3d", wsegspace 	× 	wsegspace)
  , ("WB4", replace (x (extend <|> format <|> zwj)*) x)
  , ("WB5", ahletter 	× 	ahletter)
  , ("WB6", ahletter 	× 	(midletter <|> midnumletq) ahletter)
  , ("WB7", ahletter (midletter <|> midnumletq) 	× 	ahletter)
  , ("WB7a", hebrew_letter 	× 	single_quote)
  , ("WB7b", hebrew_letter 	× 	double_quote hebrew_letter)
  , ("WB7c", hebrew_letter double_quote 	× 	hebrew_letter)
  , ("WB8", numeric 	× 	numeric)
  , ("WB9", ahletter 	× 	numeric)
  , ("WB10", numeric 	× 	ahletter)
  , ("WB11", numeric (midnum <|> midnumletq) 	× 	numeric)
  , ("WB12", numeric 	× 	(midnum <|> midnumletq) numeric)
  , ("WB13", katakana 	× 	katakana)
  , ("WB13a", (ahletter <|> numeric <|> katakana <|> extendnumlet) 	× 	extendnumlet)
  , ("WB13b", extendnumlet 	× 	(ahletter <|> numeric <|> katakana))
  , ("WB15", sot (ri ri)* ri 	× 	ri)
  , ("WB16", [^ri] (ri ri)* ri 	× 	ri)
  , ("varid", LETTER  ×  (LETTER <|> DIGIT <|> SINGLE_QUOTE <|> uniidchar)
  , ("symbol",  SYMBOL  ×  SYMBOL)
  , ("WB999", any 	÷ 	any)
  ]
