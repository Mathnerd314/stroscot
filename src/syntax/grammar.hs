optional pattern
many pattern -- zero or more repetitions
some pattern -- one or more repetitions
(pattern) -- grouping, no semantic meaning
pat_1 <|> pat_2 -- nondeterministic choice
not pat -- all sequences not matching PAT
pat1 &&& pat2 - elements matching pat1 and pat2
char "f" -- literal character
oneOf "abc" -- A character set. Matches any one of the characters.
(>*<) = liftA2 (,) -- parses left, then right, and returns both
{ a; b } = a >*< b

allowImp syn = out { implicitAllowed = True, constraintAllowed = False }

-- | Disallow implicit type declarations
disallowImp = scopedImp

-- | Implicits hare are scoped rather than top level
scopedImp syn = out { implicitAllowed = False, constraintAllowed = False }

-- | Allow scoped constraint arguments
allowConstr = out { constraintAllowed = True }


source =
  imports
  prog

imports =
  optional shebang
  whiteSpace
  moduleHeader
  many import_

moduleHeader =
  choice
    ExplicitHeader
      optional docComment
      keyword "module"
      identifier
      optional (lchar ';')
    Unqualified
      lchar '%'
      reserved "unqualified"
    Empty
      empty

import_ =
  keyword "import"
  optional (keyword "public")
  identifier
  optional
    keyword "as"
    identifier
  optional (lchar ';')

prog =
  whiteSpace
  many (decl)

decl =
  choice
    withdeclExtensions (syntaxRulesList $ syntax_rules i)
    implementation
    openInterface
    fixity
    syntaxRule
    fnDecl
    data_
    record
    runElabDecl
    using_
    params
    mutual
    namespace
    interface_
    dsl
    directive
    provider
    transform
    declaration

fnDecl =
  choice
    PTy
      checkDeclFixity $
        docstring
        fnOpts
        fnName
        lchar ':'
      typeExpr (allowImp)
      terminator
    postulate
    caf
    clause

clause =
  choice
    Unnamed -- unnamed with or function clause (inside a with)
      some (wExpr)
      rhs
      choice
        PClauseR
          whereBlock <|> terminator
        PWithR
          keyword "with"
          bracketed
          optProof
          openBlock
          some $ fnDecl
          closeBlock
    -- <==
      simpleExpr
      symbol "<=="
      fnName
      rhs
      whereBlock <|> terminator
    -- lhs application "with" clause or function clause
      lhs
      choice
        q
          rhs
          whereBlock <|> terminator
        q
          keyword "with"
          bracketed
          optProof
          openBlock
          some $ fnDecl
          closeBlock

lhs = lhsInfixApp <|> lhsPrefixApp

lhsInfixApp =
  argExpr
  symbolicOperator
  argExpr
  many wExpr

lhsPrefixApp =
  fnName
  many (implicitArg <|> constraintArg <|> argExpr) { inPattern = True }
  many wExpr

rhs =
  choice
    Bind
      lchar '='
      indentGt
      expr
    QBind
      symbol "?="
      symbol "{"
      fnName
      symbol "}"
      expr


terminator =
  choice
   lchar ';'
   lookAhead eof
   lookahead newline with <= indent
   lookAheadMatches (oneOf ")}")

fullExpr =
  expr
  eof

constraintExpr = expr { constraintAllowed = True }

expr = pi

pi =
  choice
    explicitPi
    { lchar '{'; implicitPi }
    unboundPi
    if constraintAllowed then constraintPi else fail

unboundPi =
  opExpr
  optional (bindsymbol; expr)
  -- This is used when we need to disambiguate from a constraint list
  when constraintAllowed $
    notFollowedBy $ reservedOp "=>"

opExpr = makeExprParser expr' operator_table

-- example:
-- _+_ binary op
-- +_ prefix op
-- _+ postfix op
-- [[_]] around op
opExpr = choice
  { expr';  binary_op; expr' }
  { lchar "("; partial_binary_op; expr'; lchar ")" }
  { lchar "("; expr'; partial_binary_op; lchar ")" }
  { prefix_op; expr' }
  { expr'; postfix_op }
  { lchar "("; expr'; lchar ")" }
  { around_op_left; expr'; around_op_right }
  { lchar "("; around_op_left; around_op_right; lchar ")" } -- partial around op
  expr'

expr' = externalExpr <|> internalExpr
externalExpr = withextensions (syntaxRulesList $ syntax_rules i)

internalExpr = choice
  unifyLog
  runElab
  disamb
  noImplicits
  recordType
  quoteGoal
  let_
  rewriteTerm
  doBlock
  lambda
  caseExpr
  whenRulesExpr
  withRulesExpr
  if_
  app

whenRulesExpr = { expr; keyword "when"; indentedBlock1 simple_rules }
withRulesExpr = { expr; keyword "with"; indentedBlock1 rules }

caseExpr =
  keyword "case"
  withexpr
  keyword "of"
  indentedBlock1 (caseOption)

caseOption =
  expr { inPattern = True, implicitAllowed = False  }
  symbol "=>"
  expr

app =
  simpleExpr
  many arg
  if withAppAllowed && not inPattern
    then many {reservedOp "|"; expr'}
    else empty

arg = label "function argument" $ choice
  implicitArg
  constraintArg
  simpleExpr

implicitArg = label "implicit function argument" $
  lchar '{'
  name
  option {lchar '='; expr}
  lchar '}'


constraintArg = label "constraint argument" $
  symbol "@{"
  expr
  symbol "}"

simplePattern = simpleExpr { inPattern = True }

simpleExpr = label "expression" $
  choice
    simpleExternalExpr
    bracketed {implicitAllowed = False}
    PMetavar
      lchar '?' >*<  name
    PResolveTC
      choice
        lchar '%' >*< reserved "implementation"
    proofExpr
    tacticsExpr
    reserved "Type*"
    reserved "AnyType"
    reserved "Type"
    reserved "UniqueType"
    reserved "NullType"
    keyword "impossible"
    constant
    symbol "'" >*< name
    PRef
      fnName
      if inPattern
        then optional (reservedOp "@" >*< withsimpleExpr)
        else empty
    idiom
    listExpr
    alt
    reservedOp "!" >*< withsimpleExpr
    quasiquote
    namequote
    unquote
    placeholder

placeholder = lchar '_'

bracketed = label "parenthesized expression" $
  lchar '('
  bracketed' { withAppAllowed = True }

bracketed' =
  choice
    lchar ')'
    dependentPair
    (
      opName <- operatorName
      guardNotPrefix opName
      expr
      lchar ')'
    )
    (
      simpleExpr
      operatorName
      lchar ')'
    )
    bracketedExpr
    (
      constraintExpr
      bracketedconstraintExpr
    )
  where

    guardNotPrefix opName =
      guard $ opName /= sUN "-"
      guard $ opName /= sUN "!"

      ops <- idris_infixes <$> get
      guard . not . (opName `elem`) . mapMaybe justPrefix $ ops



dependentPair pun prev openFC =
  if null prev then
      nametypePart <|> namePart
  else
    case pun of
      IsType -> nametypePart <|> namePart <|> exprPart True
      IsTerm -> exprPart False
      TypeOrTerm -> nametypePart <|> namePart <|> exprPart False
  where nametypePart =
          (ln, lnfc, colonFC) <-       $
            (ln, lnfc) <-  name
            colonFC <-  (lchar ':')
            return (ln, lnfc, colonFC)
          lty <- expr'
          starsFC <- reservedOp "**"
          dependentPair IsType ((PRef lnfc [] ln, Just (colonFC, lty), starsFC):prev) openFC
        namePart =       $
          (ln, lnfc) <-  name
          starsFC <- reservedOp "**"
          dependentPair pun ((PRef lnfc [] ln, Nothing, starsFC):prev) openFC
        exprPart isEnd =
          e <- expr
          sepFCE <-
            let stars = (Left <$>  (reservedOp "**"))
                ending = (Right <$>  (lchar ')'))
            in if isEnd then ending else stars <|> ending
          case sepFCE of
            Left starsFC -> dependentPair IsTerm ((e, Nothing, starsFC):prev) openFC
            Right closeFC ->
              return (mkPDPairs pun openFC closeFC (reverse prev) e)
        mkPDPairs pun openFC closeFC ((e, cfclty, starsFC):bnds) r =
              (PDPair openFC ([openFC] ++ maybe [] ((: []) . fst) cfclty ++ [starsFC, closeFC] ++ (=<<) (\(_,cfclty,sfc) -> maybe [] ((: []) . fst) cfclty ++ [sfc]) bnds)
                               pun e (maybe Placeholder snd cfclty) (mergePDPairs pun starsFC bnds r))
        mergePDPairs pun starsFC' [] r = r
        mergePDPairs pun starsFC' ((e, cfclty, starsFC):bnds) r =
           PDPair starsFC' [] pun e (maybe Placeholder snd cfclty) (mergePDPairs pun starsFC bnds r)

bracketedConstraintExpr = bracketExpr { constraintAllowed = True }

bracketedExpr openParenFC e = label "end of bracketed expression" $

             do lchar ')'; return e
        <|>  do exprs <- some (do comma <-  (lchar ',')
                                  r <- expr
                                  return (r, comma))
                closeParenFC <-  (lchar ')')
                let hilite = [openParenFC, closeParenFC] ++ map snd exprs
                return $ PPair openParenFC hilite TypeOrTerm e (mergePairs exprs)
        <|>  do starsFC <- reservedOp "**"
                dependentPair IsTerm [(e, Nothing, starsFC)] openParenFC
        <?>


alt = do symbol "(|"; alts <-   sepBy1 (expr' (syn { withAppAllowed = False })) (lchar ','); symbol "|)"
             return (PAlternative [] FirstSuccess alts)



hsimpleExpr =
  do lchar '.'
     e <- simpleExpr
     return $ PHidden e
  <|> simpleExpr
  <?> "expression"



unifyLog = do       $ lchar '%' >*< reserved "unifyLog"
                  PUnifyLog <$> simpleExpr
               <?> "unification log expression"



runElab = do       $ lchar '%' >*< reserved "runElab"
                 (tm, fc) <- withsimpleExpr
                 return $ PRunElab fc tm (syn_namespace)
              <?> "new-style tactics expression"



disamb = do keyword "with"
                ns <-   sepBy1 name (lchar ',')
                tm <- expr'
                return (PDisamb (map tons ns) tm)
               <?> "namespace disambiguation expression"
  where tons (NS n s) = txt (show n) : s
        tons n = [txt (show n)]


noImplicits = do       (lchar '%' >*< reserved "noImplicits")
                     tm <- simpleExpr
                     return (PNoImplicits tm)
                 <?> "no implicits expression"

quasiquote =        symbol "`("
                    e <- expr { syn_in_quasiquote = (syn_in_quasiquote) + 1 ,
                                    inPattern = False }
                    g <- optional $
                           do symbol ":"
                              expr { inPattern = False } -- don't allow antiquotes
                    symbol ")"
                    return $ PQuasiquote e g)
                  <?> "quasiquotation"


unquote =        guard (syn_in_quasiquote > 0)
                 symbol "~"
                 e <- simpleExpr { syn_in_quasiquote = syn_in_quasiquote - 1 }
                 return $ PUnquote e)
               <?> "unquotation"


namequote =          symbol "`{{"
                     (n, nfc) <-  fnName
                     symbol "}}"
                     return (PQuoteName n False nfc))
                  <|> (do symbol "`{"
                          (n, nfc) <-  fnName
                          symbol "}"
                          return (PQuoteName n True nfc)))
                <?> "quoted name"


data SetOrUpdate = FieldSet PTerm | FieldUpdate PTerm


recordType =
      do ((fgs, rec), fc) <-
            keyword "record"
            lchar '{'
            fgs <- fieldGetOrSet
            lchar '}'
            rec <- optional (do notEndApp; simpleExpr)
            return (fgs, rec)
         case fgs of
              Left fields ->
                case rec of
                   Nothing ->
                       return (PLam fc (sMN 0 "fldx") NoFC Placeholder
                                   (applyAll fc fields (PRef fc [] (sMN 0 "fldx"))))
                   Just v -> return (applyAll fc fields v)
              Right fields ->
                case rec of
                   Nothing ->
                       return (PLam fc (sMN 0 "fldx") NoFC Placeholder
                                 (getAll fc (reverse fields)
                                     (PRef fc [] (sMN 0 "fldx"))))
                   Just v -> return (getAll fc (reverse fields) v)

       <?> "record setting expression"

         fieldSet = do ns <- fieldGet
                       (do lchar '='
                           e <- expr
                           return (ns, FieldSet e))
                         <|> do symbol "$="
                                e <- expr
                                return (ns, FieldUpdate e)
                    <?> "field setter"


         fieldGet =   sepBy1 fnName (symbol "->")


         fieldGetOrSet =       (Left <$>   sepBy1 fieldSet (lchar ','))
                     <|> do f <- fieldGet
                            return (Right f)



typeExpr = do cs <- if implicitAllowed then constraintList else return []
                  sc <- constraintExpr
                  return (bindList (\r -> PPi (constraint { pcount = r })) cs sc)
               <?> "type signature"



lambda = label "lambda expression" $
  lchar '\\'
  let implicitAllowed = false
  choice
    TypedLambda
      tyOptDeclList
      lambdaTail
    UntypedLambda
      sepBy simplePattern (lchar ',')
      lambdaTail

lambdaTail = symbol "->" >*< expr

rewriteTerm = do keyword "rewrite"
                     (prf, fc) <- withexpr
                     giving <- optional (do symbol "==>"; expr')
                     using <- optional (do reserved "using"
                                           n <- name
                                           return n)
                     keyword "in";  sc <- expr
                     return (PRewrite fc using prf sc giving)
                  <?> "term rewrite expression"



rigCount =   option RigW $ do lchar '1'; return Rig1
                       <|> do lchar '0'; return Rig0



let_ =       (do keyword "let"
                     ls <- indentedBlock (let_binding)
                     keyword "in";  sc <- expr
                     return (buildLets ls sc))
           <?> "let binding"
  where buildLets [] sc = sc
        buildLets ((fc, rc, PRef nfc _ n, ty, v, []) : ls) sc
          = PLet fc rc n nfc ty v (buildLets ls sc)
        buildLets ((fc, _, pat, ty, v, alts) : ls) sc
          = PCase fc v ((pat, buildLets ls sc) : alts)

let_binding = do rc <- rigCount
                     (pat, fc) <- withexpr' (syn { inPattern = True })
                     ty <-   option Placeholder (do lchar ':'; expr')
                     lchar '='
                     v <- expr (syn { withAppAllowed = isVar pat })
                     ts <-   option [] (do lchar '|'
                                             sepBy1 (do_alt) (lchar '|'))
                     return (fc,rc,pat,ty,v,ts)
   where isVar (PRef _ _ _) = True
         isVar _ = False



if_ = (do keyword "if"
              (c, fc) <- withexpr
              keyword "then"
              t <- expr
              keyword "else"
              f <- expr
              return (PIfThenElse fc c t f))
          <?> "conditional expression"




quoteGoal = do keyword "quoteGoal"; n <- name;
                   keyword "by"
                   r <- expr
                   keyword "in"
                   (sc, fc) <- withexpr
                   return (PGoal fc r n sc)
                <?> "quote goal expression"

bindsymbol
     = do symbol "->"
          return (Exp opts st False RigW)

explicitPi
   = do xt <-       (lchar '(' >*< typeDeclList <* lchar ')')
        binder <- bindsymbol
        sc <- constraintExpr
        return (bindList (\r -> PPi (binder { pcount = r })) xt sc)

autoImplicit
   = do keyword "auto"
        when (st == Static) $ fail "auto implicits can not be static"
        xt <- typeDeclList
        lchar '}'
        symbol "->"
        sc <- constraintExpr
        return (bindList (\r -> PPi
          (TacImp [] Dynamic (PTactics [ProofSearch True True 100 Nothing [] []]) r)) xt sc)

defaultImplicit =
   keyword "default"
   when (st == Static) $ fail "default implicits can not be static"
   ist <- get
   script' <- simpleExpr
   let script = debindApp . desugar ist $ script'
   xt <- typeDeclList
   lchar '}'
   symbol "->"
   sc <- constraintExpr
   return (bindList (\r -> PPi (TacImp [] Dynamic script r)) xt sc)

normalImplicit =
   xt <- typeDeclList <* lchar '}'
   symbol "->"
   cs <- constraintList
   sc <- expr
   let (im,cl)
          = if implicitAllowed
               then (Imp opts st False (Just (Impl False True False)) True RigW,
                      constraint)
               else (Imp opts st False (Just (Impl False False False)) True RigW,
                     Imp opts st False (Just (Impl True False False)) True RigW)
   return (bindList (\r -> PPi (im { pcount = r })) xt
           (bindList (\r -> PPi (cl { pcount = r })) cs sc))

constraintPi =
   do cs <- constraintList1
      sc <- expr
      if implicitAllowed
         then return (bindList (\r -> PPi constraint { pcount = r }) cs sc)
         else return (bindList (\r -> PPi (Imp opts st False (Just (Impl True False False)) True r))
                               cs sc)

implicitPi =
  choice
    autoImplicit
    defaultImplicit
    normalImplicit

piOpts | implicitAllowed = optional (lchar '.')
piOpts = empty

constraintList = optional constraintList1

constraintList1 =
  choice
    A
      lchar '('
      sepBy1 nexpr (lchar ',')
      lchar ')'
      reservedOp "=>"
    B
      opExpr
      reservedOp "=>"
  where
    nexpr =
      choice
        { name; lchar ':'; expr}
        expr

typeDeclList =       (  sepBy1 (do rig <- rigCount
                                       (x, xfc) <-  fnName
                                       lchar ':'
                                       t <- typeExpr (disallowImp)
                                       return (rig, x, xfc, t))
                             (lchar ','))
                   <|> do ns <-   sepBy1 ( name) (lchar ',')
                          lchar ':'
                          t <- typeExpr (disallowImp)
                          return (map (\(x, xfc) -> (RigW, x, xfc, t)) ns)
                   <?> "type declaration list"



tyOptDeclList =   sepBy1 (do (x, fc) <-  nameOrPlaceholder
                                 t <-   option Placeholder (do lchar ':'
                                                               expr)
                                 return (RigW, x, fc, t))
                             (lchar ',')
                    <?> "type declaration list"

           nameOrPlaceholder = fnName
                           <|> sMN 0 "underscore" <$ reservedOp "_"
                           <?> "name or placeholder"



listExpr = do (FC f (l, c) _) <-  (lchar '[')
                  (do (FC _ _ (l', c')) <-  (lchar ']') <?> "end of list expression"
                      return (mkNil (FC f (l, c) (l', c'))))
                   <|> (do (x, fc) <-  (expr (syn { withAppAllowed = False })) <?> "expression"
                           (do       (lchar '|') <?> "list comprehension"
                               qs <-   sepBy1 (do_) (lchar ',')
                               lchar ']'
                               return (PDoBlock (map addGuard qs ++
                                          [DoExp fc (PApp fc (PRef fc [] (sUN "pure"))
                                                       [pexp x])]))) <|>
                            (do xs <- many (do commaFC <-  (lchar ',') <?> "list element"
                                               elt <- expr
                                               return (elt, commaFC))
                                rbrackFC <-  (lchar ']') <?> "end of list expression"
                                return (mkList fc rbrackFC ((x, (FC f (l, c) (l, c+1))) : xs))))
                <?> "list expression"


doBlock
    = do keyword "do"
         PDoBlock <$> indentedBlock1 (do_)
      <?> "do block"



do_
     =       (do keyword "let"
                 (i, ifc) <-  name
                 ty' <-   optional (do lchar ':'
                                       expr')
                 reservedOp "="
                 (e, fc) <- withexpr (syn { withAppAllowed = False })
                 -- If there is an explicit type, this can’t be a pattern-matching let, so do not parse alternatives
                   option (DoLet fc RigW i ifc (fromMaybe Placeholder ty') e)
                          (do lchar '|'
                              when (isJust ty') $ fail "a pattern-matching let may not have an explicit type annotation"
                              ts <-   sepBy1 (do_alt (syn { withAppAllowed = False })) (lchar '|')
                              return (DoLetP fc (PRef ifc [ifc] i) e ts)))
   <|>       (do keyword "let"
                 i <- expr'
                 reservedOp "="
                 (e, fc) <- withexpr (syn { withAppAllowed = False })
                   option (DoLetP fc i e [])
                          (do lchar '|'
                              ts <-   sepBy1 (do_alt (syn { withAppAllowed = False })) (lchar '|')
                              return (DoLetP fc i e ts)))
   <|>       (do (sc, fc) <-  (keyword "rewrite" >*< expr)
                 return (DoRewrite fc sc))
   <|>       (do (i, ifc) <-  name
                 symbol "<-"
                 (e, fc) <- withexpr (syn { withAppAllowed = False });
                   option (DoBind fc i ifc e)
                          (do lchar '|'
                              ts <-   sepBy1 (do_alt (syn { withAppAllowed = False })) (lchar '|')
                              return (DoBindP fc (PRef ifc [ifc] i) e ts)))
   <|>       (do i <- expr'
                 symbol "<-"
                 (e, fc) <- withexpr (syn { withAppAllowed = False });
                   option (DoBindP fc i e [])
                          (do lchar '|'
                              ts <-   sepBy1 (do_alt (syn { withAppAllowed = False })) (lchar '|')
                              return (DoBindP fc i e ts)))
   <|> do (e, fc) <- withexpr
          return (DoExp fc e)
   <?> "do block expression"

do_alt = do l <- expr'
                  option (Placeholder, l)
                         (do symbol "=>"
                             r <- expr'
                             return (l, r))



idiom
    = do symbol "[|"
         (e, fc) <- withexpr (syn { withAppAllowed = False })
         symbol "|]"
         return (PIdiom fc e)
      <?> "expression in idiom brackets"




constants =
  [ ("Integer",            AType (ATInt ITBig))
  , ("Int",                AType (ATInt ITNative))
  , ("Char",               AType (ATInt ITChar))
  , ("Double",             AType ATFloat)
  , ("String",             StrType)
  , ("prim__WorldType",    WorldType)
  , ("prim__TheWorld",     TheWorld)
  , ("Bits8",              AType (ATInt (ITFixed IT8)))
  , ("Bits16",             AType (ATInt (ITFixed IT16)))
  , ("Bits32",             AType (ATInt (ITFixed IT32)))
  , ("Bits64",             AType (ATInt (ITFixed IT64)))
  ]



constant =   choice [ ty <$ reserved name | (name, ty) <- constants ]
       <|>       (Fl <$> float)
       <|> BI <$> natural
       <|> Str <$> verbatimStringLiteral
       <|> Str <$> stringLiteral
       <|>       (Ch <$> charLiteral) --Currently ambigous with symbols
       <?> "constant or literal"



verbatimStringLiteral = token $ do       $ string "\"\"\""
                                   str <-   manyTill   anySingle $       (string "\"\"\"")
                                   moreQuotes <-   many $   char '"'
                                   return $ str ++ moreQuotes



static =     Static <$ reserved "%static"
         <|> return Dynamic
         <?> "static modifier"


tactics =
  [ (["intro"], Nothing, const $ -- FIXME syntax for intro (fresh name)
      do ns <-   sepBy (spaced name) (lchar ','); return $ Intro ns)
  , noArgs ["intros"] Intros
  , noArgs ["unfocus"] Unfocus
  , (["refine"], Just ExprTArg, const $
       do n <- spaced fnName
          imps <- many imp
          return $ Refine n imps)
  , (["claim"], Nothing, \syn ->
       do n <- indentGt >*< name
          goal <- indentGt >*< expr
          return $ Claim n goal)
  , (["mrefine"], Just ExprTArg, const $
       do n <- spaced fnName
          return $ MatchRefine n)
  , expressionTactic ["rewrite"] Rewrite
  , expressionTactic ["equiv"] Equiv
  , (["let"], Nothing, \syn -> -- FIXME syntax for let
       do n <- (indentGt >*< name)
          (do indentGt >*< lchar ':'
              ty <- indentGt >*< expr'
              indentGt >*< lchar '='
              t <- indentGt >*< expr
              i <- get
              return $ LetTacTy n (desugar i ty) (desugar i t))
            <|> (do indentGt >*< lchar '='
                    t <- indentGt >*< expr
                    i <- get
                    return $ LetTac n (desugar i t)))

  , (["focus"], Just ExprTArg, const $
       do n <- spaced name
          return $ Focus n)
  , expressionTactic ["exact"] Exact
  , expressionTactic ["applyTactic"] ApplyTactic
  , expressionTactic ["byReflection"] ByReflection
  , expressionTactic ["reflect"] Reflect
  , expressionTactic ["fill"] Fill
  , (["try"], Just AltsTArg, \syn ->
        do t <- spaced (tactic)
           lchar '|'
           t1 <- spaced (tactic)
           return $ Try t t1)
  , noArgs ["compute"] Compute
  , noArgs ["trivial"] Trivial
  , noArgs ["unify"] DoUnify
  , (["search"], Nothing, const $
      do depth <-   option 10 natural
         return (ProofSearch True True (fromInteger depth) Nothing [] []))
  , noArgs ["implementation"] TCImplementation
  , noArgs ["solve"] Solve
  , noArgs ["attack"] Attack
  , noArgs ["state", ":state"] ProofState
  , noArgs ["term", ":term"] ProofTerm
  , noArgs ["undo", ":undo"] Undo
  , noArgs ["qed", ":qed"] Qed
  , noArgs ["abandon", ":q"] Abandon
  , noArgs ["skip"] Skip
  , noArgs ["sourceLocation"] SourceFC
  , expressionTactic [":e", ":eval"] TEval
  , expressionTactic [":t", ":type"] TCheck
  , expressionTactic [":search"] TSearch
  , (["fail"], Just StringLitTArg, const $
       do msg <- stringLiteral
          return $ TFail [Idris.Core.TT.TextPart msg])
  , ([":doc"], Just ExprTArg, const $
       do whiteSpace
          doc <- (Right <$> constant) <|> (Left <$> fnName)
            eof
          return (TDocStr doc))
  ]
  where
  expressionTactic names tactic = (names, Just ExprTArg, \syn ->
     do t <- spaced (expr)
        i <- get
        return $ tactic (desugar i t))
  noArgs names tactic = (names, Nothing, const (return tactic))
  spaced parser = indentGt >*< parser

  imp = do lchar '?'; return False
    <|> do lchar '_'; return True



tactic =   choice [ do   choice (map reserved names); parser
                      | (names, _, parser) <- tactics ]
          <|> do lchar '{'
                 t <- tactic;
                 lchar ';';
                 ts <-   sepBy1 (tactic) (lchar ';')
                 lchar '}'
                 return $ TSeq t (mergeSeq ts)
          <|> ((lchar ':' >> empty) <?> "prover command")
          <?> "tactic"


fullTactic = do t <- tactic
                      eof
                    return t



simpleWhiteSpace = satisfy isSpace

someSpace = some (simpleWhiteSpace <|> singleLineComment <|> multiLineComment)

singleLineComment = string "--" >*< many (satisfy (not . isEol)) >*< eol
multiLineComment = string "{-" >*< inCommentChars
inCommentChars =
  startEnd = "{}-"
  choice
    string "-}"
    multiLineComment >*< inCommentChars
    string "|||" >*< many (satisfy (not . isEol)) >*< eol >*< inCommentChars
    some (noneOf startEnd) >*< inCommentChars
    oneOf startEnd >*< inCommentChars

whiteSpace = optional someSpace
token p = p >*< whiteSpace

keyword = reserved
reserved name = token $
  string name
  notFollowedBy (satisfy isAlphaNum <|> oneOf "_'.")


parserWarning fc warnOpt warnErr =
  ist <- get
  let cmdline = opt_cmdline (idris_options ist)
  unless (maybe False (`elem` cmdline) warnOpt) $
    put ist { parserWarnings = (fc, warnErr) : parserWarnings ist }








isEol '\n' = True
isEol  _   = False



eol = () <$   satisfy isEol <|>   lookAhead   eof <?> "end of line"





docComment = do dc <-  >*< docCommentLine
                rest <- many (indented docCommentLine)
                args <- many $ do (name, first) <- indented argDocCommentLine
                                  rest <- many (indented docCommentLine)
                                  return (name, concat (intersperse "\n" (first:rest)))

                return (parseDocstring $ T.pack (concat (intersperse "\n" (dc:rest))),
                        map (\(n, d) -> (n, parseDocstring (T.pack d))) args)


        docCommentLine =   hidden $       $
                           string "|||"
                           many (  satisfy (==' '))
                           contents <-   option "" (do first <-   satisfy (\c -> not (isEol c || c == '@'))
                                                       res <- many (  satisfy (not . isEol))
                                                       return $ first:res)
                           eol ; someSpace
                           return contents


        argDocCommentLine = do   string "|||"
                                 many (  satisfy isSpace)
                                 char '@'
                                 many (  satisfy isSpace)
                               n <- name
                                 many (  satisfy isSpace)
                               docs <-   many (  satisfy (not . isEol))
                                 eol ; someSpace
                               return (n, docs)



stringLiteral = token .       $   char '"' >*<   manyTill   charLiteral (  char '"')
charLiteral = token .       $   char '\'' >*<   charLiteral <*   char '\''



natural = token (          (  char '0' >*<   char' 'x' >*<   hexadecimal)
                 <|>       (  char '0' >*<   char' 'o' >*<   octal)
                 <|>         decimal)



float = token .       $   float




reservedIdentifiers = HS.fromList
  [ "Type"
  , "case", "class", "codata", "constructor", "corecord", "data"
  , "do", "dsl", "else", "export", "if", "implementation", "implicit"
  , "import", "impossible", "in", "infix", "infixl", "infixr", "instance"
  , "interface", "let", "mutual", "namespace", "of", "parameters", "partial"
  , "postulate", "private", "proof", "public", "quoteGoal", "record"
  , "rewrite", "syntax", "then", "total", "using", "where", "with"
  ]


identifierOrReservedWithExtraChars extraChars = token $       $
  c <-   satisfy isAlpha <|>   oneOf "_"
  cs <-   many (  satisfy isAlphaNum <|>   oneOf extraChars)
  return $ c : cs


char =   char


string =   string



lchar = token .   char


symbol = void . token .   string



identifierWithExtraChars extraChars =       $
  ident <- identifierOrReservedWithExtraChars extraChars
  when (ident `HS.member` reservedIdentifiers) $   unexpected .   Label . NonEmpty.fromList $ "reserved " ++ ident
  when (ident == "_") $   unexpected .   Label . NonEmpty.fromList $ "wildcard"
  return ident


identifier = identifierWithExtraChars "_'."



iName bad = maybeWithNS identifier bad <?> "name"



maybeWithNS parser bad =
  i <-   option "" (  lookAhead identifier)
  when (i `elem` bad) $   unexpected .   Label . NonEmpty.fromList $ "reserved identifier"
  mkName <$>   choice (reverse (parserNoNS parser : parsersNS parser i))

        parserNoNS = fmap (\x -> (x, ""))

        parserNS   parser ns = do xs <-  (string ns)
                                  lchar '.'
                                  x <- parser
                                  return (x, xs)

        parsersNS parser i = [      (parserNS parser ns) | ns <- (initsEndAt (=='.') i)]



name =
    keywords <- syntax_keywords <$> get
    aliases  <- module_aliases  <$> get
    n <- iName keywords
    return (unalias aliases n)
   <?> "name"
  where

    unalias aliases (NS n ns) | Just ns' <- M.lookup ns aliases = NS n ns'
    unalias aliases name = name



initsEndAt p [] = []
initsEndAt p (x:xs) | p x = [] : x_inits_xs
                    | otherwise = x_inits_xs
  where x_inits_xs = [x : cs | cs <- initsEndAt p xs]




mkName (n, "") = sUN n
mkName (n, ns) = sNS (sUN n) (reverse (parseNS ns))
  where parseNS x = case span (/= '.') x of
                      (x, "")    -> [x]
                      (x, '.':y) -> x : parseNS y



packageName = (:) <$>   oneOf firstChars <>*< many (  oneOf remChars)
  where firstChars = ['a'..'z'] ++ ['A'..'Z']
        remChars = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ['-','_']



commaSeparated p = p `  sepBy1` (  space >>   char ',' >>   space)



lastIndent = do ist <- get
                case indent_stack ist of
                  (x : xs) -> return x
                  _        -> return 1



indented p = notEndBlock >*< p <* keepTerminator



indentedBlock p = do openBlock

                     res <- many (indented p)

                     closeBlock
                     return res



indentedBlock1 p = do openBlock

                      res <- some (indented p)

                      closeBlock
                      return res



indentedBlockS p = do openBlock

                      res <- indented p

                      closeBlock
                      return res




lookAheadMatches p = isJust <$>   lookAhead (  optional p)



openBlock =     do lchar '{'
                   ist <- get
                   put (ist { brace_stack = Nothing : brace_stack ist })
            <|> do ist <- get
                   lvl' <-
                    -- if we're not indented further, it's an empty block, so
                    -- increment lvl to ensure we get to the end
                   let lvl = case brace_stack ist of
                                   Just lvl_old : _ ->
                                       if lvl' <= lvl_old then lvl_old+1
                                                          else lvl'
                                   [] -> if lvl' == 1 then 2 else lvl'
                                   _ -> lvl'
                   put (ist { brace_stack = Just lvl : brace_stack ist })
            <?> "start of block"



closeBlock = do ist <- get
                bs <- case brace_stack ist of
                        []  -> [] <$   eof
                        Nothing : xs -> lchar '}' >> return xs <?> "end of block"
                        Just lvl : xs -> (do i   <-
                                             isParen <- lookAheadMatches (char ')')
                                             isIn <- lookAheadMatches (reserved "in")
                                             if i >= lvl && not (isParen || isIn)
                                                then fail "not end of block"
                                                else return xs)
                                          <|> (do notOpenBraces
                                                    eof
                                                  return [])
                put (ist { brace_stack = bs })





keepTerminator =  () <$ lchar ';'
              <|> do c <- ; l <- lastIndent
                     unless (c <= l) $ fail "not a terminator"
              <|> do isParen <- lookAheadMatches (  oneOf ")}|")
                     isIn <- lookAheadMatches (reserved "in")
                     unless (isIn || isParen) $ fail "not a terminator"
              <|>   lookAhead   eof



notEndApp = do c <- ; l <- lastIndent
               when (c <= l) (fail "terminator")



notEndBlock = do ist <- get
                 case brace_stack ist of
                      Just lvl : xs -> do i <-
                                          isParen <- lookAheadMatches (  char ')')
                                          when (i < lvl || isParen) (fail "end of block")
                      _ -> return ()


indentGt =
  li <- lastIndent
  i <-
  when (i <= li) $ fail "Wrong indention: should be greater than context indentation"



notOpenBraces = do ist <- get
                   when (hasNothing $ brace_stack ist) $ fail "end of input"

        hasNothing = any isNothing



accessibility' = Public  <$ reserved "public" <* reserved "export"
             <|> Frozen  <$ reserved "export"
             <|> Private <$ reserved "private"
             <?> "accessibility modifier"


accessibility = do acc <- optional accessibility'
                   case acc of
                        Just a -> return a
                        Nothing -> do ist <- get
                                      return (default_access ist)



addAcc n a = do i <- get
                put (i { hide_list = addDef n a (hide_list i) })



accData Frozen n ns = do addAcc n Public -- so that it can be used in public definitions
                         mapM_ (\n -> addAcc n Private) ns -- so that they are invisible
accData a n ns = do addAcc n a
                    mapM_ (`addAcc` a) ns





fixErrorMsg msg fixes = msg ++ ", possible fixes:\n" ++ (concat $ intersperse "\n\nor\n\n" fixes)



table fixes
   = [[prefix "-" negateExpr]] ++
      toTable (reverse fixes) ++
     [[noFixityBacktickOperator],
      [binary "$"   InfixR $ \fc _ x y -> flatten $ PApp fc x [pexp y]],
      [binary "="   InfixL $ \fc _ x y -> PApp fc (PRef fc [fc] eqTy) [pexp x, pexp y]],
      [noFixityOperator]]
  where


    negateExpr _  (PConstant fc (I int))     = PConstant fc $ I $ negate int
    negateExpr _  (PConstant fc (BI bigInt)) = PConstant fc $ BI $ negate bigInt
    negateExpr _  (PConstant fc (Fl dbl))    = PConstant fc $ Fl $ negate dbl
    negateExpr _  (PConstSugar fc term)      = negateExpr fc term
    negateExpr fc (PAlternative ns tp terms) = PAlternative ns tp $ map (negateExpr fc) terms
    negateExpr fc x                          = PApp fc (PRef fc [fc] (sUN "negate")) [pexp x]


    flatten (PApp fc (PApp _ f as) bs) = flatten (PApp fc f (as ++ bs))
    flatten t                          = t


    noFixityBacktickOperator =   InfixN $
                                 (n, fc) <-  backtickOperator
                                 return $ \x y -> PApp fc (PRef fc [fc] n) [pexp x, pexp y]



    noFixityOperator =   InfixN $
                         indentGt
                         op <-       symbolicOperator
                           unexpected .   Label . fromList $ "Operator without known fixity: " ++ op



    toTable fs = map (map toBin) (groupBy (\ (Fix x _) (Fix y _) -> prec x == prec y) fs)

    toBin (Fix (PrefixN _) op) = prefix op $ \fc x ->
                                   PApp fc (PRef fc [] (sUN op)) [pexp x]
    toBin (Fix f op)           = binary op (assoc f) $ \fc n x y ->
                                   PApp fc (PRef fc [] n) [pexp x,pexp y]

    assoc (Infixl _) =   InfixL
    assoc (Infixr _) =   InfixR
    assoc (InfixN _) =   InfixN


    isBacktick (c : _) = c == '_' || isAlpha c
    isBacktick _       = False


    binary name ctor f
      | isBacktick name = ctor $       $
                            (n, fc) <-  backtickOperator
                            guard $ show (nsroot n) == name
                            return $ f fc n
      | otherwise       = ctor $
                            indentGt
                            fc <- reservedOp name
                            indentGt
                            return $ f fc (sUN name)


    prefix name f =   Prefix $
                      fc <- reservedOp name
                      indentGt
                      return (f fc)



backtickOperator =   between (indentGt >*< lchar '`') (indentGt >*< lchar '`') name



operatorName =     sUN <$> symbolicOperator
               <|> backtickOperator



operatorFront = do           $ lchar '(' >*< (eqTy <$ reservedOp "=") <* lchar ')'
                   <|> maybeWithNS (lchar '(' >*< symbolicOperator <* lchar ')') []



fnName =       operatorFront <|> name <?> "function name"



fixity = do ((f, i, ops), fc) <-

                f <- fixityType; i <- natural
                ops <-   sepBy1 (show . nsroot <$> operatorName) (lchar ',')
                terminator
                return (f, i, ops)
            let prec = fromInteger i
            istate <- get
            let infixes = idris_infixes istate
            let fs      = map (Fix (f prec)) ops
            let redecls = map (alreadyDeclared infixes) fs
            let ill     = filter (not . checkValidity) redecls
            if null ill
               then do put (istate { idris_infixes = nub $ sort (fs ++ infixes)
                                     , ibc_write     = map IBCFix fs ++ ibc_write istate
                                   })
                       return (PFix fc (f prec) ops)
               else fail $ concatMap (\(f, (x:xs)) -> "Illegal redeclaration of fixity:\n\t\""
                                                ++ show f ++ "\" overrides \"" ++ show x ++ "\"") ill
         <?> "fixity declaration"
  where

    alreadyDeclared fs f = (f, filter ((extractName f ==) . extractName) fs)


    checkValidity (f, fs) = all (== f) fs


    extractName (Fix _ n) = n



checkDeclFixity p = do decl <- p
                       case getDeclName decl of
                         Nothing -> return decl
                         Just n -> do checkNameFixity n
                                      return decl
  where getDeclName (PTy _ _ _ _ _ n _ _ )                = Just n
        getDeclName (PData _ _ _ _ _ (PDatadecl n _ _ _)) = Just n
        getDeclName _ = Nothing



checkNameFixity n = do fOk <- fixityOk n
                       unless fOk . fail $
                         "Missing fixity declaration for " ++ show n
      where fixityOk (NS n' _) = fixityOk n'
            fixityOk (UN n') | all (flip elem opChars) (str n') =
                                 do fixities <- fmap idris_infixes get
                                    return . elem (str n') . map (\ (Fix _ op) -> op) $ fixities
                             | otherwise = return True
            fixityOk _ = return True



fixityType = do reserved "infixl"; return Infixl
         <|> do reserved "infixr"; return Infixr
         <|> do reserved "infix";  return InfixN
         <|> do reserved "prefix"; return PrefixN
         <?> "fixity type"


opChars = ":!#$%&*+./<=>?@\\^|-~"


operatorLetter =   oneOf opChars


commentMarkers = [ "--", "|||" ]


invalidOperators = [":", "=>", "->", "<-", "=", "?=", "|", "**", "==>", "\\", "%", "~", "?", "!", "@"]


symbolicOperator = do op <- token . some $ operatorLetter
                      when (op `elem` (invalidOperators ++ commentMarkers)) $
                           fail $ op ++ " is not a valid operator"
                      return op

reservedOp name = token $
  do string name
       notFollowedBy operatorLetter <?> ("end of " ++ show name)

Haskell

whitechar -> newline <|> '\v' <|> space <|> tab (should include all Unicode whitespace)
program -> { lexeme <|> whitespace }
lexeme -> qvarid <|> qvarsym <|> literal <|> special <|> reservedop <|> reservedid
literal -> integer <|> float <|> char <|> string

whitespace -> whitestuff {whitestuff}
whitestuff -> whitechar <|> comment <|> ncomment


escaped-newline → escape-sequence inline-spaces opt line-break

comment -> dashes [ (any &&& not symbol) {any} ] newline
dashes -> '--' {'-'}
opencom -> '{-'
closecom -> '-}'
ncomment -> opencom ANYseq {ncomment ANYseq} closecom
ANYseq -> {ANY &&& not (opencom <|> closecom)}

varsym -> ( symbol {symbol} ) &&& not (reservedop <|> dashes)

reservedop -> '..' <|> ':' <|> '::' <|> '=' <|> '\' <|> '<|>' <|> '<-' <|> '->' <|> '@' <|> '~' <|> '=>'

modid -> {varid '.'} varid -- (modules)
qvarid -> [ modid '.' ] varid
qvarsym -> [ modid '.' ] varsym

decimal -> {'0' <|> '1' <|> ... <|> '9' <|> any Unicode decimal digit}+
octal -> {'0' <|> '1' <|> ... <|> '7'}+
hexadecimal -> {any Unicode decimal digit <|> 'A' <|> ... <|> 'F' <|> 'a' <|> ... <|> 'f'}+

integer -> decimal <|> '0o' octal <|> '0O' octal <|> '0x' hexadecimal <|> '0X' hexadecimal
float -> decimal '.' decimal [exponent] <|> decimal exponent
exponent -> ('e' <|> 'E') ['+' <|> '-'] decimal

large -> Unicode category Lt <|> Unicode category Lu
graphic -> small <|> large <|> symbol <|> digit <|> special <|> '"' <|> '\''
ANY -> graphic <|> whitechar
any -> graphic <|> space <|> tab
char -> '\'' (graphic &&& not '\'' <|> '\'} <|> space <|> (escape &&& not '\&'})) '\''
string -> '"' { (graphic &&& not '"' <|> '\' ) <|> space <|> escape <|> gap} '"'
escape -> '\' ( charesc <|> ascii <|> decimal <|> 'o' octal <|> 'x' hexadecimal )
charesc -> 'a' <|> 'b' <|> 'f' <|> 'n' <|> 'r' <|> 't' <|> 'v' <|> '\' <|> '"' <|> '\'' <|> '&'
ascii -> '^'cntrl <|> 'NUL' <|> 'SOH' <|> 'STX' <|> 'ETX' <|> 'EOT' <|> 'ENQ' <|> 'ACK' <|> 'BEL' <|> 'BS' <|> 'HT' <|> 'LF' <|> 'VT' <|> 'FF' <|> 'CR' <|> 'SO' <|> 'SI' <|> 'DLE' <|> 'DC1' <|> 'DC2' <|> 'DC3' <|> 'DC4' <|> 'NAK' <|> 'SYN' <|> 'ETB' <|> 'CAN' <|> 'EM' <|> 'SUB' <|> 'ESC' <|> 'FS' <|> 'GS' <|> 'RS' <|> 'US' <|> 'SP' <|> 'DEL'
cntrl -> uppercase unicode char <|> '' <|> '[' <|> '\' <|> ']' <|> '^' <|> '_'
gap -> '\' whitechar {whitechar} '\'


module -> 'module' modid [exports] 'where' body <|> body
body -> '{' impdecls ';' topdecls '}' <|> '{' impdecls '}' <|> '{' topdecls '}'

impdecls -> impdecl_1 ';' ... ';' impdecl_n -- (n>=1)

exports -> '(' export_1 ',' ... ',' export_n [ ',' ] ')' -- (n>=0)

export -> qvar <|> qvarid ['(..)' <|> '(' cname_1 ',' ... ',' cname_n ')'] -- (n>=0) <|> qvarid ['(..)' <|> '(' qvar_1 ',' ... ',' qvar_n ')'] -- (n>=0) <|> 'module' modid

impdecl -> 'import' ['qualified'] modid ['as' modid] [impspec] <|> -- (empty declaration)

impspec -> '(' import_1 ',' ... ',' import_n [ ',' ] ')' -- (n>=0) <|> 'hiding' '(' import_1 ',' ... ',' import_n [ ',' ] ')' -- (n>=0)

import -> var <|> varid [ '(..)' <|> '(' cname_1 ',' ... ',' cname_n ')'] -- (n>=0) <|> varid ['(..)' <|> '(' var_1 ',' ... ',' var_n ')'] -- (n>=0)
cname -> var <|> con

topdecls -> topdecl_1 ';' ... ';' topdecl_n -- (n>=0)
topdecl -> 'type' simpletype '=' type <|> 'data' [context '=>'] simpletype ['=' constrs] [deriving] <|> 'newtype' [context '=>'] simpletype '=' newconstr [deriving] <|> 'class' [scontext '=>'] varid varid ['where' cdecls] <|> 'instance' [scontext '=>'] qvarid inst ['where' idecls] <|> 'default' '('type_1 ',' ... ',' type_n')' -- (n>=0) <|> 'foreign' fdecl <|> decl

decls -> '{' decl_1 ';' ... ';' decl_n '}' -- (n>=0)
decl -> gendecl <|> (funlhs <|> pat) rhs

cdecls -> '{' cdecl_1 ';' ... ';' cdecl_n '}' -- (n>=0)
cdecl -> gendecl <|> (funlhs <|> var) rhs

idecls -> '{' idecl_1 ';' ... ';' idecl_n '}' -- (n>=0)
idecl -> (funlhs <|> var) rhs <|> -- (empty)

gendecl -> vars '::' [context '=>'] type -- (type signature) <|> fixity [integer] ops -- (fixity declaration) <|> -- (empty declaration)

ops -> op_1 ',' ... ',' op_n -- (n>=1)
vars -> var_1 ',' ...',' var_n -- (n>=1)
fixity -> 'infixl' <|> 'infixr' <|> 'infix'

type -> btype ['->' type] -- (function type)

btype -> [btype] atype -- (type application)

atype -> gtycon <|> varid <|> '(' type_1 ',' ... ',' type_k ')' -- (tuple type, k>=2) <|> '[' type ']' -- (list type) <|> '(' type ')' -- (parenthesized constructor)

gtycon -> qvarid <|> '()' -- (unit type) <|> '[]' -- (list constructor) <|> '(->)' -- (function constructor) <|> '(,'{','}')' -- (tupling constructors)

context -> class <|> '(' class_1 ',' ... ',' class_n ')' -- (n>=0)
class -> qvarid varid <|> qvarid '(' varid atype_1 ... atype_n ')' -- (n>=1)
scontext -> simpleclass <|> '(' simpleclass_1 ',' ... ',' simpleclass_n ')' -- (n>=0)
simpleclass -> qvarid varid

simpletype -> varid tyvar_1 ... tyvar_k -- (k>=0)
constrs -> constr_1 '<|>' ... '<|>' constr_n -- (n>=1)
constr -> con ['!'] atype_1 ... ['!'] atype_k -- (arity con = k, k>=0) <|> (btype <|> '!' atype) conop (btype <|> '!' atype) -- (infix conop) <|> con '{' fielddecl_1 ',' ... ',' fielddecl_n '}' -- (n>=0)
newconstr -> con atype <|> con '{' var '::' type '}'
fielddecl -> vars '::' (type <|> '!' atype)
deriving -> 'deriving' (dclass <|> '('dclass_1',' ... ',' dclass_n')')-- (n>=0)
dclass -> qvarid

inst -> gtycon <|> '(' gtycon tyvar_1 ... tyvar_k ')' -- (k>=0, tyvars distinct) <|> '(' tyvar_1 ',' ... ',' tyvar_k ')' -- (k>=2, tyvars distinct) <|> '[' varid ']' <|> '(' tyvar_1 '->' tyvar_2 ')' -- tyvar_1 and tyvar_2 distinct

fdecl -> 'import' callconv [safety] impent var '::' ftype -- (define variable) <|> 'export' callconv expent var '::' ftype -- (expose variable)
callconv -> 'ccall' <|> 'stdcall' <|> 'cplusplus' -- (calling convention) <|> 'jvm' <|> 'dotnet' <|> system-specific calling conventions
impent -> [string] -- ccall
expent -> [string] -- ccall
safety -> 'unsafe' <|> 'safe'

ftype -> frtype <|> fatype \rightarrow ftype
frtype -> fatype <|> '()'
fatype -> qvarid atype_1 \ldots atype_k -- (k \geq 0)

funlhs -> var apat { apat } <|> pat varop pat <|> '(' funlhs ')' apat { apat }

rhs -> '=' exp ['where' decls] <|> gdrhs ['where' decls]

gdrhs -> guards '=' exp [gdrhs]

guards -> '<|>' guard_1, ..., guard_n -- (n>=1)
guard -> pat '<-' infixexp -- (pattern guard) <|> 'let' decls -- (local declaration) <|> infixexp -- (boolean guard)

exp -> infixexp '::' [context '=>'] type -- (expression type signature) <|> infixexp

infixexp -> lexp qop infixexp -- (infix operator application) <|> '-' infixexp -- (prefix negation) <|> lexp

lexp -> '\' apat_1 ... apat_n '->' exp -- (lambda abstraction, n>=1) <|> 'let' decls 'in' exp -- (let expression) <|> 'if' exp [';'] 'then' exp [';'] 'else' exp -- (conditional) <|> 'case' exp 'of' '{' alts '}' -- (case expression) <|> 'do' '{' stmts '}' -- (do expression) <|> fexp
fexp -> [fexp] aexp -- (function application)

aexp -> qvar -- (variable) <|> gcon -- (general constructor) <|> literal <|> '(' exp ')' -- (parenthesized expression) <|> '(' exp_1 ',' ... ',' exp_k ')' -- (tuple, k>=2) <|> '[' exp_1 ',' ... ',' exp_k ']' -- (list, k>=1) <|> '[' exp_1 [',' exp_2] '..' [exp_3] ']' -- (arithmetic sequence) <|> '[' exp '<|>' qual_1 ',' ... ',' qual_n ']' -- (list comprehension, n>=1) <|> '(' infixexp qop ')' -- (left section) <|> '(' (qop &&& not '-') infixexp} ')' -- (right section) <|> qcon '{' fbind_1 ',' ... ',' fbind_n '}' -- (labeled construction, n>=0) <|> (aexp &&& not qcon) '{' fbind_1 ',' ... ',' fbind_n '}' -- (labeled update, n >= 1)

qual -> pat '<-' exp -- (generator) <|> 'let' decls -- (local declaration) <|> exp -- (guard)

alts -> alt_1 ';' ... ';' alt_n -- (n>=1)
alt -> pat '->' exp ['where' decls] <|> pat gdpat ['where' decls] <|> -- (empty alternative)

gdpat -> guards '->' exp [ gdpat ]

stmts -> stmt_1 ... stmt_n exp [';'] -- (n>=0)
stmt -> exp ';' <|> pat '<-' exp ';' <|> 'let' decls ';' <|> ';' -- (empty statement)

fbind -> qvar '=' exp

pat -> lpat qconop pat -- (infix constructor) <|> lpat

lpat -> apat <|> '-' (integer <|> float) -- (negative literal) <|> gcon apat_1 ... apat_k -- (arity gcon = k, k>=1)

apat -> var [@ apat] -- (as pattern) <|> gcon -- (arity gcon = 0) <|> qcon '{' fpat_1 ',' ... ',' fpat_k '}' -- (labeled pattern, k>=0) <|> literal <|> '_' -- (wildcard) <|> '(' pat ')' -- (parenthesized pattern) <|> '(' pat_1 ',' ... ',' pat_k ')' -- (tuple pattern, k>=2) <|> '[' pat_1 ',' ... ',' pat_k ']' -- (list pattern, k>=1) <|> '~' apat -- (irrefutable pattern)

fpat -> qvar '=' pat

gcon -> '()' <|> '[]' <|> '(,'{','}')' <|> qcon

var -> varid <|> '(' varsym ')' -- (variable)
qvar -> qvarid <|> '(' qvarsym ')' -- (qualified variable)
con -> varid <|> '(' varsym ')' -- (constructor)
qcon -> qvarid <|> '(' gconsym ')' -- (qualified constructor)
varop -> varsym <|> '`' varid '`' -- (variable operator)
qvarop -> qvarsym <|> '`' qvarid '`' -- (qualified variable operator)
conop -> varsym <|> '`' varid '`' -- (constructor operator)
qconop -> gconsym <|> '`' qvarid '`' -- (qualified constructor operator)
op -> varop <|> conop -- (operator)
qop -> qvarop <|> qconop -- (qualified operator)
gconsym -> ':' <|> qvarsym


decl -> '{-#' 'INLINE' qvars '#-}'
decl -> '{-#' 'NOINLINE' qvars '#-}'

decl -> '{-#' 'SPECIALIZE' spec_1 ',' ... ',' spec_k '#-}' -- (k>=1)
spec -> vars :: type

modid -> {varid '.'} varid -- (modules)

module -> 'module' modid [exports] 'where' body <|> body
body -> '{' impdecls ';' topdecls '}' <|> '{' impdecls '}' <|> '{' topdecls '}'

impdecls -> impdecl_1 ';' ... ';' impdecl_n -- (n>=1)
topdecls -> topdecl_1 ';' ... ';' topdecl_n -- (n>=1)

exports -> '(' export_1 ',' ... ',' export_n [ ',' ] ')' -- (n>=0)

export -> qvar <|> qvarid ['(..)' <|> '(' cname_1 ',' ... ',' cname_n ')'] -- (n>=0) <|> qvarid ['(..)' <|> '(' var_1 ',' ... ',' var_n ')'] -- (n>=0) <|> 'module' modid

cname -> var <|> con

impdecl -> 'import' ['qualified'] modid ['as' modid] [impspec] <|> -- (empty declaration)
impspec -> '(' import_1 ',' ... ',' import_n [ ',' ] ')' -- (n>=0) <|> 'hiding' '(' import_1 ',' ... ',' import_n [ ',' ] ')' -- (n>=0)

import -> var <|> varid [ '(..)' <|> '(' cname_1 ',' ... ',' cname_n ')'] -- (n>=0) <|> varid ['(..)' <|> '(' var_1 ',' ... ',' var_n ')'] -- (n>=0)

% var
% <|> varid
% <|> varid '(..)'
% <|> varid '(' con_1 ',' ... ',' con_n')' -- (n>=1)
% <|> varid '(..)'
% <|> varid '(' var_1 ',' ... ',' var_n')' -- (n>=0)

cname -> var <|> con


exp -> infixexp '::' [context '=>'] type -- (expression type signature) <|> infixexp

infixexp -> lexp qop infixexp -- (infix operator application) <|> '-' infixexp -- (prefix negation) <|> lexp

lexp -> '\' apat_1 ... apat_n '->' exp -- (lambda abstraction, n>=1) <|> 'let' decls 'in' exp -- (let expression) <|> 'if' exp [';'] 'then' exp [';'] 'else' exp -- (conditional) <|> 'case' exp 'of' '{' alts '}' -- (case expression) <|> 'do' '{' stmts '}' -- (do expression) <|> fexp
fexp -> [fexp] aexp -- (function application)


aexp -> qvar -- (variable) <|> gcon -- (general constructor) <|> literal


gcon -> '()' <|> '[]' <|> '(,'{','}')' <|> qcon

var -> varid <|> '(' varsym ')' -- (variable)
qvar -> qvarid <|> '(' qvarsym ')' -- (qualified variable)
con -> varid <|> '(' varsym ')' -- (constructor)
qcon -> qvarid <|> '(' gconsym ')' -- (qualified constructor)
varop -> varsym <|> '`' varid '`' -- (variable operator)
qvarop -> qvarsym <|> '`' qvarid '`' -- (qualified variable operator)
conop -> varsym <|> '`' varid '`' -- (constructor operator)
qconop -> gconsym <|> '`' qvarid '`' -- (qualified constructor operator)
op -> varop <|> conop -- (operator)
qop -> qvarop <|> qconop -- (qualified operator)
gconsym -> ':' <|> qvarsym


fexp -> [fexp] aexp -- (function application)
lexp -> '\' apat_1 ... apat_n '->' exp -- (lambda abstraction, n>=1)


infixexp -> lexp qop infixexp <|> '-' infixexp -- (prefix negation) <|> lexp
qop -> qvarop <|> qconop -- (qualified operator)




lexp -> 'if' exp [';'] 'then' exp [';'] 'else' exp


infixexp -> exp_1 qop exp_2
aexp -> '[' exp_1 ',' ... ',' exp_k ']' -- (k>=1) <|> gcon
gcon -> '[]' <|> qcon
qcon -> '(' gconsym ')'
qop -> qconop
qconop -> gconsym
gconsym -> ':'


aexp -> '(' exp_1 ',' ... ',' exp_k ')' -- (k>=2) <|> qcon
qcon -> '(,'{','}')'


aexp -> gcon <|> '(' exp ')'
gcon -> '()'


aexp -> '[' exp_1 [',' exp_2] '..' [exp_3] ']'


aexp -> '[' exp '<|>' qual_1 ',' ... ',' qual_n ']' -- (list comprehension, n>=1)
qual -> pat '<-' exp -- (generator) <|> 'let' decls -- (local declaration) <|> exp -- (boolean guard)


lexp -> 'let' decls 'in' exp


lexp -> 'case' exp 'of' '{' alts '}'
alts -> alt_1 ';' ... ';' alt_n -- (n>=1)
alt -> pat '->' exp ['where' decls] <|> pat gdpat ['where' decls] <|> -- (empty alternative)

gdpat -> guards '->' exp [ gdpat ]
guards -> '<|>' guard_1, ..., guard_n -- (n>=1)
guard -> pat '<-' infixexp -- (pattern guard) <|> 'let' decls -- (local declaration) <|> infixexp -- (boolean guard)


lexp -> 'do' '{' stmts '}' -- (do expression)
stmts -> stmt_1 ... stmt_n exp [';'] -- (n>=0)
stmt -> exp ';' <|> pat '<-' exp ';' <|> 'let' decls ';' <|> ';' -- (empty statement)


aexp -> qvar


aexp -> qcon '{' fbind_1 ',' ... ',' fbind_n '}' -- (labeled construction, n>=0)
fbind -> qvar '=' exp

aexp -> (aexp &&& not qcon) '{' fbind_1 ',' ... ',' fbind_n '}' -- (labeled update, n>=1)
exp -> exp '::' [context '=>'] type

pat -> lpat qconop pat -- (infix constructor) <|> lpat

lpat -> apat <|> '-' (integer <|> float) -- (negative literal) <|> gcon apat_1 ... apat_k -- (arity gcon = k, k>=1)

apat -> var ['@' apat] -- (as pattern) <|> gcon -- (arity gcon = 0) <|> qcon '{' fpat_1 ',' ... ',' fpat_k '}' -- (labeled pattern, k>=0) <|> literal <|> '_' -- (wildcard) <|> '(' pat ')' -- (parenthesized pattern) <|> '(' pat_1 ',' ... ',' pat_k ')' -- (tuple pattern, k>=2) <|> '[' pat_1 ',' ... ',' pat_k ']' -- (list pattern, k>=1) <|> '~' apat -- (irrefutable pattern)

fpat -> qvar '=' pat


module -> 'module' modid [exports] 'where' body <|> body
body -> '{' impdecls ';' topdecls '}' <|> '{' impdecls '}' <|> '{' topdecls '}'

topdecls -> topdecl_1 ';' ... ';' topdecl_n -- (n>=1)
topdecl -> 'type' simpletype '=' type <|> 'data' [context '=>'] simpletype ['=' constrs] [deriving] <|> 'newtype' [context '=>'] simpletype '=' newconstr [deriving] <|> 'class' [scontext '=>'] varid varid ['where' cdecls] <|> 'instance' [scontext '=>'] qvarid inst ['where' idecls] <|> 'default' '('type_1 ',' ... ',' type_n')' -- \qquad (n>=0) <|> 'foreign' fdecl <|> decl

decls -> '{' decl_1 ';' ... ';' decl_n '}' -- (n>=0)
decl -> gendecl <|> (funlhs <|> pat) rhs

cdecls -> '{' cdecl_1 ';' ... ';' cdecl_n '}' -- (n>=0)
cdecl -> gendecl <|> (funlhs <|> var) rhs

idecls -> '{' idecl_1 ';' ... ';' idecl_n '}' -- (n>=0)
idecl -> (funlhs <|> var) rhs <|> -- (empty)

gendecl -> vars '::' [context '=>'] type -- (type signature) <|> fixity [integer] ops -- (fixity declaration) <|> -- (empty declaration)

ops -> op_1 ',' ... ',' op_n -- (n>=1)
vars -> var_1 ',' ... ',' var_n -- (n>=1)
fixity -> 'infixl' <|> 'infixr' <|> 'infix'


type -> btype ['->' type] -- (function type)

btype -> [btype] atype -- (type application)

atype -> gtycon <|> varid <|> '(' type_1 ',' ... ',' type_k ')' -- (tuple type, k>=2) <|> '[' type ']' -- (list type) <|> '(' type ')' -- (parenthesised constructor)

gtycon -> qvarid <|> '()' -- (unit type) <|> '[]' -- (list constructor) <|> '(->)' -- (function constructor) <|> '(,'{','}')' -- (tupling constructors)


context -> class <|> '(' class_1 ',' ... ',' class_n ')' -- (n>=0)
class -> qvarid varid <|> qvarid '(' varid atype_1 ... atype_n ')' -- (n>=1)
qvarid -> [ modid '.' ] varid
varid -> varid
varid -> varid


topdecl -> 'data' [context '=>'] simpletype ['=' constrs] [deriving]

simpletype -> varid tyvar_1 ... tyvar_k -- (k>=0)

constrs -> constr_1 '<|>' ... '<|>' constr_n -- (n>=1)
constr -> con ['!'] atype_1 ... ['!'] atype_k -- (arity con = k, k>=0) <|> (btype <|> '!' atype) conop (btype <|> '!' atype) -- (infix conop) <|> con '{' fielddecl_1 ',' ... ',' fielddecl_n '}' -- (n>=0)
fielddecl -> vars '::' (type <|> '!' atype)

deriving -> 'deriving' (dclass <|> '('dclass_1',' ... ',' dclass_n')')-- (n>=0)
dclass -> qvarid


topdecl -> 'type' simpletype '=' type
simpletype -> varid tyvar_1 ... tyvar_k -- (k>=0)


topdecl -> 'newtype' [context '=>'] simpletype '=' newconstr [deriving]
newconstr -> con atype <|> con '{' var '::' type '}'
simpletype -> varid tyvar_1 ... tyvar_k -- (k>=0)


topdecl -> 'class' [scontext '=>'] varid varid ['where' cdecls]
scontext -> simpleclass <|> '(' simpleclass_1 ',' ... ',' simpleclass_n ')' -- (n>=0)
simpleclass -> qvarid varid
cdecls -> '{' cdecl_1 ';' ... ';' cdecl_n '}' -- (n>=0)
cdecl -> gendecl <|> (funlhs <|> var) rhs


topdecl -> 'instance' [scontext '=>'] qvarid inst ['where' idecls]
inst -> gtycon <|> '(' gtycon tyvar_1 ... tyvar_k ')' -- (k>=0, tyvars distinct) <|> '(' tyvar_1 ',' ... ',' tyvar_k ')' -- (k>=2, tyvars distinct) <|> '[' varid ']' <|> '(' tyvar_1 '->' tyvar_2 ')' -- (tyvar_1 and tyvar_2 distinct)
idecls -> '{' idecl_1 ';' ... ';' idecl_n '}' -- (n>=0)
idecl -> (funlhs <|> var) rhs <|> -- (empty)

topdecl -> 'default' '('type_1 ',' ... ',' type_n')' -- (n>=0)


gendecl -> vars '::' [context '=>'] type
vars -> var_1 ',' ...',' var_n -- (n>=1)


gendecl -> fixity [integer] ops
fixity -> 'infixl' <|> 'infixr' <|> 'infix'
ops -> op_1 ',' ... ',' op_n -- (n>=1)
op -> varop <|> conop


decl -> (funlhs <|> pat) rhs

funlhs -> var apat { apat } <|> pat varop pat <|> '(' funlhs ')' apat { apat }

rhs -> '=' exp ['where' decls] <|> gdrhs ['where' decls]

gdrhs -> guards '=' exp [gdrhs]

guards -> '<|>' guard_1, ..., guard_n -- (n>=1)

guard -> pat '<-' infixexp -- (pattern guard) <|> 'let' decls -- (local declaration) <|> infixexp -- (boolean guard)



fnOpts =
    opts <- many fnOpt
    acc <- accessibility
    opts' <- many fnOpt
    let allOpts = opts ++ opts'
    let existingTotality = allOpts `intersect` [TotalFn, CoveringFn, PartialFn]
    opts'' <- addDefaultTotality (nub existingTotality) allOpts
    return (opts'', acc)
  where prettyTot TotalFn = "total"
        prettyTot PartialFn = "partial"
        prettyTot CoveringFn = "covering"
        addDefaultTotality [] opts =
          ist <- get
          case default_total ist of
            DefaultCheckingTotal    -> return (TotalFn:opts)
            DefaultCheckingCovering -> return (CoveringFn:opts)
            DefaultCheckingPartial  -> return opts -- Don't add partial so that --warn-partial still reports warnings if necessary
        addDefaultTotality [tot] opts = return opts
        -- Should really be a semantics error instead of a parser error
        addDefaultTotality (tot1:tot2:tots) opts =
          fail ("Conflicting totality modifiers specified " ++ prettyTot tot1 ++ " and " ++ prettyTot tot2)




fnOpt = do keyword "total"; return TotalFn
        <|> PartialFn <$ keyword "partial"
        <|> CoveringFn <$ keyword "covering"
        <|> do       (lchar '%' >*< reserved "export"); c <- stringLiteral;
                      return $ CExport c
        <|> NoImplicit <$       (lchar '%' >*< reserved "no_implicit")
        <|> Inlinable <$       (lchar '%' >*< reserved "inline")
        <|> StaticFn <$       (lchar '%' >*< reserved "static")
        <|> ErrorHandler <$       (lchar '%' >*< reserved "error_handler")
        <|> ErrorReverse <$       (lchar '%' >*< reserved "error_reverse")
        <|> ErrorReduce  <$       (lchar '%' >*< reserved "error_reduce")
        <|> Reflection   <$       (lchar '%' >*< reserved "reflection")
        <|> AutoHint     <$       (lchar '%' >*< reserved "hint")
        <|> OverlappingDictionary <$       (lchar '%' >*< reserved "overlapping")
        <|> do lchar '%'; reserved "specialise";
               lchar '['; ns <-   sepBy nameTimes (lchar ','); lchar ']';
               return $ Specialise ns
        <|> Implicit <$ keyword "implicit"
        <?> "function modifier"

        nameTimes = do n <- fnName
                       t <-   option Nothing (do reds <- natural
                                                 return (Just (fromInteger reds)))
                       return (n, t)



postulate = do (doc, ext)
                       <-       $ do (doc, _) <- docstring

                                     ext <- ppostDecl
                                     return (doc, ext)
                   ist <- get
                   (opts, acc) <- fnOpts
                   (n_in, nfc) <-  fnName
                   let n = expandNS n_in
                   lchar ':'
                   ty <- typeExpr (allowImp)
                   fc <- terminator
                   addAcc n acc
                   return (PPostulate ext doc fc nfc opts n ty)
                 <?> "postulate"
   where ppostDecl = do fc <- keyword "postulate"; return False
                 <|> do lchar '%'; reserved "extern"; return True



using_ =
    do keyword "using"
       lchar '('; ns <- usingDeclList; lchar ')'
       openBlock
       let uvars = using
       ds <- many (decl (syn { using = uvars ++ ns }))
       closeBlock
       return (concat ds)
    <?> "using declaration"



params =
    do (ns, fc) <-
          keyword "parameters"
          lchar '(' >*< typeDeclList <* lchar ')'
       let ns' = [(n, ty) | (_, n, _, ty) <- ns]
       openBlock
       let pvars = syn_params
       ds <- many (decl { syn_params = pvars ++ ns' })
       closeBlock
       return [PParams fc ns' (concat ds)]
    <?> "parameters declaration"



openInterface =
    do (ns, fc) <-
         keyword "using"
         keyword "implementation"
           sepBy1 ( fnName) (lchar ',')

       openBlock
       ds <- many (decl)
       closeBlock
       return [POpenInterfaces fc (map fst ns) (concat ds)]
    <?> "open interface declaration"







mutual =
    do fc <- keyword "mutual"
       openBlock
       ds <- many (decl (syn { mut_nesting = mut_nesting + 1 } ))
       closeBlock
       return [PMutual fc (concat ds)]
    <?> "mutual block"



namespace =
    do keyword "namespace"
       (n, nfc) <-  identifier
       openBlock
       ds <- some (decl { syn_namespace = n : syn_namespace })
       closeBlock
       return [PNamespace n nfc (concat ds)]
     <?> "namespace declaration"



implementationBlock = do keyword "where"
                             openBlock
                             ds <- many (fnDecl)
                             closeBlock
                             return (concat ds)
                          <?> "implementation block"



interfaceBlock = do keyword "where"
                        openBlock
                        (cn, cd) <-   option (Nothing, emptyDocstring) $
                                          (do (doc, _) <-   option noDocs docComment
                                              n <- constructor
                                              return (Just n, doc))
                        ist <- get
                        let cd' = annotate ist cd

                        ds <- many (notEndBlock >>       (implementation)
                                                   <|> do x <- data_
                                                          return [x]
                                                   <|> fnDecl)
                        closeBlock
                        return (cn, cd', concat ds)
                     <?> "interface block"
  where

    constructor = keyword "constructor" >*<  fnName


interface_ = do (doc, argDocs, acc)
                      <-       (do (doc, argDocs) <- docstring
                                   acc <- accessibility
                                   interfaceKeyword
                                   return (doc, argDocs, acc))
                    ((cons', n, nfc, cs, fds), fc) <-
                        cons <- constraintList
                        let cons' = [(c, ty) | (_, c, _, ty) <- cons]
                        (n_in, nfc) <-  fnName
                        let n = expandNS n_in
                        cs <- many carg
                        fds <-   option [(cn, NoFC) | (cn, _, _) <- cs] fundeps
                        return (cons', n, nfc, cs, fds)

                    (cn, cd, ds) <-   option (Nothing, fst noDocs, []) (interfaceBlock)
                    accData acc n (concatMap declared ds)
                    return [PInterface doc fc cons' n nfc cs argDocs fds ds cn cd]
                 <?> "interface declaration"
  where

    fundeps = do lchar '|';   sepBy ( name) (lchar ',')


    interfaceKeyword = keyword "interface"
               <|> do fc <- keyword "class"
                      parserWarning fc Nothing (Msg classWarning)


    carg = do lchar '('; (i, ifc) <-  name; lchar ':'; ty <- expr; lchar ')'
              return (i, ifc, ty)
       <|> do (i, ifc) <-  name
              return (i, ifc, PType ifc)



implementation = do (doc, argDocs) <- docstring
                        (opts, acc) <- fnOpts
                        optional implementationKeyword

                        ((en, cs, cs', cn, cnfc, args, pnames), fc) <-
                            en <- optional implementationName
                            cs <- constraintList
                            let cs' = [(c, ty) | (_, c, _, ty) <- cs]
                            (cn, cnfc) <-  fnName
                            args <- many (simpleExpr)
                            pnames <- implementationUsing
                            return (en, cs, cs', cn, cnfc, args, pnames)

                        let sc = PApp fc (PRef cnfc [cnfc] cn) (map pexp args)
                        let t = bindList (\r -> PPi constraint { pcount = r }) cs sc

                        ds <- implementationBlock
                        return [PImplementation doc argDocs fc cs' pnames acc opts cn cnfc args [] t en ds]
                      <?> "implementation declaration"

        implementationName = do lchar '['; n_in <- fnName; lchar ']'
                                let n = expandNS n_in
                                return n
                             <?> "implementation name"


        implementationKeyword = keyword "implementation"
                         <|> do fc <- keyword "instance"
                                parserWarning fc Nothing (Msg instanceWarning)


        implementationUsing = do keyword "using"
                                 ns <-   sepBy1 ( fnName) (lchar ',')
                                 return (map fst ns)
                              <|> return []


docstring = do (doc, argDocs) <-   option noDocs docComment
                   ist <- get
                   let doc' = annotCode (tryFullExpr ist) doc
                       argDocs' = [ (n, annotCode (tryFullExpr ist) d)
                                  | (n, d) <- argDocs ]
                   return (doc', argDocs')




usingDeclList
               =       (  sepBy1 (usingDecl) (lchar ','))
             <|> do ns <-   sepBy1 name (lchar ',')
                    lchar ':'
                    t <- typeExpr (disallowImp)
                    return (map (\x -> UImplicit x t) ns)
             <?> "using declaration list"



usingDecl =       (do x <- fnName
                          lchar ':'
                          t <- typeExpr (disallowImp)
                          return (UImplicit x t))
            <|> do c <- fnName
                   xs <- many fnName
                   return (UConstraint c xs)
            <?> "using declaration"

caf = do keyword "let"
             (n, fc) <-  (expandNS <$> fnName)

             lchar '='
             t <- indented $ expr
             terminator
             return (PCAF fc n t)
           <?> "constant applicative form declaration"



syntaxRule =
  choice
    Rule
      optional (keyword "term" <|> keyword "pattern")
      keyword "syntax"
      some syntaxSym
      lchar '='
      typeExpr (allowImp) >>= uniquifyBinders [n | Binding n <- syms]
      terminator
    DeclRule
      keyword "decl"
      keyword "syntax"
      some syntaxSym
      lchar '='
      openBlock
      some (decl)
      closeBlock

syntaxSym =
  choice
    Expr
      lchar '['
      name
      lchar ']'
    Binding
      lchar '{'
      name
      lchar '}'
    Keyword
      iName []
    Symbol
      stringLiteral


codegen_ =
  choice
    identifier
    reserved "Bytecode"

directive = do       (lchar '%' >*< reserved "lib")
                   cgn <- codegen_
                   lib <- stringLiteral
                   return [PDirective (DLib cgn lib)]
             <|> do       (lchar '%' >*< reserved "link")
                    cgn <- codegen_; obj <- stringLiteral
                    return [PDirective (DLink cgn obj)]
             <|> do       (lchar '%' >*< reserved "flag")
                    cgn <- codegen_; flag <- stringLiteral
                    return [PDirective (DFlag cgn flag)]
             <|> do       (lchar '%' >*< reserved "include")
                    cgn <- codegen_
                    hdr <- stringLiteral
                    return [PDirective (DInclude cgn hdr)]
             <|> do       (lchar '%' >*< reserved "hide"); n <- fnName
                    return [PDirective (DHide n)]
             <|> do       (lchar '%' >*< reserved "freeze"); n <- iName []
                    return [PDirective (DFreeze n)]
             <|> do       (lchar '%' >*< reserved "thaw"); n <- iName []
                    return [PDirective (DThaw n)]
             -- injectivity assertins are intended for debugging purposes
             -- only, and won't be documented/could be removed at any point
             <|> do       (lchar '%' >*< reserved "assert_injective"); n <- fnName
                    return [PDirective (DInjective n)]
             -- Assert totality of something after definition. This is
             -- here as a debugging aid, so commented out...
--              <|> do       (lchar '%' >*< reserved "assert_set_total"); n <- fst <$> fnName
--                     return [PDirective (DSetTotal n)]
             <|> do       (lchar '%' >*< reserved "access")
                    acc <- accessibility
                    ist <- get
                    put ist { default_access = acc }
                    return [PDirective (DAccess acc)]
             <|> do       (lchar '%' >*< reserved "default"); tot <- totality
                    i <- get
                    put (i { default_total = tot } )
                    return [PDirective (DDefault tot)]
             <|> do       (lchar '%' >*< reserved "logging")
                    i <- natural
                    return [PDirective (DLogging i)]
             <|> do       (lchar '%' >*< reserved "dynamic")
                    libs <-   sepBy1 stringLiteral (lchar ',')
                    return [PDirective (DDynamicLibs libs)]
             <|> do       (lchar '%' >*< reserved "name")
                    (ty, tyFC) <-  fnName
                    ns <-   sepBy1 ( name) (lchar ',')
                    return [PDirective (DNameHint ty tyFC ns)]
             <|> do       (lchar '%' >*< reserved "error_handlers")
                    (fn, nfc) <-  fnName
                    (arg, afc) <-  fnName
                    ns <-   sepBy1 ( name) (lchar ',')
                    return [PDirective (DErrorHandlers fn nfc arg afc ns) ]
             <|> do       (lchar '%' >*< reserved "language"); ext <- pLangExt;
                    return [PDirective (DLanguage ext)]
             <|> do       (lchar '%' >*< reserved "deprecate")
                    n <- fnName
                    alt <-   option "" stringLiteral
                    return [PDirective (DDeprecate n alt)]
             <|> do       (lchar '%' >*< reserved "fragile")
                    n <- fnName
                    alt <-   option "" stringLiteral
                    return [PDirective (DFragile n alt)]
             <|> do fc <-       (lchar '%' >*< reserved "used")
                    fn <- fnName
                    arg <- iName []
                    return [PDirective (DUsed fc fn arg)]
             <|> do       (lchar '%' >*< reserved "auto_implicits")
                    b <- on_off
                    return [PDirective (DAutoImplicits b)]
             <?> "directive"
  where on_off = do reserved "on"; return True
             <|> do reserved "off"; return False


pLangExt = (reserved "TypeProviders" >> return TypeProviders)
       <|> (reserved "ErrorReflection" >> return ErrorReflection)
       <|> (reserved "UniquenessTypes" >> return UniquenessTypes)
       <|> (reserved "LinearTypes" >> return LinearTypes)
       <|> (reserved "DSLNotation" >> return DSLNotation)
       <|> (reserved "ElabReflection" >> return ElabReflection)
       <|> (reserved "FirstClassReflection" >> return FCReflection)



totality
        = do keyword "total";   return DefaultCheckingTotal
      <|> do keyword "partial"; return DefaultCheckingPartial
      <|> do keyword "covering"; return DefaultCheckingCovering



provider = do doc <-       (do (doc, _) <- docstring
                                   lchar '%' >*< reserved "provide"
                                   return doc)
                  provideTerm doc <|> providePostulate doc
               <?> "type provider"
  where provideTerm doc =
          do lchar '('; (n, nfc) <-  fnName; lchar ':'; t <- typeExpr; lchar ')'
             keyword "with"
             (e, fc) <-  (expr) <?> "provider expression"
             return  [PProvider doc fc nfc (ProvTerm t e) n]
        providePostulate doc =
          do keyword "postulate"
             (n, nfc) <-  fnName
             keyword "with"
             (e, fc) <-  (expr) <?> "provider expression"
             return [PProvider doc fc nfc (ProvPostulate e) n]



transform = do       (lchar '%' >*< reserved "transform")
                    -- leave it unchecked, until we work out what this should
                    -- actually mean...
--                     safety <- option True (do reserved "unsafe"
--                                               return False)
                   l <- expr
                   fc <- symbol "==>"
                   r <- expr
                   return [PTransform fc False l r]
                <?> "transform"



runElabDecl =
  do kwFC <-       ( (lchar '%' >*< reserved "runElab"))
     script <- expr <?> "elaborator script"
     return $ PRunElabDecl kwFC script (syn_namespace)
  <?> "top-level elaborator script"



pPkg =
    reserved "package"
    p <- pPkgName
    someSpace
    modify $ \st -> st { pkgname = p }
    some pClause
    st <- get
      eof
    return st


pPkgName = (either fail pure . pkgName =<< packageName) <?> "PkgName"


filename = (do
                -- Treat a double-quoted string as a filename to support spaces.
                -- This also moves away from tying filenames to identifiers, so
                -- it will also accept hyphens
                -- (https://github.com/idris-lang/Idris-dev/issues/2721)
    filename <- stringLiteral
                -- Through at least version 0.9.19.1, IPKG executable values were
                -- possibly namespaced identifiers, like foo.bar.baz.
            <|> show <$> iName []
    case filenameErrorMessage filename of
      Just errorMessage -> fail errorMessage
      Nothing -> return filename)
    <?> "filename"


textUntilEol = many (  satisfy (not . isEol)) <* eol <* someSpace


clause name p f = do value <- reserved name >*< lchar '=' >*< p <* someSpace
                     modify $ \st -> f st value


commaSep p =   sepBy1 p (lchar ',')


pOptions =
  str <- stringLiteral
  case execArgParserPure (words str) of
    Opts.Success a -> return a
    Opts.Failure e -> fail $ fst $ Opts.renderFailure e ""
    _              -> fail "Unexpected error"


libIdentifier = identifierWithExtraChars "_'-."


pClause = clause "executable" filename (\st v -> st { execout = Just v })
      <|> clause "main" (iName []) (\st v -> st { idris_main = Just v })
      <|> clause "sourcedir" (identifier <|> stringLiteral) (\st v -> st { sourcedir = v })
      <|> clause "opts" pOptions (\st v -> st { idris_opts = v ++ idris_opts st })
      <|> clause "pkgs" (commaSep (pPkgName <* someSpace)) (\st ps ->
             let pkgs = pureArgParser $ concatMap (\x -> ["-p", show x]) ps
             in st { pkgdeps    = ps `union` pkgdeps st
                   , idris_opts = pkgs ++ idris_opts st })
      <|> clause "modules" (commaSep moduleName) (\st v -> st { modules = modules st ++ v })
      <|> clause "libs" (commaSep libIdentifier) (\st v -> st { libdeps = libdeps st ++ v })
      <|> clause "objs" (commaSep identifier) (\st v -> st { objs = objs st ++ v })
      <|> clause "makefile" (iName []) (\st v -> st { makefile = Just (show v) })
      <|> clause "tests" (commaSep (iName [])) (\st v -> st { idris_tests = idris_tests st ++ v })
      <|> clause "version" textUntilEol (\st v -> st { pkgversion = Just v })
      <|> clause "readme" textUntilEol (\st v -> st { pkgreadme = Just v })
      <|> clause "license" textUntilEol (\st v -> st { pkglicense = Just v })
      <|> clause "homepage" textUntilEol (\st v -> st { pkghomepage = Just v })
      <|> clause "sourceloc" textUntilEol (\st v -> st { pkgsourceloc = Just v })
      <|> clause "bugtracker" textUntilEol (\st v -> st { pkgbugtracker = Just v })
      <|> clause "brief" stringLiteral (\st v -> st { pkgbrief = Just v })
      <|> clause "author" textUntilEol (\st v -> st { pkgauthor = Just v })
      <|> clause "maintainer" textUntilEol (\st v -> st { pkgmaintainer = Just v })



record = (appdo
                (doc, paramDocs, acc, opts) <-       (do
                      (doc, paramDocs) <-   option noDocs docComment
                      ist <- get
                      let doc' = annotCode (tryFullExpr ist) doc
                          paramDocs' = [ (n, annotCode (tryFullExpr ist) d)
                                     | (n, d) <- paramDocs ]
                      acc <- accessibility
                      opts <- dataOpts []
                      co <- recordI
                      return (doc', paramDocs', acc, opts ++ co))
                (tyn_in, nfc) <-  fnName
                let tyn = expandNS tyn_in
                let rsyn = { syn_namespace = show (nsroot tyn) :
                                                    syn_namespace }
                params <-   manyTill (recordParameter rsyn) (keyword "where")
                (fields, cname, cdoc) <- indentedBlockS $ recordBody rsyn tyn
                let fnames = map (expandNS rsyn) (mapMaybe getName fields)
                case cname of
                     Just cn' -> accData acc tyn (fst cn' : fnames)
                     Nothing -> return ()
                return $ \fc -> PRecord doc rsyn fc opts tyn nfc params paramDocs fields cname cdoc)
              <?> "record type declaration"
  where

    recordBody tyn =
        ist <- get

        (constructorName, constructorDoc) <-   option (Nothing, emptyDocstring)
                                             (do (doc, _) <-   option noDocs docComment
                                                 n <-  constructor
                                                 return (Just n, doc))

        let constructorDoc' = annotate ist constructorDoc

        fields <- many . indented $ fieldLine

        return (concat fields, constructorName, constructorDoc')
      where

        fieldLine =
            doc <- optional docComment
            c <- optional $ lchar '{'
            let oneName = (do (n, nfc) <-  fnName
                              return $ Just (expandNS n, nfc))
                          <|> (symbol "_" >> return Nothing)
            ns <- commaSeparated oneName
            lchar ':'
            -- Implicits are scoped in fields (fields aren't top level)
            t <- typeExpr (scopedImp)
            p <- endPlicity c
            ist <- get
            let doc' = case doc of -- Temp: Throws away any possible arg docs
                        Just (d,_) -> Just $ annotate ist d
                        Nothing    -> Nothing
            return $ map (\n -> (n, p, t, doc')) ns


        constructor = keyword "constructor" >*< fnName


        endPlicity (Just _) = do lchar '}'
                                 return impl
        endPlicity Nothing = return expl


        annotate ist = annotCode $ tryFullExpr ist


recordParameter =
  (do lchar '('
      (n, nfc, pt) <- (namedTy <|> onlyName)
      lchar ')'
      return (n, nfc, expl, pt))
  <|>
  (do (n, nfc, pt) <- onlyName
      return (n, nfc, expl, pt))
  where

    namedTy =
      do (n, nfc) <-  fnName
         lchar ':'
         ty <- typeExpr (allowImp)
         return (expandNS n, nfc, ty)

    onlyName =
      do (n, nfc) <-  fnName
         return (expandNS n, nfc, PType nfc)





data_ = (checkDeclFixity $
            do (doc, argDocs, acc, dataOpts) <-       (do
                    (doc, argDocs) <-   option noDocs docComment

                    acc <- accessibility
                    errRev <- dataOpts []
                    co <- dataI
                    ist <- get
                    let dataOpts = errRev ++ co
                        doc' = annotCode (tryFullExpr ist) doc
                        argDocs' = [ (n, annotCode (tryFullExpr ist) d)
                                   | (n, d) <- argDocs ]
                    return (doc', argDocs', acc, dataOpts))
               (tyn_in, nfc) <-  fnName
               (do       (lchar ':')
                   ty <- typeExpr (allowImp)
                   let tyn = expandNS tyn_in
                   d <-   option (PData doc argDocs nfc dataOpts (PLaterdecl tyn nfc ty)) (do
                     keyword "where"
                     cons <- indentedBlock (constructor)
                     accData acc tyn (map (\ (_, _, n, _, _, _, _) -> n) cons)
                     return $ PData doc argDocs nfc dataOpts (PDatadecl tyn nfc ty cons))
                   terminator
                   return d) <|> (do
                    args <- many name
                    let ty = bindArgs (map (const (PType nfc)) args) (PType nfc)
                    let tyn = expandNS tyn_in
                    d <-   option (PData doc argDocs nfc dataOpts (PLaterdecl tyn nfc ty)) (do
                            (lchar '=') <|> do keyword "where"
                                               let kw = (if Codata `elem` dataOpts then "co" else "") ++ "data "
                                               let n  = show tyn_in ++ " "
                                               let s  = kw ++ n
                                               let as = unwords (map show args) ++ " "
                                               let ns = concat (intersperse " -> " $ map ((\x -> "(" ++ x ++ " : Type)") . show) args)
                                               let ss = concat (intersperse " -> " $ map (const "Type") args)
                                               let fix1 = s ++ as ++ " = ..."
                                               let fix2 = s ++ ": " ++ ns ++ " -> Type where\n  ..."
                                               let fix3 = s ++ ": " ++ ss ++ " -> Type where\n  ..."
                                               fail $ fixErrorMsg "unexpected \"where\"" [fix1, fix2, fix3]
                      cons <-   sepBy1 (simpleConstructor (syn { withAppAllowed = False })) (reservedOp "|")
                      let conty = mkPApp nfc (PRef nfc [] tyn) (map (PRef nfc []) args)
                      cons' <- mapM (\ (doc, argDocs, x, xfc, cargs, cfc, fs) ->
                                   do let cty = bindArgs cargs conty
                                      return (doc, argDocs, x, xfc, cty, cfc, fs)) cons
                      accData acc tyn (map (\ (_, _, n, _, _, _, _) -> n) cons')
                      return $ PData doc argDocs nfc dataOpts (PDatadecl tyn nfc ty cons'))
                    terminator
                    return d))
           <?> "data type declaration"


constructor
    = do (doc, argDocs) <-   option noDocs docComment
         (cn_in, nfc) <-  fnName
         let cn = expandNS cn_in
         lchar ':'
         fs <-   option [] (do lchar '%'; reserved "erase"
                                 sepBy1 name (lchar ','))
         (ty, fc) <- withtypeExpr (allowImp)
         ist <- get
         let doc' = annotCode (tryFullExpr ist) doc
             argDocs' = [ (n, annotCode (tryFullExpr ist) d)
                        | (n, d) <- argDocs ]
         checkNameFixity cn
         return (doc', argDocs', cn, nfc, ty, fc, fs)
      <?> "constructor"



simpleConstructor
     = (appdo
          (doc, _) <-   option noDocs (      docComment)
          ist <- get
          let doc' = annotCode (tryFullExpr ist) doc
          (cn_in, nfc) <-  fnName
          let cn = expandNS cn_in
          args <- many simpleExpr
          checkNameFixity cn
          return $ \fc -> (doc', [], cn, nfc, args, fc, []))
        <?> "constructor"


dsl = do keyword "dsl"
             n <- fnName
             bs <- indentedBlock (overload)
             let dsl = mkDSL bs (dsl_info)
             checkDSL dsl
             i <- get
             put (i { idris_dsls = addDef n dsl (idris_dsls i) })
             return (PDSL n dsl)
          <?> "dsl block declaration"

          mkDSL bs dsl = let var    = lookup "variable" bs
                             first  = lookup "index_first" bs
                             next   = lookup "index_next" bs
                             leto   = lookup "let" bs
                             lambda = lookup "lambda" bs
                             pi     = lookup "pi" bs in
                             initDSL { dsl_var = var,
                                       index_first = first,
                                       index_next = next,
                                       dsl_lambda = lambda,
                                       dsl_let = leto,
                                       dsl_pi = pi }


-- FIXME: currently does nothing, check if DSL is really sane
--
-- Issue #1595 on the Issue Tracker
--
--     https://github.com/idris-lang/Idris-dev/issues/1595
--

checkDSL dsl = return ()



overload = do o <- identifier <|> "let" <$ reserved "let"
                  if o `notElem` overloadable
                     then fail $ show o ++ " is not an overloading"
                     else
                       lchar '='
                       t <- expr
                       return (o, t)
               <?> "dsl overload declaratioN"
    where overloadable = ["let","lambda","pi","index_first","index_next","variable"]



optProof =
  optional
    keyword "proof"
    fnName

wExpr =
  lchar '|'
  expr' { inPattern = True }

whereBlock =
  keyword "where"
  indentedBlock1 (decl)

argExpr = choice
  hsimpleExpr {inPattern = true}
  simpleExternalExpr {inPattern = true}

dataI = do keyword "data"; return []
    <|> do keyword "codata"; return [Codata]


recordI = do keyword "record"; return []
          <|> do keyword "corecord"; return [Codata]


dataOpts opts =
      do reserved "%error_reverse"; dataOpts (DataErrRev : opts)
  <|> return opts
  <?> "data options"

simpleExternalExpr = do i <- get
                            extensions (filter isSimple (syntaxRulesList $ syntax_rules i))
  where
    isSimple (Rule (Expr x:xs) _ _) = False
    isSimple (Rule (SimpleExpr x:xs) _ _) = False
    isSimple (Rule [Keyword _] _ _) = True
    isSimple (Rule [Symbol _]  _ _) = True
    isSimple (Rule (_:xs) _ _) = case last xs of
        Keyword _ -> True
        Symbol _  -> True
        _ -> False
    isSimple _ = False



extensions rules = extension [] (filter isValid rules)
                       <?> "user-defined expression"
  where

    isValid (Rule _ _ AnySyntax) = True
    isValid (Rule _ _ PatternSyntax) = inPattern
    isValid (Rule _ _ TermSyntax) = not (inPattern)
    isValid (DeclRule _ _) = False

data SynMatch = SynTm PTerm | SynBind FC Name -- ^ the FC is for highlighting information
  deriving Show


extension ns rules =
    choice $ flip map (groupBy (ruleGroup `on` syntaxSymbols) rules) $ \rs ->
    case head rs of -- can never be []
      Rule (symb:_) _ _ ->       $
        n <- extensionSymbol symb
        extension (n : ns) [Rule ss t ctx | (Rule (_:ss) t ctx) <- rs]
      -- If we have more than one Rule in this bucket, our grammar is
      -- nondeterministic.
      Rule [] ptm _ -> return (flatten (updateSynMatch (mapMaybe id ns) ptm))
  where
    ruleGroup [] [] = True
    ruleGroup (s1:_) (s2:_) = s1 == s2
    ruleGroup _ _ = False


    extensionSymbol (Keyword n)    = Nothing <$ keyword (show n)
    extensionSymbol (Expr n)       = do tm <- expr
                                        return $ Just (n, SynTm tm)
    extensionSymbol (SimpleExpr n) = do tm <- simpleExpr
                                        return $ Just (n, SynTm tm)
    extensionSymbol (Binding n)    = do (b, fc) <-  name
                                        return $ Just (n, SynBind fc b)
    extensionSymbol (Symbol s)     = Nothing <$  (symbol s)



impossible = PImpossible <$ keyword "impossible"


   symbol : `identifier` | `special`
   identifier : `letter` (`letter` | `digit`)*
   special : `punct`+
   letter : "A"|...|"Z"|"a"|...|"z"|"_"|unicode character not punctuation or digit
   digit : "0"|...|"9"
   punct : "!"|"#"|"$"|"%"|"&"|ASCII punctuation|U+00A1 through U+00BF, U+00D7, U+00F7, and U+20D0 through U+2BFF

