--------------------------------------------------------------------------------
-- Copyright 2001-2012, Daan Leijen, Bastiaan Heeren, Jurriaan Hage. This file 
-- is distributed under the terms of the BSD3 License. For more information, 
-- see the file "LICENSE.txt", which is included in the distribution.
--------------------------------------------------------------------------------
--  $Id$

module Lvm.Core.Expr
  ( CoreModule
  , CoreDecl
  , Expr(..)
  , Binds(..)
  , Bind(..)
  , Alts
  , Alt(..)
  , Pat(..)
  , Literal(..)
  , PrimFun(..)
  , Con(..)
  , Variable(..)
  , ppPattern
  , IntType(..)
  , getExpressionStrictness

  , typeOfPrimFun
  , typeOfPrimFunArity
  )
where

import           Prelude                 hiding ( (<$>) )
import           Lvm.Common.Byte
import           Lvm.Common.Id
import           Lvm.Core.Module
import           Lvm.Core.PrettyId
import           Lvm.Core.Type
import           Text.PrettyPrint.Leijen

----------------------------------------------------------------
-- Modules
----------------------------------------------------------------
type CoreModule = Module Expr
type CoreDecl = Decl Expr

----------------------------------------------------------------
-- Core expressions:
----------------------------------------------------------------
data Expr       = Let       !Binds Expr
                | Match     !Id Alts
                | Ap        Expr Expr
                | ApType    !Expr !Type
                | Lam       !Bool !Variable Expr
                | Forall    !Quantor !Kind !Expr
                | Con       !Con
                | Var       !Id
                | Lit       !Literal
                | Prim      !PrimFun

data Variable = Variable { variableName :: !Id, variableType :: !Type }
data Binds      = Rec       ![Bind]
                | Strict    !Bind
                | NonRec    !Bind

data Bind       = Bind      !Variable !Expr

type Alts = [Alt]
data Alt        = Alt       !Pat Expr

data Pat
  = PatCon    !Con ![Type] ![Id]
  | PatLit    !Literal
  | PatDefault

data Literal    = LitInt    !Int !IntType
                | LitDouble !Double
                | LitBytes  !Bytes

data PrimFun    = PrimFinish
                | PrimRead
                | PrimWrite
                | PrimWriteCtor !Con
                | PrimToEnd
                | PrimNewCursor
  deriving (Eq, Ord) -- Required for Iridium's Expr's (Eq, Ord)

data Con        = ConId  !Id
                | ConTuple !Arity
  deriving (Eq, Ord) -- Required for above Eq, Ord

----------------------------------------------------------------
-- Pretty printing
----------------------------------------------------------------

instance Pretty Expr where
  pretty = ppExpr 0 []

ppExpr :: Int -> QuantorNames -> Expr -> Doc
ppExpr p quantorNames expr = case expr of
  Match x as -> prec 0 $ align
    (   text "match"
    <+> ppVarId x
    <+> text "with"
    <+> text "{"
    <$> indent 2 (ppAlts quantorNames as)
    <+> text "}"
    )
  Let bs x -> prec 0 $ align
    (ppLetBinds quantorNames bs (text "in" <+> ppExpr 0 quantorNames x))
  Lam strict (Variable x t) e ->
    prec 0
      $   text (if strict then "\\ !" else "\\")
      <>  ppVarId x
      <>  text ": "
      <>  ppType 0 quantorNames t
      <+> text "->"
      <$> indent 2 (ppExpr 0 quantorNames e)
  Forall quantor k e ->
    let quantorNames' = case quantor of
          Quantor idx (Just name) -> (idx, name) : quantorNames
          _                       -> quantorNames
    in  prec 0
          $   text "forall"
          <+> text (show quantor)
          <>  text ": "
          <>  pretty k
          <>  text "."
          <$> indent 2 (ppExpr 0 quantorNames' e)
  Ap e1 e2 -> prec 9 $ ppExpr 9 quantorNames e1 <+> ppExpr 10 quantorNames e2
  ApType e1 t ->
    prec 9
      $   ppExpr 9 quantorNames e1
      <+> text "{ "
      <>  ppType 0 quantorNames t
      <>  text " }"
  Var x   -> ppVarId x
  Con con -> pretty con
  Lit lit -> pretty lit
  Prim pr -> pretty pr
 where
  prec p' | p' >= p   = id
          | otherwise = parens

instance Pretty Con where
  pretty con = case con of
    ConId    x     -> ppConId x
    ConTuple arity -> parens (char '@' <> pretty arity)

----------------------------------------------------------------
--
----------------------------------------------------------------

ppLetBinds :: QuantorNames -> Binds -> Doc -> Doc
ppLetBinds quantorNames binds doc = case binds of
  NonRec bind -> nest 4 (text "let" <+> ppBind quantorNames bind) <$> doc
  Strict bind -> nest 5 (text "let!" <+> ppBind quantorNames bind) <$> doc
  Rec    recs -> nest 4 (text "let" <+> ppBindList quantorNames recs) <$> doc -- let rec not parsable

ppBind :: QuantorNames -> Bind -> Doc
ppBind quantorNames (Bind (Variable x t) expr) = nest
  2
  (   ppId x
  <>  text ": "
  <+> ppType 0 quantorNames t
  <>  text " = "
  <+> ppExpr 0 quantorNames expr
  <>  semi
  )

ppBindList :: QuantorNames -> [Bind] -> Doc
ppBindList quantorNames = vcat . map (ppBind quantorNames)

ppAlt :: QuantorNames -> Alt -> Doc
ppAlt quantorNames (Alt pat expr) = nest
  4
  (   ppPattern quantorNames pat
  <+> text "->"
  </> ppExpr 0 quantorNames expr
  <>  semi
  )

ppAlts :: QuantorNames -> [Alt] -> Doc
ppAlts quantorNames = vcat . map (ppAlt quantorNames)

----------------------------------------------------------------
--
----------------------------------------------------------------


ppPattern :: QuantorNames -> Pat -> Doc
ppPattern quantorNames (PatCon con tps ids) = hsep
  (  pretty con
  :  map (\tp -> text "{" <+> ppType 0 quantorNames tp <+> text "}") tps
  ++ map ppVarId ids
  )
ppPattern _ (PatLit lit) = pretty lit
ppPattern _ PatDefault   = text "_"

instance Pretty Literal where
  pretty lit = case lit of
    LitInt i t  -> text "(@" <> text (show t) <+> pretty i <> text ")"
    LitDouble d -> pretty d
    LitBytes  s -> text (show (stringFromBytes s))

instance Pretty PrimFun where
  pretty PrimFinish     = text "_prim_finish"
  pretty PrimRead       = text "_prim_read"
  pretty PrimWrite      = text "_prim_write"
  pretty PrimToEnd      = text "_prim_to_end"
  pretty PrimNewCursor  = text "_prim_new_cursor"
  pretty (PrimWriteCtor (ConId x)) =
    text "(_prim_write_ctor" <+> text (stringFromId x) <> text ")"
  pretty (PrimWriteCtor _) =
    text "_prim_write_ctor__tuple"

instance Show PrimFun where
  show = show . pretty

getExpressionStrictness :: Expr -> [Bool]
getExpressionStrictness (Forall _ _ expr) = getExpressionStrictness expr
getExpressionStrictness (Lam strict _ expr) =
  strict : getExpressionStrictness expr
getExpressionStrictness _ = []

-- TODO: REMOVE THESE LATER, just here for testing
packedIntType, intType :: Type
packedIntType = TCon $ typeConFromString "TreeTest.PACKED_Int"
intType       = TCon $ typeConFromString "Int"

-- The reason this function has been moved here, is so that Iridium can reach it too!
typeOfPrimFunArity :: PrimFun -> (Int, Type)
typeOfPrimFunArity PrimFinish = (,) 2 $
  typeFunction
    [ TStrict $ TCon (TConCursorNeeds [] packedIntType)
    , TStrict $ TCon (TConCursorNeeds [packedIntType] packedIntType)
    ]
    packedIntType -- TODO: Return Has cursor
typeOfPrimFunArity PrimRead   = undefined
typeOfPrimFunArity PrimWrite  = (,) 2 $
  typeFunction
    [ TStrict $ TCon (TConCursorNeeds [intType] packedIntType)
    , TStrict $ intType
    ]
    (TCon (TConCursorNeeds [] packedIntType))
typeOfPrimFunArity (PrimWriteCtor c) = (,) 1 $
  typeFunction
    [ TStrict $ TCon $ TConCursorNeeds [packedIntType] packedIntType
    ]
    (TCon $ TConCursorNeeds [intType] packedIntType)
typeOfPrimFunArity PrimToEnd = (,) 1 $
  typeFunction
    [ TStrict $ TCon $ TConCursorNeeds [] packedIntType
    ]
    (TCon (TConCursorEnd 0)) -- TODO: Implement addresses for cursors
typeOfPrimFunArity PrimNewCursor = (0, TCon $ TConCursorNeeds [packedIntType] packedIntType)
--typeOfPrimFun PrimFinish =
--  TForall (Quantor 0 (Just "a")) KStar $
--  TAp (TAp (TCon TConFun) (TCon (TConCursorNeeds [] (TVar 0)))) (TVar 0)
--typeOfPrimFun PrimWrite  =
--  TForall (Quantor 0 (Just "a")) KStar $
--  TForall (Quantor 1 (Just "b")) KStar $
--  typeApply (typeApply (TCon TConFun) (TCon (TConCursorNeeds [TVar 0] (TVar 1)))) (TVar 0)
  -- Does not recognize the TVars inside the TConCursor...

typeOfPrimFun :: PrimFun -> Type
typeOfPrimFun = snd . typeOfPrimFunArity

