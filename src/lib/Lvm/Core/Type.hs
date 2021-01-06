{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- Copyright 2001-2012, Daan Leijen, Bastiaan Heeren, Jurriaan Hage. This file
-- is distributed under the terms of the BSD3 License. For more information,
-- see the file "LICENSE.txt", which is included in the distribution.
--------------------------------------------------------------------------------
--  $Id: Data.hs 250 2012-08-22 10:59:40Z bastiaan $

module Lvm.Core.Type
   ( Type(..), Kind(..), TypeConstant(..), Quantor(..), QuantorNames, IntType(..), TypeCursor(..)
   , ppTypeVar, ppType, showType, freshQuantorName, arityFromType, typeUnit, typeBool
   , typeToStrict, typeNotStrict, typeIsStrict, typeSetStrict, typeConFromString, typeFunction
   , typeSubstitute, typeTupleElements, typeRemoveArgumentStrictness, typeWeaken
   , typeSubstitutions, typeExtractFunction, typeApply, typeApplyList, dictionaryDataTypeName
   , typeList, typePrependList
   -- Pattern synonyms
   , pattern TypeCons, pattern TypeFun
   ) where

import Lvm.Common.Id
import Text.PrettyPrint.Leijen

import Debug.Trace

import qualified Data.Set as S
import qualified Data.Map as M

----------------------------------------------------------------
-- Types
----------------------------------------------------------------
data Type = TAp !Type !Type
          | TForall !Quantor !Kind !Type
          -- * The inner type should have kind *. The inner type
          -- may not be TStrict
          | TStrict !Type
          -- * We use Debruijn indices to identify type variables.
          | TVar !Int
          | TCon !TypeConstant
          | TCursor !TypeCursor
          deriving (Eq, Ord)

newtype Quantor
  = Quantor (Maybe String)
  deriving (Eq, Ord)

type QuantorNames = [String]

data TypeConstant
  = TConDataType !Id
  | TConTuple !Int
  | TConTypeClassDictionary !Id
  | TConFun
  | TConCons -- Apply to a type, then to another list
  | TConNil
  | TConReturnType
  | TConAppend
  | TConWriteLength
  deriving (Eq, Ord)

data TypeCursor
  = TCursorEnd !Int -- TODO: Should be an address...
  | TCursorNeeds Type Type -- Needs inputs to produce output.
  | TCursorHas Type
  deriving (Eq, Ord)

data IntType
  = IntTypeInt
  | IntTypeChar
  -- Currently only used by the backend, for fields of thunks.
  -- To expose this to the frontend, we will also need to implement casts to / from int16
  | IntTypeInt16
  deriving (Eq, Ord)

instance Show IntType where
  show IntTypeInt = "Int"
  show IntTypeChar = "Char"

data Kind = KFun !Kind !Kind
          | KStar
          deriving (Eq, Ord)

pattern TypeCons head tail = TAp (TAp (TCon TConCons) head) tail
pattern TypeFun  arg  res  = TAp (TAp (TCon TConFun)  arg ) res

typeConFromString :: String -> TypeConstant
typeConFromString "->" = TConFun
typeConFromString ('(' : str)
  | rest == ")" = TConTuple (length commas + 1)
  where
    (commas, rest) = span (== ',') str
typeConFromString name = TConDataType $ idFromString name

typeToStrict :: Type -> Type
typeToStrict (TForall quantor kind t) = TForall quantor kind $ typeToStrict t
typeToStrict t@(TStrict _) = t
typeToStrict t = TStrict t

typeNotStrict :: Type -> Type
typeNotStrict (TForall quantor kind t) = TForall quantor kind $ typeNotStrict t
typeNotStrict (TStrict t) = typeNotStrict t
typeNotStrict t = t

typeIsStrict :: Type -> Bool
typeIsStrict (TForall _ _ t) = typeIsStrict t
typeIsStrict (TStrict _) = True
typeIsStrict _ = False

typeSetStrict :: Bool -> Type -> Type
typeSetStrict True = typeToStrict
typeSetStrict False = typeNotStrict

typeRemoveArgumentStrictness :: Type -> Type
typeRemoveArgumentStrictness (TForall quantor kind tp) = TForall quantor kind $ typeRemoveArgumentStrictness tp
typeRemoveArgumentStrictness (TypeFun tArg tReturn) =
  TypeFun (typeNotStrict tArg) $ typeRemoveArgumentStrictness tReturn
typeRemoveArgumentStrictness (TStrict tp) = TStrict $ typeRemoveArgumentStrictness tp
typeRemoveArgumentStrictness tp = tp

typeUnit :: Type
typeUnit = TCon $ TConTuple 0

typeBool :: Type
typeBool = TCon $ TConDataType $ idFromString "Bool"

typeFunction :: [Type] -> Type -> Type
typeFunction [] ret = ret
typeFunction (a:as) ret = TypeFun a $ typeFunction as ret

arityFromType :: Type -> Int
arityFromType tp
  = case tp of
      TypeFun _ t2    -> arityFromType t2 + 1
      TAp     _ _     -> 0 -- assumes saturated constructors!
      TForall _ _ t   -> arityFromType t
      TStrict t       -> arityFromType t
      TVar    _       -> 0
      TCon    _       -> 0

----------------------------------------------------------------
-- Pretty printing
----------------------------------------------------------------

showType :: QuantorNames -> Type -> String
showType quantors tp = show $ ppType 5 quantors tp

instance Show Kind where
  show = show . pretty

instance Pretty Type where
  pretty = ppType 0 []

instance Pretty Kind where
  pretty = ppKind 0

instance Pretty TypeConstant where
  pretty (TConDataType name) = pretty name
  pretty (TConTypeClassDictionary name) = text "(@dictionary" <+> pretty name <+> text ")"
  pretty (TConTuple arity) = text ('(' : replicate (arity - 1) ',' ++ ")")
  pretty TConFun = text "->"
  pretty TConNil = text "$[]"
  pretty TConCons = text "$:"
  pretty TConReturnType = text "ReturnType"
  pretty TConAppend = text "CursorAppendFn"
  pretty TConWriteLength = text "WriteLength"

instance Pretty TypeCursor where
  pretty = ppCursor []

dictionaryDataTypeName :: Id -> Id
dictionaryDataTypeName = idFromString . ("Dict$" ++) . stringFromId

instance Show TypeConstant where
  show = show . pretty

ppTypeVar :: QuantorNames -> Int -> Doc
ppTypeVar (q:_ ) 0 = text q
ppTypeVar (_:qs) i = ppTypeVar qs (i - 1)
ppTypeVar []     i = text $ "f$" ++ show i ++ ""

freshQuantorName :: QuantorNames -> Quantor -> String
freshQuantorName quantorNames (Quantor (Just name))
  | name `notElem` quantorNames = name
freshQuantorName quantorNames _ = "v$" ++ show (length quantorNames)

ppType :: Int -> QuantorNames -> Type -> Doc
ppType level quantorNames tp
  = parenthesized $
    case tp of
      TAp (TCon a) t2 | a == TConDataType (idFromString "[]") -> text "[" <> ppType 0 quantorNames t2 <> text "]"
      TypeFun t1 t2   -> ppHi t1 <+> text "->" <+> ppEq t2
      TAp     t1 t2   -> ppEq t1 <+> ppHi t2
      TForall quantor k t   ->
        let quantorName = freshQuantorName quantorNames quantor
        in text "forall" <+> text quantorName {- <> text ":" <+> pretty k -} <> text "."
            <+> ppType 0 (quantorName : quantorNames) t
      TStrict t       -> text "!" <> ppHi t
      TVar    a       -> ppTypeVar quantorNames a
      TCon    a       -> pretty a
      TCursor a       -> ppCursor quantorNames a
  where
    tplevel = levelFromType tp
    parenthesized doc
      | level <= tplevel  = doc
      | otherwise         = parens doc
    ppHi  = ppType (tplevel+1) quantorNames
    ppEq = ppType tplevel quantorNames

ppCursor :: QuantorNames -> TypeCursor -> Doc
ppCursor quantorNames (TCursorEnd x) = text "End(addr<" <> pretty x <> text ">)" -- TODO: see if this can be more... Text, less String
ppCursor quantorNames (TCursorNeeds ts t) =
      text "Needs(["
  <>  ppType 0 quantorNames ts
  <>  text "],"
  <+> ppType 0 quantorNames t
  <>  text ")"
ppCursor quantorNames (TCursorHas ts) =
      text "Has(["
  <> ppType 0 quantorNames ts
  <> text "])"

ppKind :: Int -> Kind -> Doc
ppKind level kind
  = parenthesized $
    case kind of
      KFun k1 k2    -> ppHi k1 <+> text "->" <+> ppEq k2
      KStar         -> text "*"
  where
    (klevel,parenthesized)
      | level <= levelFromKind kind   = (levelFromKind kind,id)
      | otherwise                     = (0,parens)

    ppHi = ppKind (if klevel<=0 then 0 else klevel+1)
    ppEq = ppKind klevel

levelFromType :: Type -> Int
levelFromType tp
  = case tp of
      TForall{} -> 2
      TypeFun _ _ -> 3
      TAp{}     -> 4
      TStrict{} -> 5
      TVar{}    -> 6
      TCon{}    -> 6
      TCursor{} -> 6

levelFromKind :: Kind -> Int
levelFromKind kind
  = case kind of
      KFun{}    -> 1
      KStar{}   -> 2

-- Reindexes the type variables in the type
typeReindex :: (Int -> Int) -> Type -> Type
typeReindex k = go 0
  where
    -- n is the number of TForalls that we descended into.
    -- That is an offset when calling `k`, as we shouldn't
    -- reindex any bound type variables
    go :: Int -> Type -> Type
    go n (TAp t1 t2) = TAp (go n t1) (go n t2)
    go n (TForall q kind t) = TForall q kind $ go (n + 1) t
    go n (TStrict t) = TStrict $ go n t
    -- go n (TCursor (TCursorNeeds ins out)) = TCursor $ TCursorNeeds (go k' `fmap` ins) (go k' out)
    -- go n (TCursor c@(TCursorEnd _)) = TCursor c
    go n (TCursor c) = undefined
    go n (TVar idx)
      | idx < n = TVar idx
      | otherwise = TVar $ n + k (idx - n)
    go n (TCon c) = TCon c

-- Increases all Debruijn indices of free type variables
typeWeaken :: Int -> Type -> Type
typeWeaken 0 = id
typeWeaken m = typeReindex (m+)

-- Applies a type substitution of free type variables with Debruijn indices
-- k..(k + length tps). It maps `k` to `head tps`, `k + 1` to `tps !! 1`, ...
typeSubstitutions :: Int -> [Type] -> Type -> Type
typeSubstitutions _ []  = id
typeSubstitutions k tps = go k
  where
    tpCount = length tps

    go :: Int -> Type -> Type
    go k' (TAp t1 t2) = TAp (go k' t1) (go k' t2)
    go k' (TForall q kind t) = TForall q kind $ go (k' + 1) t
    go k' (TStrict t) = TStrict $ go k' t
    go k' (TVar idx) = substitute k' idx
    go k' (TCon c) = TCon c
    go k' (TCursor (TCursorNeeds ins out)) = TCursor $ TCursorNeeds (go k' ins) (go k' out)
    go k' (TCursor c@(TCursorEnd _)) = TCursor c
    go k' (TCursor c) = undefined

    substitute :: Int -> Int -> Type
    substitute k' idx
      | idx < k' = TVar idx
      -- Note that we need to weaken here if we descended into TForalls.
      -- We did that k'-k times.
      | idx < k' + tpCount = typeWeaken (k' - k) $ tps !! (idx - k')
      | otherwise = TVar $ idx - tpCount

typeSubstitute :: Int -> Type -> Type -> Type
typeSubstitute var rightType leftType = typeSubstitutions var [rightType] leftType

typeListElement :: Type -> Type
typeListElement (TAp (TCon (TConDataType dataType)) a)
  | dataType == idFromString "[]" = a
typeListElement tp = error $ "typeListElement: expected a list type, got " ++ showType [] tp ++ " instead"

typeTupleElements :: Type -> [Type]
typeTupleElements tupleType = elements 0 tupleType []
  where
    elements n (TCon (TConTuple m)) accum
      | n < m = error $ "typeTupleElements: expected a saturated tuple type, got a partially applied tuple type (received " ++ show n ++ " arguments, expected " ++ show m ++ ")"
      | n > m = error $ "typeTupleElements: got an over applied tuple type"
      | otherwise = accum
    elements n (TAp t1 t2) accum = elements (n + 1) t1 (t2 : accum)
    elements _ (TVar _) _ = error $ "typeTupleElements: expected a tuple type, got a type variable instead"
    elements _ tp _ = error $ "typeTupleElements: expected a tuple type, got " ++ showType [] tp ++ " instead"

typeExtractFunction :: Type -> ([Type], Type)
typeExtractFunction (TypeFun t1 t2) = (t1 : args, ret)
  where
    (args, ret) = typeExtractFunction t2
typeExtractFunction tp = ([], tp)

typeApply :: Type -> Type -> Type
typeApply (TForall _ _ t1) t2 = typeSubstitute 0 t2 t1
typeApply t1 t2 = TAp t1 t2

typeApplyList :: Type -> [Type] -> Type
-- typeApplyList = foldl typeApply
typeApplyList tp args = go tp args []
  where
    go :: Type -> [Type] -> [Type] -> Type
    go tp [] [] = tp
    go tp [] substitution = typeSubstitutions 0 substitution tp
    go (TForall _ _ tp) (arg:args) substitution = go tp args (arg:substitution)
    go tp args [] = foldl TAp tp args
    go tp args substitution = go (typeSubstitutions 0 substitution tp) args []

-- Could be a fold
typeList :: [Type] -> Type
typeList [] = TCon $ TConNil
typeList (x:xs) = TypeCons x $ typeList xs

-- Assumes the second argument is a list type
typePrependList :: [Type] -> Type -> Type
typePrependList []     t = t
typePrependList (x:xs) t = TypeCons x $ typePrependList xs t

