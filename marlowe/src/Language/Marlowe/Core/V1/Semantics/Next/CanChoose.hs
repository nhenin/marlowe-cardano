
{-# LANGUAGE DataKinds #-}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE MultiParamTypeClasses #-}

{-# LANGUAGE DeriveAnyClass #-}

{-# LANGUAGE NoImplicitPrelude #-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Language.Marlowe.Core.V1.Semantics.Next.CanChoose
  ( CanChoose(..)
  , difference
  , overlap
  ) where


import Control.Monad ((<=<))
import Data.Aeson.Types (FromJSON(parseJSON), KeyValue((.=)), ToJSON(toJSON), Value(Object), object, (.:))
import Data.List (tails)
import Data.Maybe (fromJust)
import qualified Data.Range as R
import GHC.Generics (Generic)
import Language.Marlowe.Core.V1.Semantics.Next.Indexed (CaseIndex(CaseIndex), Indexed(..), getIndexedValue)
import Language.Marlowe.Core.V1.Semantics.Next.IsMerkleizedContinuation
import Language.Marlowe.Core.V1.Semantics.Types (Bound(..), ChoiceId)
import Language.Marlowe.Pretty (Pretty)
import Prelude

data CanChoose  = CanChoose {choiceId :: ChoiceId, bounds :: [Bound] ,isMerkleizedContinuation :: IsMerkleizedContinuation}
  deriving stock (Show,Eq,Ord,Generic)
  deriving anyclass (Pretty)


overlap :: [CanChoose]  -> Bool
overlap l
    = let combinations = [(x,y) | (x:ys) <- tails l, y <- ys]
      in any (uncurry overlapWith) combinations

overlapWith :: CanChoose -> CanChoose -> Bool
overlapWith a b | choiceId a /= choiceId b
    = let combinations = [(x,y) | x <- bounds a, y <- bounds b]
      in any (uncurry overlapWith') combinations
overlapWith _ _ = False

overlapWith' :: Bound -> Bound -> Bool
overlapWith' a b = R.rangesOverlap (toRange a) (toRange b)

boundsByChoiceId :: ChoiceId -> [Indexed CanChoose] -> [Bound]
boundsByChoiceId choiceId' = bounds <=< (filter(\x -> choiceId'  == choiceId x) . fmap getIndexedValue)


difference :: Indexed CanChoose -> [Indexed CanChoose] -> Maybe (Indexed CanChoose)
difference (Indexed i CanChoose {..})
    = (\case
       [] -> Nothing
       newBounds -> Just $ Indexed i (CanChoose choiceId newBounds isMerkleizedContinuation))
    . difference' bounds
    . boundsByChoiceId choiceId


difference'
    :: [Bound]
    -> [Bound]
    -> [Bound]
difference' xs ys = toBounds <$> R.difference (toRange <$> xs) $ (toRange <$> ys)


toRange :: Bound -> R.Range Integer
toRange (Bound a b) = R.SpanRange (R.Bound a R.Inclusive) (R.Bound b R.Inclusive)

toBounds :: [R.Range Integer] -> [Bound]
toBounds xs = fromJust . toBound <$> xs

toBound :: R.Range Integer -> Maybe Bound
toBound (R.SpanRange (R.Bound a R.Inclusive) (R.Bound b R.Inclusive)) = Just $ Bound a b
toBound _ = Nothing


instance FromJSON (Indexed CanChoose) where
  parseJSON (Object v)
    =  Indexed
         <$>  (CaseIndex <$> v .: "case_index")
         <*>  (CanChoose
                <$>  v .: "for_choice"
                <*>  v .: "can_choose_between"
                <*>  v .: "is_merkleized_continuation")
  parseJSON _ = fail "CanChoose must be an object CanChoose "

instance ToJSON (Indexed CanChoose) where
  toJSON (Indexed caseIndex (CanChoose choiceId bounds isMerkleizedContinuation)) = object
      [ "for_choice" .= choiceId
      , "can_choose_between" .= bounds
      , "case_index" .= caseIndex
      , "is_merkleized_continuation" .= isMerkleizedContinuation
      ]
