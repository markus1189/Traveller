{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeFamilies #-}
module Main (main) where

import           Control.Applicative (Applicative, pure, (*>), (<$>), (<*>))
import           Control.Lens (Index, IxValue, Ixed, ix, preview, traverse, view)
import           Control.Lens.Operators
import           Control.Lens.TH
import           Control.Monad.Random (MonadRandom, getRandomRs)
import           Data.Foldable (find)
import           Data.Functor (void)
import           Data.IORef (IORef, newIORef, readIORef, writeIORef)
import           Data.List (genericTake, genericLength, genericReplicate, foldl')
import           Data.Maybe (listToMaybe, fromMaybe)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as T
import           Graphics.Vty.LLInput
import           Graphics.Vty.Widgets.All
import           System.Random (Random)
import           Text.Printf (printf)

newtype Width  = Width Int  deriving (Show,Ord,Eq,Enum,Integral,Num,Real,Random)
newtype Height = Height Int deriving (Show,Ord,Eq,Enum,Integral,Num,Real,Random)

data Element = Wall | Gold | Hero | Empty deriving Eq

elementToText :: Element -> Text
elementToText Wall = "#"
elementToText Gold = "."
elementToText Hero = "@"
elementToText Empty = " "

newtype Level = Level { unLevel :: [[Element]] }
makeLenses ''Level
makePrisms ''Level

type instance Index Level = (Width,Height)
type instance IxValue Level = Element
instance Ixed Level where
  ix (Width w,Height h) = _Level . ix h . ix w

dimensions :: Level -> (Width,Height)
dimensions (Level cs) = (width,height)
  where width = maybe 0 genericLength . listToMaybe $ cs
        height = genericLength cs

frame :: Element -> Level -> Level
frame fill l@(Level cells) =
  Level $ [upperAndLower] ++ fmap padLine cells ++ [upperAndLower]
  where (width,_) = dimensions l
        upperAndLower = genericReplicate (width + 2) fill
        padLine t = [fill] ++ t ++ [fill]

toText :: Level -> Text
toText = T.intercalate "\n"
         . fmap T.concat
         . (traverse . traverse %~ elementToText)
         . unLevel
         . frame Wall

data GameState = GameState { _level :: Level
                           , _moves :: Int
                           , _score :: Int
                           , _heroPos :: (Width,Height)
                           }
makeLenses ''GameState

voidBool :: Applicative f => f a -> f Bool
voidBool x = x *> pure True

scoreBoard :: GameState -> Text
scoreBoard = interpolate <$> view moves <*> view score
  where interpolate m s = T.pack $ printf "Moves: %3d, Score: %3d\n" m s

main :: IO ()
main = do initGs <- initialGameState
          rlvl <- plainText (initGs ^. level & toText)
          scores <- plainText (scoreBoard initGs)
          ui <- centered =<< vBox rlvl scores
          fg <- newFocusGroup
          void $ addToFocusGroup fg rlvl
          gs <- newIORef initGs
          fg `onKeyPressed` \_ key _ ->
             case key of
               KASCII 'q' -> voidBool shutdownUi
               KASCII 'h' -> voidBool . modifyGameState scores rlvl gs $ moveHero LEFT
               KASCII 'j' -> voidBool . modifyGameState scores rlvl gs $ moveHero DOWN
               KASCII 'k' -> voidBool . modifyGameState scores rlvl gs $ moveHero UP
               KASCII 'l' -> voidBool . modifyGameState scores rlvl gs $ moveHero RIGHT
               _ -> return False
          c <- newCollection
          void $ addToCollection c ui fg
          runUi c defaultContext

modifyGameState :: Widget FormattedText -> Widget FormattedText -> IORef GameState -> (GameState -> GameState) -> IO ()
modifyGameState scoresArea renderArea gs f = do
  oldState <- readIORef gs
  let newState = f oldState
  setText renderArea (newState ^. level & toText)
  setText scoresArea (scoreBoard newState)
  writeIORef gs newState

data Direction = LEFT | DOWN | UP | RIGHT

adjust :: Direction -> (Width,Height) -> (Width,Height)
adjust d (x,y) = case d of
                   LEFT -> (x-1,y)
                   DOWN -> (x,y+1)
                   UP -> (x,y-1)
                   RIGHT -> (x+1,y)

moveHero :: Direction -> GameState -> GameState
moveHero dir gs =
  if gs ^. moves > 0 && inBounds newHeroPosition (view level gs)
     then gs &~ do
       level . ix heroPosition .= Empty
       heroPos .= newHeroPosition
       score %= tryPickup (gs ^. level) newHeroPosition
       level . ix newHeroPosition .= Hero
       moves -= 1
     else gs
  where inBounds (x,y) (dimensions -> (lx,ly)) =
          x `elem` [0..lx-1] && y `elem` [0..ly-1]
        heroPosition = gs ^. heroPos
        newHeroPosition = adjust dir heroPosition

tryPickup :: Level -> (Width, Height) -> Int -> Int
tryPickup lvl p = if preview (ix p) lvl == Just Gold then (+1) else id

emptyLevel :: Width -> Height -> Level
emptyLevel w h = Level . genericTake w . fmap (genericTake h) $ repeat . repeat $ Empty

generateLevel :: (Applicative m, MonadRandom m) => Width -> Height -> m Level
generateLevel w h = do
  let emptyLvl = emptyLevel w h
  elements <- randomPositions w h
  return $ foldl' setGold emptyLvl (pickDistinct 100 elements)

initialGameState :: (Applicative m, MonadRandom m) => m GameState
initialGameState = do
  lvl <- generateLevel 20 20
  hPos <- fromMaybe (error "Could not place Hero.") <$> findHeroPos lvl
  return $ GameState (lvl & ix hPos .~ Hero) 100 0 hPos

randomPositions :: (MonadRandom f, Applicative f) => Width -> Height -> f [(Width, Height)]
randomPositions w h = zip <$> getRandomRs (0,w-1) <*> getRandomRs (0,h-1)

setGold :: Level -> (Width,Height) -> Level
setGold l wh = l & ix wh .~ Gold

pickDistinct :: Ord a => Int -> [a] -> [a]
pickDistinct num = go num Set.empty
  where go :: Ord a => Int -> Set a -> [a] -> [a]
        go 0 s _ = Set.toList s
        go _ s [] = Set.toList s
        go n seen (x:xs) = if Set.member x seen
                              then go n seen xs
                              else go (n-1) (Set.insert x seen) xs

findHeroPos :: (Applicative m, MonadRandom m) => Level -> m (Maybe (Width,Height))
findHeroPos l = do
  positions <- uncurry randomPositions $ dimensions l
  return $ find (\p -> preview (ix p) l == Just Empty) positions
