{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.ByteString.Char8 (unpack)
import qualified Data.ByteString.Lazy.UTF8 ()
import Data.Maybe
import Data.Text ()
import Data.Text.Encoding ()
import qualified Data.Text.Lazy.Encoding as TLE
import Lucid
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp (run)
import TicTacToe

-- Some html view logic helpers

stateToHtml :: GameState -> String
stateToHtml s = case gameStatus s of
  (Oturn) -> "O turn"
  (Xturn) -> "X turn"
  (Xwin) -> "X Win!"
  (Owin) -> "O Win!"
  (Draw) -> "Draw!!! try more!"

stateWithTurns :: GameState -> [String]
stateWithTurns s = t <$> zip [0 .. 8] s
  where
    t (_, X) = " X "
    t (_, O) = " O "
    t (i, E) = "<a href='?s=" ++ (show $ fromMaybe s $ nextState s i) ++ "'> E " ++ "</a>"

dohtml s title = do
  renderText $
    do
      doctype_
      html_ $ do
        head_ $ ""
        body_ $ do
          p_ $ toHtmlRaw $ "Tic Tac Toe game <a href='?s=" ++ show initialState ++ "'>restart</a>"
          h1_ $ toHtmlRaw title
      table_ $ do
        tr_ $ do
          td_ $ toHtmlRaw $ s !! 0
          td_ $ toHtmlRaw $ s !! 1
          td_ $ toHtmlRaw $ s !! 2
        tr_ $ do
          td_ $ toHtmlRaw $ s !! 3
          td_ $ toHtmlRaw $ s !! 4
          td_ $ toHtmlRaw $ s !! 5
        tr_ $ do
          td_ $ toHtmlRaw $ s !! 6
          td_ $ toHtmlRaw $ s !! 7
          td_ $ toHtmlRaw $ s !! 8

-- web server app

app :: Application
app req respond = do
  -- print "requested"
  let params = queryString req
      state = case lookup "s" params of
        Just (Just val) -> (read $ unpack val) :: GameState
        Just (Nothing) -> initialState
        Nothing -> initialState
      title = stateToHtml state
  print $ show state
  respond $
    responseLBS
      status200
      [("Content-Type", "text/html")]
      (TLE.encodeUtf8 $ uncurry (dohtml . stateWithTurns) $ (state, title))

main :: IO ()
main = do
  putStrLn "Starting tictactoe server on http://localhost:8080"
  run 8080 app
