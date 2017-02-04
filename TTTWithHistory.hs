-- ====================================================================
--                    Tic tac toe with history
-- ====================================================================
-- The following are the history operations when applied to TicTacToe.
-- For another type, similar operations are required.

import History 
import TicTacToe

-- Start a new TTT with an empty Board.
tttNewGame :: History TTT
tttNewGame = History [newGame] []

-- Play "next" on the current TTT state.
tttNext :: History TTT -> History TTT
tttNext = makeMove next 

-- Similar to next except with an explicit move. 
tttPlay :: Int -> Int -> (History TTT -> History TTT)
-- Can you explain these types:
--        play r c :: TTT -> TTT
--        makeMove (play r c) :: History TTT -> History TTT
tttPlay = \r c -> makeMove (play r c)  

-- Start a TTT. Always start at (3, 1).
tttStart :: History TTT
tttStart = tttStartAt 3 1 

-- Start a TTT at r c.
tttStartAt :: Int -> Int -> History TTT
tttStartAt = \r c -> tttPlay r c tttNewGame
