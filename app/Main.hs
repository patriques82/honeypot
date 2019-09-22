module Main where

import           Honeypot

main :: IO ()
main = runGame player level

level :: Config
level = config { dim = (10, 10)
               , blocks = blockPositions
               , dir = East
               , pos = (3,3)
               , fuel = 100
               , enemies = [ enemy1, enemy2 ]
               , gfxCtx = context { tankBmp = "data/tank.bmp"
                                  , enemyBmp = "data/monster.bmp"
                                  , size = (1100, 1100)
                                  }
               }


enemy1, enemy2 :: Path
enemy1 = do go 1 2
            go 2 2
            go 2 4
            end
enemy2 = do go 10 2
            end

blockPositions :: [(Int, Int)]
blockPositions = [ (1,1), (1,7), (1,10)
                 , (2,5), (2,9), (3,1)
                 , (3,2), (4,5), (10,3)
                 ]

player :: Step Event
player = return Noop
