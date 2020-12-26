module Day12 where

import Data.Bifunctor (bimap, first, second)
import Data.IntMap (fromList, (!))
import Data.Text (lines, unpack)
import Data.Text.IO (readFile)

north = second . flip (-)

east = first . (+)

south = second . (+)

west = first . flip (-)

directions = fromList [(0, north), (90, east), (180, south), (270, west)]

left a b = mod (360 + a - b) 360

right a b = mod (a + b) 360

waypointLeft (x, y) = (y, - x)

waypointRight (x, y) = (- y, x)

both f = bimap f f

sumTuple (a1, a2) (b1, b2) = (a1 + b1, a2 + b2)

turns deg = div deg 90

action ('N' : value) state = first (north $ read value) state
action ('S' : value) state = first (south $ read value) state
action ('E' : value) state = first (east $ read value) state
action ('W' : value) state = first (west $ read value) state
action ('L' : value) state = second (flip left $ read value) state
action ('R' : value) state = second (right $ read value) state
action ('F' : value) (coord, dir) = first ((directions ! dir) (read value)) (coord, dir)

waypointAction ('N' : value) state = second (north $ read value) state
waypointAction ('S' : value) state = second (south $ read value) state
waypointAction ('E' : value) state = second (east $ read value) state
waypointAction ('W' : value) state = second (west $ read value) state
waypointAction ('L' : value) (coord, waypoint) = (coord, iterate waypointLeft waypoint !! turns (read value))
waypointAction ('R' : value) (coord, waypoint) = (coord, iterate waypointRight waypoint !! turns (read value))
waypointAction ('F' : value) (coord, waypoint) = (sumTuple coord $ both (* read value) waypoint, waypoint)

run f seed = uncurry (+) . bimap abs abs . fst . foldl (flip f) seed

main = do
  contents <- fmap Data.Text.lines (Data.Text.IO.readFile "12.txt")

  print . run action ((0, 0), 90) $ map unpack contents
  print . run waypointAction ((0, 0), (10, -1)) $ map unpack contents