import Keyboard


player = { angle=0, score=0 }

clearGrey = rgb 100 100 200
playerColor  = rgb 100 200 100

playerPos angle  = (300 * cos angle, 300 * sin angle)

movePlayer {x} p= { p | angle <- p.angle + x/20 }

step (t, dir) = movePlayer dir


render p = collage 700 700
       [ filled clearGrey (circle 300), rect 10 40 |> filled playerColor |> move (playerPos p.angle) |> rotate p.angle ]


input = let delta = lift (\t -> t/20) (fps 50)
        in sampleOn delta (lift2 (,) delta Keyboard.arrows)

main  = lift render (foldp step player input)

