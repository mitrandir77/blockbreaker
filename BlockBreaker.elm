import Keyboard


player = { angle=0, score=0, x=300, y=0, ball={x=0, y=0, vx=2, vy=2} }

clearGrey = rgb 100 100 200
playerColor  = rgb 100 200 100

playerPos angle  = (300 * cos angle, 300 * sin angle)

movePlayer {x} p=
    let newAngle = p.angle + x/20
        (newX, newY) = playerPos newAngle
    in { p | angle <- newAngle, x <- newX, y<-newY }

moveBall t p =
    let moveBall b t = { b | x <- b.x + b.vx*t , y <- b.y+ b.vy*t }
    in { p | ball <- moveBall p.ball t }

near k c n = n >= k-c && n <= k+c
within ball player = (ball.x |> near player.x 8)
                  && (ball.y |> near player.y 20)


detectCollision p =
    let revertedBall b = {b | vx <- 0-b.vx, vy<-0-b.vy }
    in   if within p.ball p then {p | ball <- revertedBall p.ball } else p

step (t, dir) = movePlayer dir . moveBall t . detectCollision


render p = collage 700 700
       [ filled clearGrey (circle 300),
         rect 14 45 |> filled playerColor |> move (p.x, p.y) |> rotate p.angle,
         circle 10 |> filled white |> move (p.ball.x, p.ball.y)]


input = let delta = lift (\t -> t/20) (fps 50)
        in sampleOn delta (lift2 (,) delta Keyboard.arrows)

main  = lift render (foldp step player input)

