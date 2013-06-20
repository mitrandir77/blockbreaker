import Keyboard

player = { angle=0, score=0, x=300, y=0, balls=[{x=0, y=0, vx=1, vy=0}, {x=3, y=3, vx=0, vy=1}] }

clearGrey = rgb 100 100 200
playerColor  = rgb 100 200 100

playerPos angle  = (300 * cos angle, 300 * sin angle)

movePlayer {x} p =
    let newAngle = p.angle + x/20
        (newX, newY) = playerPos newAngle
    in { p | angle <- newAngle, x <- newX, y<-newY }

near k c n = n >= k-c && n <= k+c

within ball player = (ball.x |> near player.x 8)
                  && (ball.y |> near player.y 20)

detectCollision p ball =
    let revertedBall b = {b | vx <- 0-b.vx, vy<-0-b.vy }
    in if within ball p then revertedBall ball else ball

detectCollisions p =
    let d b = detectCollision p b
    in {p | balls <- map d p.balls }

moveBall b t = { b | x <- b.x + b.vx*t , y <- b.y+ b.vy*t }

moveBalls t p =
    let m b = moveBall b t
    in {p | balls <- map m p.balls }

step (t, dir) = movePlayer dir . moveBalls t . detectCollisions

render p =
    let drawBall ball = (circle 10 |> filled white |> move (ball.x, ball.y))
        balls = map drawBall p.balls
        board = [filled clearGrey (circle 300)]
        pad = [rect 14 45 |> filled playerColor |> move (p.x, p.y) |> rotate p.angle]
    in collage 700 700 (board ++ pad ++ balls)


input = let delta = lift (\t -> t/20) (fps 50)
        in sampleOn delta (lift2 (,) delta Keyboard.arrows)

main  = lift render (foldp step player input)

