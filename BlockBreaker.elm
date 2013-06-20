import Keyboard
import Window

tupleMin (w,h) = min w h

context = {
    size=0,
    w=0,
    h=0,
    angle=0,
    score=0,
    x=300,
    y=0,
    balls=[{x=0, y=0, vx=1, vy=0},
           {x=3, y=3, vx=0, vy=1}] }

clearGrey = rgb 100 100 200
playerColor  = rgb 100 200 100

playerPos angle = (cos angle, sin angle)

movePlayer {x} p =
    let newAngle = p.angle + x/20
        (newX, newY) = playerPos newAngle
    in { p | angle <- newAngle, x <- newX * p.radius, y <- newY * p.radius }

near k c n = n >= k-c && n <= k+c

within ball context = (ball.x |> near context.x 8)
                  && (ball.y |> near context.y 20)

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

updateDimensions (w, h) p =
    { p | radius <- ((min w h) - 40) / 2,
          size <- (min w h) - 40,
          w <- w,
          h <- h }

step (t, dir, dimensions) = updateDimensions dimensions . movePlayer dir . moveBalls t . detectCollisions

render p =
    let drawBall ball = (circle 10 |> filled white |> move (ball.x, ball.y))
        balls = map drawBall p.balls
        board = [filled clearGrey (circle p.radius)]
        pad = [rect 14 45 |> filled playerColor |> move (p.x, p.y) |> rotate p.angle]
    in collage (p.size + 20) (p.size + 20) (board ++ pad ++ balls)


input = let delta = lift (\t -> t/20) (fps 50)
            toSignal d a wh = (d, a, wh)
        in sampleOn delta (lift3 toSignal delta Keyboard.arrows Window.dimensions)

main  = lift render (foldp step context input)
