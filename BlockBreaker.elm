import Keyboard
import Window

tupleMin (w,h) = min w h

context = {
    size=0,
    w=0,
    h=0,
    players=[{color=(rgb 200 0 0), angle=0, score=0, x=300, y=0},
             {color=(rgb 0 200 0), angle=180, score=0, x=0-300, y=0}],
    balls=[{x=0, y=0, vx=1, vy=0},
           {x=3, y=3, vx=0, vy=1}]
    }

nth l n = last (take n l)

clearGrey = rgb 100 100 200

movePlayer (player, {x}) radius = 
    let newAngle = player.angle + x/20
        playerPos angle = (cos angle, sin angle)
        (newX, newY) = playerPos newAngle
    in { player | angle <- newAngle, x <- newX * radius, y <- newY * radius }

movePlayers controls context =
    let m x = movePlayer x context.radius
    in if length context.players <= length controls then
        { context | players <- map m (zip context.players controls) }
    else
        error ">2 players are not supported"

near k c n = n >= k-c && n <= k+c

within ball player = (ball.x |> near player.x 8)
                  && (ball.y |> near player.y 20)

detectCollision p ball =
    let revertedBall b = {b | vx <- 0-b.vx, vy<-0-b.vy }
    in if within ball p then revertedBall ball else ball

detectPlayerCollisions player context =
    let d balls = detectCollision player balls
    in {context | balls <- map d context.balls }

detectCollisions context =
    foldl detectPlayerCollisions context context.players

moveBall b t = { b | x <- b.x + b.vx*t , y <- b.y+ b.vy*t }

moveBalls t context =
    let m ball = moveBall ball t
    in {context | balls <- map m context.balls }

updateDimensions (w, h) c =
    { c | radius <- ((min w h) - 40) / 2,
          size <- (min w h) - 40,
          w <- w,
          h <- h }

step (delta, directions, dimensions) = updateDimensions dimensions . movePlayers directions . moveBalls delta . detectCollisions

render c =
    let drawBall ball = (circle 10 |> filled white |> move (ball.x, ball.y))
        drawPad player = (rect 14 45 |> filled player.color |> move (player.x, player.y) |> rotate player.angle)
        pads = map drawPad c.players
        balls = map drawBall c.balls
        board = [filled clearGrey (circle c.radius)]
    in collage (c.size + 20) (c.size + 20) (board ++ pads ++ balls)


input = let delta = lift (\t -> t/20) (fps 50)
            toSignal delta keyboard_wasd keyboard_arrows window_dimensions = (delta, [keyboard_wasd, keyboard_arrows], window_dimensions)
        in sampleOn delta (lift4 toSignal delta Keyboard.wasd Keyboard.arrows Window.dimensions)

main  = lift render (foldp step context input)
