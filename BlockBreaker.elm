import Keyboard
import Window

context = {
    size=0,
    w=0,
    h=0,
    players=[{color=(rgb 200 0 0), angle=0, score=0, x=300, y=0},
             {color=(rgb 0 200 0), angle=180, score=0, x=0-300, y=0}],
    balls=[{x=0, y=0, vx=1, vy=0},
           {x=3, y=3, vx=0, vy=1}]
    }

nth lst position = last (take position lst)

clearGrey = rgb 100 100 200

movePlayer (player, {x}) radius = 
    let newAngle = player.angle + x/20
        playerPos angle = (cos angle, sin angle)
        (newX, newY) = playerPos newAngle
    in { player | angle <- newAngle, x <- newX * radius, y <- newY * radius }

movePlayers controls context =
    let mover playerControlTuple = movePlayer playerControlTuple context.radius
    in if length context.players <= length controls then
        { context | players <- map mover (zip context.players controls) }
    else
        error ">2 players are not supported"

near a b distance = distance >= a - b && distance <= a + b

within ball player = (ball.x |> near player.x 8)
                  && (ball.y |> near player.y 20)

detectCollision p ball =
    let revertedBall b = {b | vx <- 0 - b.vx,
                              vy <- 0 - b.vy }
    in if within ball p then revertedBall ball else ball

detectPlayerCollisions player context =
    let detector balls = detectCollision player balls
    in {context | balls <- map detector context.balls }

detectCollisions context =
    foldl detectPlayerCollisions context context.players

moveBall ball time = { ball | x <- ball.x + ball.vx * time,
                              y <- ball.y + ball.vy * time }

moveBalls time context =
    let mover ball = moveBall ball time
    in {context | balls <- map mover context.balls }

updateDimensions (width, height) context =
    { context | radius <- ((min width height) - 40) / 2,
                size <- (min width height) - 40,
                w <- width,
                h <- height }

step (delta, directions, dimensions) =
    updateDimensions dimensions .
    movePlayers directions .
    moveBalls delta .
    detectCollisions

render context =
    let drawBall ball = (circle 10 |> filled white |> move (ball.x, ball.y))
        drawPad player = (rect 14 45 |> filled player.color |> move (player.x, player.y) |> rotate player.angle)
        pads = map drawPad context.players
        balls = map drawBall context.balls
        board = [filled clearGrey (circle context.radius)]
    in collage (context.size + 20) (context.size + 20) (board ++ pads ++ balls)


input = let delta = lift (\t -> t/20) (fps 50)
            toSignal delta keyboard_wasd keyboard_arrows window_dimensions = (delta, [keyboard_wasd, keyboard_arrows], window_dimensions)
        in sampleOn delta (lift4 toSignal delta Keyboard.wasd Keyboard.arrows Window.dimensions)

main  = lift render (foldp step context input)
