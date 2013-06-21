import Keyboard
import Window

settings = {
    margin=40,
    ballSize=10,
    padWidth=14,
    padHeight=85,
    scoreColor=rgb 0 200 0,
    scoreSize=1,
    padSpeed=0.1,
    ballSpeed=2,
    fps=50
    }

context = {
    diameter=0,
    w=0,
    h=0,
    players=[{color=(rgb 200 0 0), angle=0, score=0, x=300, y=0, name="Kazet"},
             {color=(rgb 0 200 0), angle=180, score=0, x=0-300, y=0, name="Kwaps"}],
    balls=[{x=0, y=0, vx=1, vy=0},
           {x=3, y=3, vx=0, vy=1}]
    }

nth lst position = last (take position lst)

clearGrey = rgb 100 100 200

movePlayer (player, {x}) radius = 
    let newAngle = player.angle + x * settings.padSpeed
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

moveBall ball time = { ball | x <- ball.x + ball.vx * settings.ballSpeed * time,
                              y <- ball.y + ball.vy * settings.ballSpeed * time }

moveBalls time context =
    let mover ball = moveBall ball time
    in {context | balls <- map mover context.balls }

updateDimensions (width, height) context =
    { context | radius <- ((min width height) - 2 * settings.margin) / 2,
                diameter <- (min width height) - 2 * settings.margin,
                w <- width,
                h <- height }

step (delta, directions, dimensions) =
    updateDimensions dimensions .
    movePlayers directions .
    moveBalls delta .
    detectCollisions

txt f = text . f . monospace . Text.color settings.scoreColor . toText

render context =
    let drawBall ball = (circle settings.ballSize |> filled white |> move (ball.x, ball.y))
        drawPad player = (rect settings.padWidth settings.padHeight |> filled player.color |> move (player.x, player.y) |> rotate player.angle)
        makeScores player = " " ++ player.name ++ " " ++ show player.score
        scoreText = txt (Text.height settings.scoreSize) (foldr (++) "" (map makeScores context.players))
        scores = [toForm scoreText |> move (0, context.radius + settings.margin / 2)]
        pads = map drawPad context.players
        balls = map drawBall context.balls
        board = [filled clearGrey (circle context.radius)]
    in collage (context.diameter + 2 * settings.margin) (context.diameter + 2 * settings.margin) (board ++ pads ++ balls ++ scores)


input = let delta = lift (\t -> t/20) (fps settings.fps)
            toSignal delta keyboard_wasd keyboard_arrows window_dimensions = (delta, [keyboard_wasd, keyboard_arrows], window_dimensions)
        in sampleOn delta (lift4 toSignal delta Keyboard.wasd Keyboard.arrows Window.dimensions)

main  = lift render (foldp step context input)
